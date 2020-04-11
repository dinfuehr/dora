use std::mem;
use std::ptr;

use crate::bytecode::{self, BytecodeFunction, ConstPoolEntry, ConstPoolOpcode};
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, JitDescriptor};
use crate::gc::Address;
use crate::handle::{root, Handle};
use crate::object::{
    self, byte_array_from_buffer, int_array_alloc_heap, ByteArray, IntArray, Obj, Ref,
};
use crate::threads::THREAD;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> Code {
    let bytecode_fct = bytecode::generate(vm, fct, src, cls_type_params, fct_type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(&bytecode_fct);
    }

    let compile_fct_id = vm.fct_by_name("compile").expect("compile()-method missing");
    let compile_fct = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = root(allocate_compilation_info(vm, &bytecode_fct));

    let tld_address = THREAD.with(|thread| {
        let thread = thread.borrow();
        let ptr = &thread.tld;

        Address::from_ptr(ptr as *const _)
    });

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Ref<Obj>) -> Ref<ByteArray> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = root(compile_fct_ptr(
        tld_address,
        compile_fct,
        encoded_compilation_info.direct(),
    ));
    let mut machine_code_array = vec![0; machine_code.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            machine_code.data() as *mut u8,
            machine_code_array.as_mut_ptr(),
            machine_code.len(),
        );
    }

    Code::from_optimized_buffer(vm, &machine_code_array, JitDescriptor::DoraFct(fct.id))
}

pub fn bytecode(vm: &VM, name: &str) -> Ref<Obj> {
    let bc_fct_id = vm.fct_by_name(name).expect("compile()-method missing");

    let fct = vm.fcts.idx(bc_fct_id);
    let fct = fct.read();
    let src = fct.src();
    let src = src.read();

    let bytecode_fct = bytecode::generate(vm, &*fct, &*src, &TypeList::empty(), &TypeList::empty());

    if should_emit_bytecode(vm, &*fct) {
        bytecode::dump(&bytecode_fct);
    }

    allocate_compilation_info(vm, &bytecode_fct)
}

fn allocate_compilation_info(vm: &VM, bytecode_fct: &BytecodeFunction) -> Ref<Obj> {
    let bytecode_array = root(byte_array_from_buffer(vm, bytecode_fct.code()));
    let constpool_array = root(allocate_constpool_array(vm, &bytecode_fct));
    let registers_array = root(allocate_registers_array(vm, &bytecode_fct));

    allocate_encoded_compilation_info(
        vm,
        bytecode_array,
        constpool_array,
        registers_array,
        bytecode_fct.arguments() as i32,
    )
}

fn allocate_registers_array(vm: &VM, fct: &BytecodeFunction) -> Ref<IntArray> {
    let mut array = int_array_alloc_heap(vm, fct.registers().len());

    for (idx, &ty) in fct.registers().iter().enumerate() {
        array.set_at(idx, ty as u32 as i32);
    }

    array
}

fn allocate_constpool_array(vm: &VM, fct: &BytecodeFunction) -> Ref<ByteArray> {
    use byteorder::{LittleEndian, WriteBytesExt};
    let mut buffer = Vec::new();

    for const_entry in fct.const_pool_entries() {
        match const_entry {
            ConstPoolEntry::String(ref value) => {
                buffer.push(ConstPoolOpcode::Float as u8);
                buffer
                    .write_u32::<LittleEndian>(value.len() as u32)
                    .unwrap();

                for byte in value.bytes() {
                    buffer.push(byte);
                }
            }
            &ConstPoolEntry::Float(value) => {
                buffer.push(ConstPoolOpcode::Float as u8);
                buffer.write_u32::<LittleEndian>(value.to_bits()).unwrap();
            }
            &ConstPoolEntry::Double(value) => {
                buffer.push(ConstPoolOpcode::Double as u8);
                buffer.write_u64::<LittleEndian>(value.to_bits()).unwrap();
            }
            &ConstPoolEntry::Int(value) => {
                buffer.push(ConstPoolOpcode::Int as u8);
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
            }
            &ConstPoolEntry::Long(value) => {
                buffer.push(ConstPoolOpcode::Long as u8);
                buffer.write_u64::<LittleEndian>(value as u64).unwrap();
            }
            &ConstPoolEntry::Char(value) => {
                buffer.push(ConstPoolOpcode::Char as u8);
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
            }
        }
    }

    byte_array_from_buffer(vm, &buffer)
}

fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_array: Handle<ByteArray>,
    constpool_array: Handle<ByteArray>,
    registers_array: Handle<IntArray>,
    arguments: i32,
) -> Ref<Obj> {
    let cls_id = vm.cls_def_by_name("EncodedCompilationInfo");
    let obj = object::alloc(vm, cls_id);

    let fid = vm.field_in_class(cls_id, "code");
    object::write_ref(vm, obj, cls_id, fid, bytecode_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "constpool");
    object::write_ref(vm, obj, cls_id, fid, constpool_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "registers");
    object::write_ref(vm, obj, cls_id, fid, registers_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "arguments");
    object::write_int(vm, obj, cls_id, fid, arguments);

    obj
}
