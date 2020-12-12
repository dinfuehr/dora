use std::mem;
use std::ptr;

use crate::bytecode::{self, BytecodeFunction, ConstPoolEntry, ConstPoolOpcode};
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, JitDescriptor};
use crate::gc::Address;
use crate::handle::{root, Handle};
use crate::object::{
    self, byte_array_from_buffer, int_array_alloc_heap, Int32Array, Obj, Ref, UInt8Array,
};
use crate::sym::NestedSymTable;
use crate::threads::THREAD;
use crate::ty::SourceTypeArray;
use crate::vm::{AnalysisData, Fct, VM};

pub fn compile(vm: &VM, fct: &Fct, src: &AnalysisData, _type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = bytecode::generate(vm, fct, src);

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), &bytecode_fct);
    }

    let compile_name = vm.interner.intern("compile");
    let compile_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(compile_name)
        .expect("compile()-method missing");
    let compile_fct = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = root(allocate_compilation_info(vm, &bytecode_fct));

    let tld_address = THREAD.with(|thread| {
        let thread = thread.borrow();
        let ptr = &thread.tld;

        Address::from_ptr(ptr as *const _)
    });

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Ref<Obj>) -> Ref<UInt8Array> =
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
    let fct_name = vm.interner.intern(name);
    let bc_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(fct_name)
        .expect("compile()-method missing");

    let fct = vm.fcts.idx(bc_fct_id);
    let fct = fct.read();
    let analysis = fct.analysis();

    let bytecode_fct = bytecode::generate(vm, &*fct, &*analysis);

    if should_emit_bytecode(vm, &*fct) {
        bytecode::dump(vm, Some(&*fct), &bytecode_fct);
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

fn allocate_registers_array(vm: &VM, fct: &BytecodeFunction) -> Ref<Int32Array> {
    let mut array = int_array_alloc_heap(vm, fct.registers().len());

    for (idx, ty) in fct.registers().iter().enumerate() {
        array.set_at(idx, ty.kind() as u32 as i32);
    }

    array
}

fn allocate_constpool_array(vm: &VM, fct: &BytecodeFunction) -> Ref<UInt8Array> {
    use byteorder::{LittleEndian, WriteBytesExt};
    let mut buffer = Vec::new();

    for const_entry in fct.const_pool_entries() {
        match const_entry {
            ConstPoolEntry::String(ref value) => {
                buffer.push(ConstPoolOpcode::Float32 as u8);
                buffer
                    .write_u32::<LittleEndian>(value.len() as u32)
                    .unwrap();

                for byte in value.bytes() {
                    buffer.push(byte);
                }
            }
            &ConstPoolEntry::Float32(value) => {
                buffer.push(ConstPoolOpcode::Float32 as u8);
                buffer.write_u32::<LittleEndian>(value.to_bits()).unwrap();
            }
            &ConstPoolEntry::Float64(value) => {
                buffer.push(ConstPoolOpcode::Float64 as u8);
                buffer.write_u64::<LittleEndian>(value.to_bits()).unwrap();
            }
            &ConstPoolEntry::Int32(value) => {
                buffer.push(ConstPoolOpcode::Int32 as u8);
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
            }
            &ConstPoolEntry::Int64(value) => {
                buffer.push(ConstPoolOpcode::Int64 as u8);
                buffer.write_u64::<LittleEndian>(value as u64).unwrap();
            }
            &ConstPoolEntry::Char(value) => {
                buffer.push(ConstPoolOpcode::Char as u8);
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
            }
            &ConstPoolEntry::Fct(_, _) => unimplemented!(),
            &ConstPoolEntry::Generic(_, _, _) => unimplemented!(),
            &ConstPoolEntry::Class(_, _) => unimplemented!(),
            &ConstPoolEntry::Field(_, _, _) => unimplemented!(),
            &ConstPoolEntry::FieldFixed(_, _) => unimplemented!(),
            &ConstPoolEntry::Enum(_, _) => unimplemented!(),
            &ConstPoolEntry::EnumVariant(_, _, _) => unimplemented!(),
            &ConstPoolEntry::Struct(_, _) => unimplemented!(),
            &ConstPoolEntry::StructField(_, _, _) => unimplemented!(),
            &ConstPoolEntry::Trait(_, _, _) => unimplemented!(),
        }
    }

    byte_array_from_buffer(vm, &buffer)
}

fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_array: Handle<UInt8Array>,
    constpool_array: Handle<UInt8Array>,
    registers_array: Handle<Int32Array>,
    arguments: i32,
) -> Ref<Obj> {
    let cls_id = vm.cls_def_by_name(vm.boots_namespace_id, "EncodedCompilationInfo");
    let obj = object::alloc(vm, cls_id);

    let fid = vm.field_in_class(cls_id, "code");
    object::write_ref(vm, obj, cls_id, fid, bytecode_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "constpool");
    object::write_ref(vm, obj, cls_id, fid, constpool_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "registers");
    object::write_ref(vm, obj, cls_id, fid, registers_array.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "arguments");
    object::write_int32(vm, obj, cls_id, fid, arguments);

    obj
}
