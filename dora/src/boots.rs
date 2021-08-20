use std::mem;
use std::ptr;

use crate::bytecode::{self, BytecodeFunction, ConstPoolEntry, ConstPoolOpcode, SourceTypeOpcode};
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::fct::{Code, JitDescriptor};
use crate::gc::Address;
use crate::handle::{handle, Handle};
use crate::object::{
    self, byte_array_from_buffer, int_array_alloc_heap, Int32Array, Obj, Ref, UInt8Array,
};
use crate::sym::NestedSymTable;
use crate::threads::current_thread;
use crate::ty::{SourceType, SourceTypeArray};
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

    let encoded_compilation_info = handle(allocate_compilation_info(vm, &bytecode_fct));

    let tld_address = current_thread().tld_address();

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Ref<Obj>) -> Ref<UInt8Array> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = handle(compile_fct_ptr(
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
    let bytecode_array = handle(byte_array_from_buffer(vm, bytecode_fct.code()));
    let constpool_array = handle(allocate_constpool_array(vm, &bytecode_fct));
    let registers_array = handle(allocate_registers_array(vm, &bytecode_fct));

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
            &ConstPoolEntry::Fct(fct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Fct as u8);
                buffer
                    .write_u32::<LittleEndian>(fct_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::Generic(_, _, _) => unimplemented!(),
            &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Class as u8);
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::Field(cls_id, ref source_type_array, field_id) => {
                buffer.push(ConstPoolOpcode::Class as u8);
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
            }
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

fn encode_source_type_array(vm: &VM, sta: &SourceTypeArray, buffer: &mut Vec<u8>) {
    use byteorder::{LittleEndian, WriteBytesExt};
    buffer.write_u32::<LittleEndian>(sta.len() as u32).unwrap();

    for ty in sta.iter() {
        encode_source_type(vm, ty, buffer);
    }
}

fn encode_source_type(_vm: &VM, ty: SourceType, buffer: &mut Vec<u8>) {
    let opcode = match ty {
        SourceType::Error | SourceType::Any | SourceType::Ptr | SourceType::This => unreachable!(),
        SourceType::Unit => SourceTypeOpcode::Unit,
        SourceType::Bool => SourceTypeOpcode::Bool,
        SourceType::Char => SourceTypeOpcode::Char,
        SourceType::UInt8 => SourceTypeOpcode::UInt8,
        SourceType::Int32 => SourceTypeOpcode::Int32,
        SourceType::Int64 => SourceTypeOpcode::Int64,
        SourceType::Float32 => SourceTypeOpcode::Float32,
        SourceType::Float64 => SourceTypeOpcode::Float64,
        _ => unreachable!(),
    };

    buffer.push(opcode as u8);
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
