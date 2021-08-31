use std::mem;
use std::ptr;

use crate::bytecode::InstructionSet;
use crate::bytecode::{self, BytecodeFunction, ConstPoolEntry, ConstPoolOpcode, SourceTypeOpcode};
use crate::bytecode::{BytecodeType, BytecodeTypeKind};
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
use crate::vm::{Fct, VM};

pub fn compile(vm: &VM, fct: &Fct, type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), &bytecode_fct);
    }

    let compile_name = vm.interner.intern("compile");
    let compile_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(compile_name)
        .expect("compile()-method missing");
    let compile_address = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info = handle(allocate_compilation_info(vm, bytecode_fct, type_params));

    let tld_address = current_thread().tld_address();

    let dora_stub_address = vm.dora_stub();
    let compile_fct_ptr: extern "C" fn(Address, Address, Address) -> Ref<UInt8Array> =
        unsafe { mem::transmute(dora_stub_address) };

    let machine_code = handle(compile_fct_ptr(
        tld_address,
        compile_address,
        encoded_compilation_info.direct_ptr(),
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

pub fn encode_test(vm: &VM, fct: &Fct, type_params: &SourceTypeArray) {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");
    let _encoded_compilation_info =
        handle(allocate_compilation_info(vm, bytecode_fct, type_params));

    // println!("before getting decode in rust");

    // let decode_name = vm.interner.intern("decode");
    // let decode_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
    //     .get_fct(decode_name)
    //     .expect("decode()-method missing");
    // let decode_address = vm.ensure_compiled(decode_fct_id);

    // let tld_address = current_thread().tld_address();
    // let dora_stub_address = vm.dora_stub();

    // let decode_fct_ptr: extern "C" fn(Address, Address, Address) =
    //     unsafe { mem::transmute(dora_stub_address) };

    // vm.code_map.lock().dump(vm);

    // decode_fct_ptr(
    //     tld_address,
    //     decode_address,
    //     encoded_compilation_info.direct_ptr(),
    // );

    // println!("after decode in rust");
}

pub fn bytecode(vm: &VM, name: &str) -> Ref<Obj> {
    let fct_name = vm.interner.intern(name);
    let bc_fct_id = NestedSymTable::new(vm, vm.boots_namespace_id)
        .get_fct(fct_name)
        .expect("compile()-method missing");

    let fct = vm.fcts.idx(bc_fct_id);
    let fct = fct.read();

    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, &*fct) {
        bytecode::dump(vm, Some(&*fct), bytecode_fct);
    }

    allocate_compilation_info(vm, bytecode_fct, &SourceTypeArray::empty())
}

fn allocate_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &SourceTypeArray,
) -> Ref<Obj> {
    let bytecode_array = handle(byte_array_from_buffer(vm, bytecode_fct.code()));
    let constpool_array = handle(allocate_constpool_array(vm, &bytecode_fct));
    let registers_array = handle(allocate_registers_array(vm, &bytecode_fct));
    let registers_array2 = handle(allocate_registers_array2(vm, &bytecode_fct));
    let type_params = handle(allocate_type_params(vm, type_params));

    allocate_encoded_compilation_info(
        vm,
        bytecode_array,
        constpool_array,
        registers_array,
        registers_array2,
        type_params,
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

fn allocate_registers_array2(vm: &VM, fct: &BytecodeFunction) -> Ref<UInt8Array> {
    use byteorder::{LittleEndian, WriteBytesExt};

    let mut buffer = Vec::new();

    buffer
        .write_u32::<LittleEndian>(fct.registers().len() as u32)
        .unwrap();

    for ty in fct.registers().iter() {
        encode_bytecode_type(vm, ty, &mut buffer);
    }

    byte_array_from_buffer(vm, &buffer)
}

fn allocate_type_params(vm: &VM, type_params: &SourceTypeArray) -> Ref<UInt8Array> {
    let mut buffer = Vec::new();
    encode_source_type_array(vm, type_params, &mut buffer);
    byte_array_from_buffer(vm, &buffer)
}

fn allocate_constpool_array(vm: &VM, fct: &BytecodeFunction) -> Ref<UInt8Array> {
    use byteorder::{LittleEndian, WriteBytesExt};
    let mut buffer = Vec::new();

    buffer
        .write_u32::<LittleEndian>(fct.const_pool_entries().len() as u32)
        .unwrap();

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
            &ConstPoolEntry::Generic(tp_id, fct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Generic as u8);
                buffer
                    .write_u32::<LittleEndian>(tp_id.to_usize() as u32)
                    .unwrap();
                buffer
                    .write_u32::<LittleEndian>(fct_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Class as u8);
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::Field(cls_id, ref source_type_array, field_id) => {
                buffer.push(ConstPoolOpcode::Field as u8);
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
            }
            &ConstPoolEntry::FieldFixed(cls_def_id, field_id) => {
                buffer.push(ConstPoolOpcode::FieldFixed as u8);
                buffer
                    .write_u32::<LittleEndian>(cls_def_id.to_usize() as u32)
                    .unwrap();
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
            }
            &ConstPoolEntry::Enum(enum_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Enum as u8);
                buffer
                    .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::EnumVariant(enum_id, ref source_type_array, variant_id) => {
                buffer.push(ConstPoolOpcode::EnumVariant as u8);
                buffer
                    .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
                buffer.write_u32::<LittleEndian>(variant_id as u32).unwrap();
            }
            &ConstPoolEntry::Struct(struct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Struct as u8);
                buffer
                    .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
            }
            &ConstPoolEntry::StructField(struct_id, ref source_type_array, field_id) => {
                buffer.push(ConstPoolOpcode::StructField as u8);
                buffer
                    .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
            }
            &ConstPoolEntry::Trait(trait_id, ref source_type_array, ref source_type) => {
                buffer.push(ConstPoolOpcode::Trait as u8);
                buffer
                    .write_u32::<LittleEndian>(trait_id.to_usize() as u32)
                    .unwrap();
                encode_source_type_array(vm, source_type_array, &mut buffer);
                encode_source_type(vm, source_type.clone(), &mut buffer);
            }
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

fn encode_source_type(vm: &VM, ty: SourceType, buffer: &mut Vec<u8>) {
    use byteorder::{LittleEndian, WriteBytesExt};

    match ty {
        SourceType::Error | SourceType::Any | SourceType::Ptr | SourceType::This => unreachable!(),
        SourceType::Unit => buffer.push(SourceTypeOpcode::Unit as u8),
        SourceType::Bool => buffer.push(SourceTypeOpcode::Bool as u8),
        SourceType::Char => buffer.push(SourceTypeOpcode::Char as u8),
        SourceType::UInt8 => buffer.push(SourceTypeOpcode::UInt8 as u8),
        SourceType::Int32 => buffer.push(SourceTypeOpcode::Int32 as u8),
        SourceType::Int64 => buffer.push(SourceTypeOpcode::Int64 as u8),
        SourceType::Float32 => buffer.push(SourceTypeOpcode::Float32 as u8),
        SourceType::Float64 => buffer.push(SourceTypeOpcode::Float64 as u8),
        SourceType::Class(cls_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Class as u8);
            buffer
                .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Struct(struct_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Struct as u8);
            buffer
                .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Trait(trait_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Struct as u8);
            buffer
                .write_u32::<LittleEndian>(trait_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Enum(enum_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Enum as u8);
            buffer
                .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Tuple(tuple_id) => {
            buffer.push(SourceTypeOpcode::Tuple as u8);
            let subtypes = vm.tuples.lock().get(tuple_id);
            buffer
                .write_u32::<LittleEndian>(subtypes.len() as u32)
                .unwrap();
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer);
            }
        }
        SourceType::TypeParam(type_param_id) => {
            buffer.push(SourceTypeOpcode::TypeParam as u8);
            buffer
                .write_u32::<LittleEndian>(type_param_id.to_usize() as u32)
                .unwrap();
        }
        SourceType::Module(_) | SourceType::Lambda(_) => unimplemented!(),
    }
}

fn encode_bytecode_type(vm: &VM, ty: &BytecodeType, buffer: &mut Vec<u8>) {
    use byteorder::{LittleEndian, WriteBytesExt};

    match ty {
        BytecodeType::Bool => buffer.push(BytecodeTypeKind::Bool as u8),
        BytecodeType::Char => buffer.push(BytecodeTypeKind::Char as u8),
        BytecodeType::UInt8 => buffer.push(BytecodeTypeKind::UInt8 as u8),
        BytecodeType::Int32 => buffer.push(BytecodeTypeKind::Int32 as u8),
        BytecodeType::Int64 => buffer.push(BytecodeTypeKind::Int64 as u8),
        BytecodeType::Float32 => buffer.push(BytecodeTypeKind::Float32 as u8),
        BytecodeType::Float64 => buffer.push(BytecodeTypeKind::Float64 as u8),
        BytecodeType::Ptr => buffer.push(BytecodeTypeKind::Ptr as u8),
        BytecodeType::Tuple(tuple_id) => {
            buffer.push(SourceTypeOpcode::Tuple as u8);
            let subtypes = vm.tuples.lock().get(*tuple_id);
            buffer
                .write_u32::<LittleEndian>(subtypes.len() as u32)
                .unwrap();
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer);
            }
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.push(SourceTypeOpcode::TypeParam as u8);
            buffer.write_u32::<LittleEndian>(*type_param_id).unwrap();
        }
        BytecodeType::Enum(enum_id, ref source_type_array) => {
            buffer.push(SourceTypeOpcode::Enum as u8);
            buffer
                .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Struct(struct_id, ref source_type_array) => {
            buffer.push(SourceTypeOpcode::Struct as u8);
            buffer
                .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                .unwrap();
            encode_source_type_array(vm, source_type_array, buffer);
        }
    }
}

fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_array: Handle<UInt8Array>,
    constpool_array: Handle<UInt8Array>,
    registers_array: Handle<Int32Array>,
    registers_array2: Handle<UInt8Array>,
    type_params: Handle<UInt8Array>,
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

    let fid = vm.field_in_class(cls_id, "registers2");
    object::write_ref(
        vm,
        obj,
        cls_id,
        fid,
        registers_array2.direct().cast::<Obj>(),
    );

    let fid = vm.field_in_class(cls_id, "typeParams");
    object::write_ref(vm, obj, cls_id, fid, type_params.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "arguments");
    object::write_int32(vm, obj, cls_id, fid, arguments);

    let fid = vm.field_in_class(cls_id, "arch");
    let instruction_set = if cfg!(target_arch = "x86_64") {
        InstructionSet::X64
    } else if cfg!(target_arch = "aarch64") {
        InstructionSet::Arm64
    } else {
        panic!()
    };
    object::write_int32(vm, obj, cls_id, fid, instruction_set as i32);

    obj
}
