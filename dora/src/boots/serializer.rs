use byteorder::{LittleEndian, WriteBytesExt};

use crate::bytecode::{
    BytecodeFunction, ConstPoolEntry, ConstPoolOpcode, InstructionSet, SourceTypeOpcode,
};
use crate::bytecode::{BytecodeType, BytecodeTypeKind};
use crate::handle::{handle, Handle};
use crate::object::{self, byte_array_from_buffer, Obj, Ref, UInt8Array};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::VM;

pub fn allocate_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &SourceTypeArray,
) -> Ref<Obj> {
    let mut buffer = ByteBuffer::new();

    let bytecode_array = handle(encode_bytecode_array(vm, bytecode_fct.code(), &mut buffer));
    let constpool_array = handle(encode_constpool_array(vm, &mut buffer, &bytecode_fct));
    let registers_array = handle(encode_registers_array(vm, &bytecode_fct, &mut buffer));
    let type_params = handle(encode_type_params(vm, type_params, &mut buffer));

    buffer.emit_u32(bytecode_fct.arguments());

    let instruction_set = if cfg!(target_arch = "x86_64") {
        InstructionSet::X64
    } else if cfg!(target_arch = "aarch64") {
        InstructionSet::Arm64
    } else {
        panic!()
    };
    buffer.emit_u8(instruction_set.to_int());

    let full = handle(byte_array_from_buffer(vm, &buffer.data()));

    allocate_encoded_compilation_info(
        vm,
        bytecode_array,
        constpool_array,
        registers_array,
        type_params,
        full,
        bytecode_fct.arguments() as i32,
    )
}

fn encode_bytecode_array(vm: &VM, bytecode: &[u8], buffer2: &mut ByteBuffer) -> Ref<UInt8Array> {
    buffer2.emit_u32(bytecode.len() as u32);

    for &byte in bytecode {
        buffer2.emit_u8(byte);
    }

    byte_array_from_buffer(vm, bytecode)
}

fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_array: Handle<UInt8Array>,
    constpool_array: Handle<UInt8Array>,
    registers_array: Handle<UInt8Array>,
    type_params: Handle<UInt8Array>,
    full: Handle<UInt8Array>,
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

    let fid = vm.field_in_class(cls_id, "typeParams");
    object::write_ref(vm, obj, cls_id, fid, type_params.direct().cast::<Obj>());

    let fid = vm.field_in_class(cls_id, "full");
    object::write_ref(vm, obj, cls_id, fid, full.direct().cast::<Obj>());

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
    object::write_int32(vm, obj, cls_id, fid, instruction_set.to_int() as i32);

    obj
}

fn encode_registers_array(
    vm: &VM,
    fct: &BytecodeFunction,
    buffer2: &mut ByteBuffer,
) -> Ref<UInt8Array> {
    let mut buffer = Vec::new();

    buffer
        .write_u32::<LittleEndian>(fct.registers().len() as u32)
        .unwrap();
    buffer2.emit_u32(fct.registers().len() as u32);

    for ty in fct.registers().iter() {
        encode_bytecode_type(vm, ty, &mut buffer, buffer2);
    }

    byte_array_from_buffer(vm, &buffer)
}

fn encode_type_params(
    vm: &VM,
    type_params: &SourceTypeArray,
    buffer2: &mut ByteBuffer,
) -> Ref<UInt8Array> {
    let mut buffer = Vec::new();
    encode_source_type_array(vm, type_params, &mut buffer, buffer2);
    byte_array_from_buffer(vm, &buffer)
}

fn encode_bytecode_type(
    vm: &VM,
    ty: &BytecodeType,
    buffer: &mut Vec<u8>,
    buffer2: &mut ByteBuffer,
) {
    match ty {
        BytecodeType::Bool => {
            buffer.push(BytecodeTypeKind::Bool as u8);
            buffer2.emit_u8(BytecodeTypeKind::Bool as u8);
        }
        BytecodeType::Char => {
            buffer.push(BytecodeTypeKind::Char as u8);
            buffer2.emit_u8(BytecodeTypeKind::Char as u8);
        }
        BytecodeType::UInt8 => {
            buffer.push(BytecodeTypeKind::UInt8 as u8);
            buffer2.emit_u8(BytecodeTypeKind::UInt8 as u8);
        }
        BytecodeType::Int32 => {
            buffer.push(BytecodeTypeKind::Int32 as u8);
            buffer2.emit_u8(BytecodeTypeKind::Int32 as u8);
        }
        BytecodeType::Int64 => {
            buffer.push(BytecodeTypeKind::Int64 as u8);
            buffer2.emit_u8(BytecodeTypeKind::Int64 as u8);
        }
        BytecodeType::Float32 => {
            buffer.push(BytecodeTypeKind::Float32 as u8);
            buffer2.emit_u8(BytecodeTypeKind::Float32 as u8);
        }
        BytecodeType::Float64 => {
            buffer.push(BytecodeTypeKind::Float64 as u8);
            buffer2.emit_u8(BytecodeTypeKind::Float64 as u8);
        }
        BytecodeType::Ptr => {
            buffer.push(BytecodeTypeKind::Ptr as u8);
            buffer2.emit_u8(BytecodeTypeKind::Ptr as u8);
        }
        BytecodeType::Tuple(tuple_id) => {
            buffer.push(SourceTypeOpcode::Tuple.to_int());
            buffer2.emit_u8(BytecodeTypeKind::Tuple as u8);
            let subtypes = vm.tuples.lock().get(*tuple_id);
            buffer
                .write_u32::<LittleEndian>(subtypes.len() as u32)
                .unwrap();
            buffer2.emit_u32(subtypes.len() as u32);
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer, buffer2);
            }
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.push(SourceTypeOpcode::TypeParam.to_int());
            buffer2.emit_u8(BytecodeTypeKind::TypeParam as u8);
            buffer.write_u32::<LittleEndian>(*type_param_id).unwrap();
            buffer2.emit_u32(*type_param_id);
        }
        BytecodeType::Enum(enum_id, ref source_type_array) => {
            buffer.push(SourceTypeOpcode::Enum.to_int());
            buffer2.emit_u8(BytecodeTypeKind::Enum as u8);
            buffer
                .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer, buffer2);
        }
        BytecodeType::Struct(struct_id, ref source_type_array) => {
            buffer.push(SourceTypeOpcode::Struct.to_int());
            buffer2.emit_u8(BytecodeTypeKind::Struct as u8);
            buffer
                .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer, buffer2);
        }
    }
}

pub fn encode_source_type(vm: &VM, ty: SourceType, buffer: &mut Vec<u8>, buffer2: &mut ByteBuffer) {
    match ty {
        SourceType::Error | SourceType::Any | SourceType::Ptr | SourceType::This => unreachable!(),
        SourceType::Unit => {
            buffer.push(SourceTypeOpcode::Unit.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Unit.to_int());
        }
        SourceType::Bool => {
            buffer.push(SourceTypeOpcode::Bool.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Bool.to_int());
        }
        SourceType::Char => {
            buffer.push(SourceTypeOpcode::Char.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Char.to_int());
        }
        SourceType::UInt8 => {
            buffer.push(SourceTypeOpcode::UInt8.to_int());
            buffer2.emit_u8(SourceTypeOpcode::UInt8.to_int());
        }
        SourceType::Int32 => {
            buffer.push(SourceTypeOpcode::Int32.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Int32.to_int());
        }
        SourceType::Int64 => {
            buffer.push(SourceTypeOpcode::Int64.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Int64.to_int());
        }
        SourceType::Float32 => {
            buffer.push(SourceTypeOpcode::Float32.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Float32.to_int());
        }
        SourceType::Float64 => {
            buffer.push(SourceTypeOpcode::Float64.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Float64.to_int());
        }
        SourceType::Class(cls_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Class.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Class.to_int());
            buffer
                .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(cls_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer, buffer2);
        }
        SourceType::Struct(struct_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Struct.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Struct.to_int());
            buffer
                .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer, buffer2);
        }
        SourceType::Trait(trait_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Trait.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Trait.to_int());
            buffer
                .write_u32::<LittleEndian>(trait_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(trait_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer, buffer2);
        }
        SourceType::Enum(enum_id, source_type_array_id) => {
            let source_type_array = vm.source_type_arrays.lock().get(source_type_array_id);
            buffer.push(SourceTypeOpcode::Enum.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Enum.to_int());
            buffer
                .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer, buffer2);
        }
        SourceType::Tuple(tuple_id) => {
            buffer.push(SourceTypeOpcode::Tuple.to_int());
            buffer2.emit_u8(SourceTypeOpcode::Tuple.to_int());
            let subtypes = vm.tuples.lock().get(tuple_id);
            buffer
                .write_u32::<LittleEndian>(subtypes.len() as u32)
                .unwrap();
            buffer2.emit_u32(subtypes.len() as u32);
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer, buffer2);
            }
        }
        SourceType::TypeParam(type_param_id) => {
            buffer.push(SourceTypeOpcode::TypeParam.to_int());
            buffer2.emit_u8(SourceTypeOpcode::TypeParam.to_int());
            buffer
                .write_u32::<LittleEndian>(type_param_id.to_usize() as u32)
                .unwrap();
            buffer2.emit_u32(type_param_id.to_usize() as u32);
        }
        SourceType::Module(_) | SourceType::Lambda(_) => unimplemented!(),
    }
}

fn encode_constpool_array(
    vm: &VM,
    buffer2: &mut ByteBuffer,
    fct: &BytecodeFunction,
) -> Ref<UInt8Array> {
    let mut buffer = Vec::new();

    buffer
        .write_u32::<LittleEndian>(fct.const_pool_entries().len() as u32)
        .unwrap();

    buffer2.emit_u32(fct.const_pool_entries().len() as u32);

    for const_entry in fct.const_pool_entries() {
        match const_entry {
            ConstPoolEntry::String(ref value) => {
                buffer.push(ConstPoolOpcode::Float32.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Float32.to_int());
                buffer
                    .write_u32::<LittleEndian>(value.len() as u32)
                    .unwrap();
                buffer2.emit_u32(value.len() as u32);

                for byte in value.bytes() {
                    buffer.push(byte);
                    buffer2.emit_u8(byte);
                }
            }
            &ConstPoolEntry::Float32(value) => {
                buffer.push(ConstPoolOpcode::Float32.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Float32.to_int());
                buffer.write_u32::<LittleEndian>(value.to_bits()).unwrap();
                buffer2.emit_u32(value.to_bits());
            }
            &ConstPoolEntry::Float64(value) => {
                buffer.push(ConstPoolOpcode::Float64.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Float32.to_int());
                buffer.write_u64::<LittleEndian>(value.to_bits()).unwrap();
                buffer2.emit_u64(value.to_bits());
            }
            &ConstPoolEntry::Int32(value) => {
                buffer.push(ConstPoolOpcode::Int32.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Int32.to_int());
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
                buffer2.emit_u32(value as u32);
            }
            &ConstPoolEntry::Int64(value) => {
                buffer.push(ConstPoolOpcode::Int64.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Int64.to_int());
                buffer.write_u64::<LittleEndian>(value as u64).unwrap();
                buffer2.emit_u64(value as u64);
            }
            &ConstPoolEntry::Char(value) => {
                buffer.push(ConstPoolOpcode::Char.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Char.to_int());
                buffer.write_u32::<LittleEndian>(value as u32).unwrap();
                buffer2.emit_u32(value as u32);
            }
            &ConstPoolEntry::Fct(fct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Fct.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Fct.to_int());
                buffer
                    .write_u32::<LittleEndian>(fct_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(fct_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
            }
            &ConstPoolEntry::Generic(tp_id, fct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Generic.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Generic.to_int());
                buffer
                    .write_u32::<LittleEndian>(tp_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(tp_id.to_usize() as u32);
                buffer
                    .write_u32::<LittleEndian>(fct_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(fct_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
            }
            &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Class.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Class.to_int());
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(cls_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
            }
            &ConstPoolEntry::Field(cls_id, ref source_type_array, field_id) => {
                buffer.push(ConstPoolOpcode::Field.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Field.to_int());
                buffer
                    .write_u32::<LittleEndian>(cls_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(cls_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(field_id.to_usize() as u32);
            }
            &ConstPoolEntry::FieldFixed(cls_def_id, field_id) => {
                buffer.push(ConstPoolOpcode::FieldFixed.to_int());
                buffer2.emit_u8(ConstPoolOpcode::FieldFixed.to_int());
                buffer
                    .write_u32::<LittleEndian>(cls_def_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(cls_def_id.to_usize() as u32);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(field_id.to_usize() as u32);
            }
            &ConstPoolEntry::Enum(enum_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Enum.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Enum.to_int());
                buffer
                    .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(enum_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
            }
            &ConstPoolEntry::EnumVariant(enum_id, ref source_type_array, variant_id) => {
                buffer.push(ConstPoolOpcode::EnumVariant.to_int());
                buffer2.emit_u8(ConstPoolOpcode::EnumVariant.to_int());
                buffer
                    .write_u32::<LittleEndian>(enum_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(enum_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
                buffer.write_u32::<LittleEndian>(variant_id as u32).unwrap();
                buffer2.emit_u32(variant_id as u32);
            }
            &ConstPoolEntry::Struct(struct_id, ref source_type_array) => {
                buffer.push(ConstPoolOpcode::Struct.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Struct.to_int());
                buffer
                    .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(struct_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
            }
            &ConstPoolEntry::StructField(struct_id, ref source_type_array, field_id) => {
                buffer.push(ConstPoolOpcode::StructField.to_int());
                buffer2.emit_u8(ConstPoolOpcode::StructField.to_int());
                buffer
                    .write_u32::<LittleEndian>(struct_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(struct_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
                buffer
                    .write_u32::<LittleEndian>(field_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(field_id.to_usize() as u32);
            }
            &ConstPoolEntry::Trait(trait_id, ref source_type_array, ref source_type) => {
                buffer.push(ConstPoolOpcode::Trait.to_int());
                buffer2.emit_u8(ConstPoolOpcode::Trait.to_int());
                buffer
                    .write_u32::<LittleEndian>(trait_id.to_usize() as u32)
                    .unwrap();
                buffer2.emit_u32(trait_id.to_usize() as u32);
                encode_source_type_array(vm, source_type_array, &mut buffer, buffer2);
                encode_source_type(vm, source_type.clone(), &mut buffer, buffer2);
            }
        }
    }

    byte_array_from_buffer(vm, &buffer)
}

fn encode_source_type_array(
    vm: &VM,
    sta: &SourceTypeArray,
    buffer: &mut Vec<u8>,
    buffer2: &mut ByteBuffer,
) {
    buffer.write_u32::<LittleEndian>(sta.len() as u32).unwrap();
    buffer2.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_source_type(vm, ty, buffer, buffer2);
    }
}

pub struct ByteBuffer {
    data: Vec<u8>,
}

impl ByteBuffer {
    pub fn new() -> ByteBuffer {
        ByteBuffer { data: Vec::new() }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn extend_from_slice(&mut self, data: &[u8]) {
        self.data.extend_from_slice(data);
    }

    pub fn emit_u8(&mut self, data: u8) {
        self.data.push(data);
    }

    pub fn emit_u32(&mut self, data: u32) {
        self.data.write_u32::<LittleEndian>(data).unwrap();
    }

    pub fn emit_u64(&mut self, data: u64) {
        self.data.write_u64::<LittleEndian>(data).unwrap();
    }
}
