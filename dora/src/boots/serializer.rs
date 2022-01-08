use byteorder::{LittleEndian, WriteBytesExt};

use crate::bytecode::{
    BytecodeFunction, ConstPoolEntry, ConstPoolOpcode, InstructionSet, SourceTypeOpcode,
};
use crate::bytecode::{BytecodeType, BytecodeTypeKind};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::object::{byte_array_from_buffer, Obj, Ref};
use crate::vm::VM;

pub fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &SourceTypeArray,
    architecture: InstructionSet,
) -> Ref<Obj> {
    let mut buffer = ByteBuffer::new();
    encode_compilation_info(vm, bytecode_fct, type_params, architecture, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub fn allocate_encoded_bytecode_function(vm: &VM, bytecode_fct: &BytecodeFunction) -> Ref<Obj> {
    let mut buffer = ByteBuffer::new();
    encode_bytecode_function(vm, bytecode_fct, &mut buffer);
    byte_array_from_buffer(vm, &buffer.data()).cast()
}

fn encode_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &SourceTypeArray,
    architecture: InstructionSet,
    buffer: &mut ByteBuffer,
) {
    encode_bytecode_function(vm, bytecode_fct, buffer);
    encode_type_params(vm, type_params, buffer);
    encode_architecture(architecture, buffer);
}

fn encode_bytecode_function(vm: &VM, bytecode_fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    encode_bytecode_array(bytecode_fct, buffer);
    encode_constpool_array(vm, bytecode_fct, buffer);
    encode_registers_array(vm, bytecode_fct, buffer);
    buffer.emit_u32(bytecode_fct.arguments());
}

fn encode_architecture(architecture: InstructionSet, buffer: &mut ByteBuffer) {
    buffer.emit_u8(architecture.to_int());
}

fn encode_bytecode_array(fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.code().len() as u32);

    for &byte in fct.code() {
        buffer.emit_u8(byte);
    }
}

fn encode_registers_array(vm: &VM, fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.registers().len() as u32);

    for ty in fct.registers().iter() {
        encode_bytecode_type(vm, ty, buffer);
    }
}

fn encode_type_params(vm: &VM, type_params: &SourceTypeArray, buffer: &mut ByteBuffer) {
    encode_source_type_array(vm, type_params, buffer);
}

fn encode_bytecode_type(vm: &VM, ty: &BytecodeType, buffer: &mut ByteBuffer) {
    match ty {
        BytecodeType::Bool => {
            buffer.emit_u8(BytecodeTypeKind::Bool as u8);
        }
        BytecodeType::Char => {
            buffer.emit_u8(BytecodeTypeKind::Char as u8);
        }
        BytecodeType::UInt8 => {
            buffer.emit_u8(BytecodeTypeKind::UInt8 as u8);
        }
        BytecodeType::Int32 => {
            buffer.emit_u8(BytecodeTypeKind::Int32 as u8);
        }
        BytecodeType::Int64 => {
            buffer.emit_u8(BytecodeTypeKind::Int64 as u8);
        }
        BytecodeType::Float32 => {
            buffer.emit_u8(BytecodeTypeKind::Float32 as u8);
        }
        BytecodeType::Float64 => {
            buffer.emit_u8(BytecodeTypeKind::Float64 as u8);
        }
        BytecodeType::Ptr => {
            buffer.emit_u8(BytecodeTypeKind::Ptr as u8);
        }
        BytecodeType::Tuple(tuple_id) => {
            buffer.emit_u8(BytecodeTypeKind::Tuple as u8);
            let subtypes = vm.tuples.lock().get(*tuple_id);
            buffer.emit_u32(subtypes.len() as u32);
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer);
            }
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.emit_u8(BytecodeTypeKind::TypeParam as u8);
            buffer.emit_u32(*type_param_id);
        }
        BytecodeType::Enum(enum_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Enum as u8);
            buffer.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Struct(struct_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Struct as u8);
            buffer.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
    }
}

fn encode_source_type_array(vm: &VM, sta: &SourceTypeArray, buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_source_type(vm, ty, buffer);
    }
}

fn encode_source_type(vm: &VM, ty: SourceType, buffer: &mut ByteBuffer) {
    match ty {
        SourceType::Error | SourceType::Any | SourceType::Ptr | SourceType::This => unreachable!(),
        SourceType::Unit => {
            buffer.emit_u8(SourceTypeOpcode::Unit.to_int());
        }
        SourceType::Bool => {
            buffer.emit_u8(SourceTypeOpcode::Bool.to_int());
        }
        SourceType::Char => {
            buffer.emit_u8(SourceTypeOpcode::Char.to_int());
        }
        SourceType::UInt8 => {
            buffer.emit_u8(SourceTypeOpcode::UInt8.to_int());
        }
        SourceType::Int32 => {
            buffer.emit_u8(SourceTypeOpcode::Int32.to_int());
        }
        SourceType::Int64 => {
            buffer.emit_u8(SourceTypeOpcode::Int64.to_int());
        }
        SourceType::Float32 => {
            buffer.emit_u8(SourceTypeOpcode::Float32.to_int());
        }
        SourceType::Float64 => {
            buffer.emit_u8(SourceTypeOpcode::Float64.to_int());
        }
        SourceType::Class(cls_id, source_type_array) => {
            buffer.emit_u8(SourceTypeOpcode::Class.to_int());
            buffer.emit_u32(cls_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Struct(struct_id, source_type_array) => {
            buffer.emit_u8(SourceTypeOpcode::Struct.to_int());
            buffer.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Trait(trait_id, source_type_array) => {
            buffer.emit_u8(SourceTypeOpcode::Trait.to_int());
            buffer.emit_u32(trait_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Enum(enum_id, source_type_array) => {
            buffer.emit_u8(SourceTypeOpcode::Enum.to_int());
            buffer.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, &source_type_array, buffer);
        }
        SourceType::Tuple(tuple_id) => {
            buffer.emit_u8(SourceTypeOpcode::Tuple.to_int());
            let subtypes = vm.tuples.lock().get(tuple_id);
            buffer.emit_u32(subtypes.len() as u32);
            for subty in subtypes.iter() {
                encode_source_type(vm, subty.clone(), buffer);
            }
        }
        SourceType::TypeParam(type_param_id) => {
            buffer.emit_u8(SourceTypeOpcode::TypeParam.to_int());
            buffer.emit_u32(type_param_id.to_usize() as u32);
        }
        SourceType::Module(_) | SourceType::Lambda(_) => unimplemented!(),
    }
}

fn encode_constpool_array(vm: &VM, fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.const_pool_entries().len() as u32);

    for const_entry in fct.const_pool_entries() {
        encode_constpool_entry(vm, const_entry, buffer);
    }
}

fn encode_constpool_entry(vm: &VM, const_entry: &ConstPoolEntry, buffer: &mut ByteBuffer) {
    match const_entry {
        ConstPoolEntry::String(ref value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.to_int());
            buffer.emit_u32(value.len() as u32);

            for byte in value.bytes() {
                buffer.emit_u8(byte);
            }
        }
        &ConstPoolEntry::Float32(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.to_int());
            buffer.emit_u32(value.to_bits());
        }
        &ConstPoolEntry::Float64(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.to_int());
            buffer.emit_u64(value.to_bits());
        }
        &ConstPoolEntry::Int32(value) => {
            buffer.emit_u8(ConstPoolOpcode::Int32.to_int());
            buffer.emit_u32(value as u32);
        }
        &ConstPoolEntry::Int64(value) => {
            buffer.emit_u8(ConstPoolOpcode::Int64.to_int());
            buffer.emit_u64(value as u64);
        }
        &ConstPoolEntry::Char(value) => {
            buffer.emit_u8(ConstPoolOpcode::Char.to_int());
            buffer.emit_u32(value as u32);
        }
        &ConstPoolEntry::Fct(fct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Fct.to_int());
            buffer.emit_u32(fct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Generic(tp_id, fct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Generic.to_int());
            buffer.emit_u32(tp_id.to_usize() as u32);
            buffer.emit_u32(fct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Class.to_int());
            buffer.emit_u32(cls_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Field(cls_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::Field.to_int());
            buffer.emit_u32(cls_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
            buffer.emit_u32(field_id.to_usize() as u32);
        }
        &ConstPoolEntry::FieldFixed(cls_def_id, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::FieldFixed.to_int());
            buffer.emit_u32(cls_def_id.to_usize() as u32);
            buffer.emit_u32(field_id.to_usize() as u32);
        }
        &ConstPoolEntry::Enum(enum_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Enum.to_int());
            buffer.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::EnumVariant(enum_id, ref source_type_array, variant_id) => {
            buffer.emit_u8(ConstPoolOpcode::EnumVariant.to_int());
            buffer.emit_u32(enum_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
            buffer.emit_u32(variant_id as u32);
        }
        &ConstPoolEntry::Struct(struct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Struct.to_int());
            buffer.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::StructField(struct_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::StructField.to_int());
            buffer.emit_u32(struct_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
            buffer.emit_u32(field_id.to_usize() as u32);
        }
        &ConstPoolEntry::Trait(trait_id, ref source_type_array, ref source_type) => {
            buffer.emit_u8(ConstPoolOpcode::Trait.to_int());
            buffer.emit_u32(trait_id.to_usize() as u32);
            encode_source_type_array(vm, source_type_array, buffer);
            encode_source_type(vm, source_type.clone(), buffer);
        }
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
