use std::convert::TryInto;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::boots::data::InstructionSet;
use crate::object::{byte_array_from_buffer, Obj, Ref};
use crate::vm::VM;
use dora_frontend::bytecode::{
    BytecodeFunction, BytecodeTypeArray, ConstPoolEntry, ConstPoolOpcode,
};
use dora_frontend::bytecode::{BytecodeType, BytecodeTypeKind};

pub fn allocate_encoded_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    architecture: InstructionSet,
) -> Ref<Obj> {
    let mut buffer = ByteBuffer::new();
    encode_compilation_info(vm, bytecode_fct, type_params, architecture, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn encode_compilation_info(
    vm: &VM,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
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
    buffer.emit_u8(architecture.into());
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

fn encode_type_params(vm: &VM, type_params: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    encode_bytecode_type_array(vm, type_params, buffer);
}

fn encode_bytecode_type_array(vm: &VM, sta: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(vm, &ty, buffer);
    }
}

fn encode_bytecode_type(vm: &VM, ty: &BytecodeType, buffer: &mut ByteBuffer) {
    match ty {
        BytecodeType::Unit => {
            buffer.emit_u8(BytecodeTypeKind::Unit as u8);
        }
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
        BytecodeType::Tuple(subtypes) => {
            buffer.emit_u8(BytecodeTypeKind::Tuple as u8);
            encode_bytecode_type_array(vm, subtypes, buffer);
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.emit_u8(BytecodeTypeKind::TypeParam as u8);
            buffer.emit_id(*type_param_id as usize);
        }
        BytecodeType::Enum(enum_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Enum as u8);
            buffer.emit_id(enum_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Struct(struct_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Struct as u8);
            buffer.emit_id(struct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Class(class_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Class as u8);
            buffer.emit_id(class_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Trait(trait_id, ref source_type_array) => {
            buffer.emit_u8(BytecodeTypeKind::Trait as u8);
            buffer.emit_id(trait_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        BytecodeType::Lambda(params, ret) => {
            buffer.emit_u8(BytecodeTypeKind::Lambda as u8);
            encode_bytecode_type_array(vm, params, buffer);
            encode_bytecode_type(vm, ret.as_ref(), buffer);
        }
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
            buffer.emit_u8(ConstPoolOpcode::Float32.into());
            buffer.emit_u32(value.len() as u32);

            for byte in value.bytes() {
                buffer.emit_u8(byte);
            }
        }
        &ConstPoolEntry::Float32(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.into());
            buffer.emit_u32(value.to_bits());
        }
        &ConstPoolEntry::Float64(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.into());
            buffer.emit_u64(value.to_bits());
        }
        &ConstPoolEntry::Int32(value) => {
            buffer.emit_u8(ConstPoolOpcode::Int32.into());
            buffer.emit_u32(value as u32);
        }
        &ConstPoolEntry::Int64(value) => {
            buffer.emit_u8(ConstPoolOpcode::Int64.into());
            buffer.emit_u64(value as u64);
        }
        &ConstPoolEntry::Char(value) => {
            buffer.emit_u8(ConstPoolOpcode::Char.into());
            buffer.emit_u32(value as u32);
        }
        &ConstPoolEntry::Fct(fct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Fct.into());
            buffer.emit_id(fct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Generic(tp_id, fct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Generic.into());
            buffer.emit_id(tp_id as usize);
            buffer.emit_id(fct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Class.into());
            buffer.emit_id(cls_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::Field(cls_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::Field.into());
            buffer.emit_id(cls_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            buffer.emit_id(field_id as usize);
        }
        &ConstPoolEntry::Enum(enum_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Enum.into());
            buffer.emit_id(enum_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::EnumVariant(enum_id, ref source_type_array, variant_idx) => {
            buffer.emit_u8(ConstPoolOpcode::EnumVariant.into());
            buffer.emit_id(enum_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            buffer.emit_id(variant_idx.try_into().unwrap());
        }
        &ConstPoolEntry::EnumElement(enum_id, ref source_type_array, variant_idx, element_idx) => {
            buffer.emit_u8(ConstPoolOpcode::EnumElement.into());
            buffer.emit_id(enum_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            buffer.emit_id(variant_idx.try_into().unwrap());
            buffer.emit_id(element_idx as usize);
        }
        &ConstPoolEntry::Struct(struct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Struct.into());
            buffer.emit_id(struct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::StructField(struct_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::StructField.into());
            buffer.emit_id(struct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            buffer.emit_id(field_id as usize);
        }
        &ConstPoolEntry::Trait(trait_id, ref source_type_array, ref source_type) => {
            buffer.emit_u8(ConstPoolOpcode::Trait.into());
            buffer.emit_id(trait_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            encode_bytecode_type(vm, source_type, buffer);
        }
        &ConstPoolEntry::TupleElement(ref tuple_ty, element_idx) => {
            buffer.emit_u8(ConstPoolOpcode::TupleElement.into());
            encode_bytecode_type_array(vm, &tuple_ty.tuple_subtypes(), buffer);
            buffer.emit_id(element_idx as usize);
        }
        &ConstPoolEntry::Tuple(ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Tuple.into());
            encode_bytecode_type_array(vm, &source_type_array, buffer);
        }
        &ConstPoolEntry::Lambda(ref params, ref return_type) => {
            buffer.emit_u8(ConstPoolOpcode::Lambda.into());
            encode_bytecode_type_array(vm, params, buffer);
            encode_bytecode_type(vm, return_type, buffer);
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

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[allow(dead_code)]
    pub fn extend_from_slice(&mut self, data: &[u8]) {
        self.data.extend_from_slice(data);
    }

    pub fn emit_u8(&mut self, data: u8) {
        self.data.push(data);
    }

    pub fn emit_u32(&mut self, data: u32) {
        self.data.write_u32::<LittleEndian>(data).unwrap();
    }

    pub fn emit_id(&mut self, data: usize) {
        assert!(data <= i32::MAX as usize);
        self.emit_u32(data as u32);
    }

    pub fn emit_u64(&mut self, data: u64) {
        self.data.write_u64::<LittleEndian>(data).unwrap();
    }
}
