use std::convert::TryInto;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::{CompilationData, SpecializeSelf, get_bytecode};
use dora_bytecode::opcode as opc;
use dora_bytecode::{
    BytecodeFunction, BytecodeTypeArray, ConstPoolEntry, ConstPoolOpcode, ConstValue, EnumData,
    FunctionData, Location, Program, StructData,
};
use dora_bytecode::{BytecodeTraitType, BytecodeType};

pub fn encode_compilation_info(compilation_data: &CompilationData, buffer: &mut ByteBuffer) {
    encode_bytecode_function(&compilation_data.bytecode_fct, buffer);
    buffer.emit_id(compilation_data.fct_id.index());
    encode_type_params(&compilation_data.type_params, buffer);
    encode_bytecode_type(&compilation_data.return_type, buffer);
    encode_optional_specialize_self(&compilation_data.specialize_self, buffer);
    encode_location(&compilation_data.loc, buffer);
    buffer.emit_bool(compilation_data.emit_debug);
    buffer.emit_bool(compilation_data.emit_final_graph);
    buffer.emit_bool(compilation_data.emit_graph_after_each_pass);
    buffer.emit_bool(compilation_data.emit_html);
    buffer.emit_bool(compilation_data.emit_code_comments);
}

pub fn encode_optional_specialize_self(
    specialize_self: &Option<SpecializeSelf>,
    buffer: &mut ByteBuffer,
) {
    if let Some(specialize_self) = specialize_self {
        buffer.emit_bool(true);
        encode_specialize_self(specialize_self, buffer);
    } else {
        buffer.emit_bool(false);
    }
}

pub fn encode_specialize_self(specialize_self: &SpecializeSelf, buffer: &mut ByteBuffer) {
    buffer.emit_id(specialize_self.impl_id.index());
    buffer.emit_u32(specialize_self.container_type_params as u32);
    encode_bytecode_trait_type(&specialize_self.trait_ty, buffer);
    encode_bytecode_type(&specialize_self.extended_ty, buffer);
}

pub fn encode_struct_data(struct_: &StructData, buffer: &mut ByteBuffer) {
    let count = struct_.type_params.names.len() as u32;
    buffer.emit_u32(count);

    let types = struct_
        .fields
        .iter()
        .map(|f| f.ty.clone())
        .collect::<Vec<_>>();
    encode_bytecode_type_slice(&types, buffer);
}

pub fn encode_enum_data(enum_: &EnumData, buffer: &mut ByteBuffer) {
    let count = enum_.type_params.names.len() as u32;
    buffer.emit_u32(count);

    let variants = enum_.variants.len();
    buffer.emit_u32(variants as u32);

    for variant in &enum_.variants {
        encode_bytecode_type_slice(&variant.arguments, buffer);
    }
}

pub fn encode_function_inlining_info(fct: &FunctionData, buffer: &mut ByteBuffer) {
    buffer.emit_bool(fct.bytecode.is_some());
    buffer.emit_u32(
        fct.bytecode
            .as_ref()
            .map(|bc| bc.code().len() as u32)
            .unwrap_or(0),
    );
    buffer.emit_bool(fct.is_force_inline);
    buffer.emit_bool(fct.is_never_inline);
}

pub fn encode_function_bytecode_data(
    program: &Program,
    fct: &FunctionData,
    buffer: &mut ByteBuffer,
) {
    let (bc, specialize_self) = get_bytecode(program, fct).expect("missing bytecode");
    encode_bytecode_function(bc, buffer);
    encode_bytecode_type(&fct.return_type, buffer);
    encode_optional_specialize_self(&specialize_self, buffer);
}

fn encode_bytecode_function(bytecode_fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    encode_bytecode_array(bytecode_fct, buffer);
    encode_constpool_array(bytecode_fct, buffer);
    encode_registers_array(bytecode_fct, buffer);
    encode_bytecode_locations(bytecode_fct, buffer);
    buffer.emit_u32(bytecode_fct.arguments());
}

fn encode_bytecode_array(fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.code().len() as u32);

    for &byte in fct.code() {
        buffer.emit_u8(byte);
    }
}

fn encode_registers_array(fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.registers().len() as u32);

    for ty in fct.registers().iter() {
        encode_bytecode_type(ty, buffer);
    }
}

fn encode_bytecode_locations(fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.locations().len() as u32);

    for (offset, loc) in fct.locations() {
        buffer.emit_u32(offset.to_u32());
        encode_location(loc, buffer);
    }
}

fn encode_location(loc: &Location, buffer: &mut ByteBuffer) {
    buffer.emit_u32(loc.line());
    buffer.emit_u32(loc.column());
}

fn encode_type_params(type_params: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    encode_bytecode_type_array(type_params, buffer);
}

pub fn encode_bytecode_type_array(sta: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(&ty, buffer);
    }
}

fn encode_bytecode_type_slice(sta: &[BytecodeType], buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(ty, buffer);
    }
}

pub fn encode_bytecode_type(ty: &BytecodeType, buffer: &mut ByteBuffer) {
    match ty {
        BytecodeType::Unit => {
            buffer.emit_u8(opc::BYTECODE_TYPE_UNIT);
        }
        BytecodeType::Bool => {
            buffer.emit_u8(opc::BYTECODE_TYPE_BOOL);
        }
        BytecodeType::Char => {
            buffer.emit_u8(opc::BYTECODE_TYPE_CHAR);
        }
        BytecodeType::UInt8 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_U_INT8);
        }
        BytecodeType::Int32 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_INT32);
        }
        BytecodeType::Int64 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_INT64);
        }
        BytecodeType::Float32 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_FLOAT32);
        }
        BytecodeType::Float64 => {
            buffer.emit_u8(opc::BYTECODE_TYPE_FLOAT64);
        }
        BytecodeType::Ptr => {
            buffer.emit_u8(opc::BYTECODE_TYPE_PTR);
        }
        BytecodeType::Address => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ADDRESS);
        }
        BytecodeType::This => {
            buffer.emit_u8(opc::BYTECODE_TYPE_THIS);
        }
        BytecodeType::Tuple(subtypes) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TUPLE);
            encode_bytecode_type_array(subtypes, buffer);
        }
        BytecodeType::TypeParam(type_param_id) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TYPE_PARAM);
            buffer.emit_id(*type_param_id as usize);
        }
        BytecodeType::Enum(enum_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ENUM);
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::Struct(struct_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_STRUCT);
            buffer.emit_id(struct_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::Class(class_id, source_type_array) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_CLASS);
            buffer.emit_id(class_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        BytecodeType::TraitObject(trait_id, source_type_array, assoc_types) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_TRAIT_OBJECT);
            buffer.emit_id(trait_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            encode_bytecode_type_array(assoc_types, buffer);
        }
        BytecodeType::Lambda(params, ret) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_LAMBDA);
            encode_bytecode_type_array(params, buffer);
            encode_bytecode_type(ret.as_ref(), buffer);
        }
        BytecodeType::Assoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            buffer.emit_u8(opc::BYTECODE_TYPE_ASSOC);
            encode_bytecode_type(ty.as_ref(), buffer);
            encode_bytecode_trait_type(trait_ty, buffer);
            buffer.emit_id(assoc_id.index());
        }
        BytecodeType::TypeAlias(..) => {
            unreachable!()
        }
        BytecodeType::Ref(inner) => {
            buffer.emit_u8(opc::BYTECODE_TYPE_REF);
            encode_bytecode_type(inner.as_ref(), buffer);
        }
    }
}

fn encode_bytecode_trait_type(trait_ty: &BytecodeTraitType, buffer: &mut ByteBuffer) {
    buffer.emit_u32(trait_ty.trait_id.index_as_u32());
    encode_bytecode_type_array(&trait_ty.type_params, buffer);
    buffer.emit_u32(trait_ty.bindings.len() as u32);

    for (alias_id, ty) in &trait_ty.bindings {
        buffer.emit_u32(alias_id.index_as_u32());
        encode_bytecode_type(ty, buffer);
    }
}

fn encode_constpool_array(fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    buffer.emit_u32(fct.const_pool_entries().len() as u32);

    for const_entry in fct.const_pool_entries() {
        encode_constpool_entry(const_entry, buffer);
    }
}

fn encode_constpool_entry(const_entry: &ConstPoolEntry, buffer: &mut ByteBuffer) {
    match const_entry {
        ConstPoolEntry::String(value) => {
            buffer.emit_u8(ConstPoolOpcode::String.into());
            encode_string(value, buffer);
        }
        &ConstPoolEntry::Float32(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float32.into());
            buffer.emit_u32(value.to_bits());
        }
        &ConstPoolEntry::Float64(value) => {
            buffer.emit_u8(ConstPoolOpcode::Float64.into());
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
            buffer.emit_id(fct_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        &ConstPoolEntry::TraitObjectMethod(ref trait_object_ty, fct_id) => {
            buffer.emit_u8(ConstPoolOpcode::TraitObjectMethod.into());
            encode_bytecode_type(trait_object_ty, buffer);
            buffer.emit_id(fct_id.index());
        }
        &ConstPoolEntry::Generic {
            ref object_type,
            ref trait_ty,
            fct_id,
            ref fct_type_params,
        } => {
            buffer.emit_u8(ConstPoolOpcode::Generic.into());
            encode_bytecode_type(object_type, buffer);
            encode_bytecode_trait_type(trait_ty, buffer);
            buffer.emit_id(fct_id.index());
            encode_bytecode_type_array(fct_type_params, buffer);
        }
        &ConstPoolEntry::Class(cls_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Class.into());
            buffer.emit_id(cls_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        &ConstPoolEntry::ClassField(cls_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::Field.into());
            buffer.emit_id(cls_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            buffer.emit_id(field_id as usize);
        }
        &ConstPoolEntry::Enum(enum_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Enum.into());
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        &ConstPoolEntry::EnumVariant(enum_id, ref source_type_array, variant_idx) => {
            buffer.emit_u8(ConstPoolOpcode::EnumVariant.into());
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            buffer.emit_id(variant_idx.try_into().unwrap());
        }
        &ConstPoolEntry::EnumElement(enum_id, ref source_type_array, variant_idx, element_idx) => {
            buffer.emit_u8(ConstPoolOpcode::EnumElement.into());
            buffer.emit_id(enum_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            buffer.emit_id(variant_idx.try_into().unwrap());
            buffer.emit_id(element_idx as usize);
        }
        &ConstPoolEntry::Struct(struct_id, ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Struct.into());
            buffer.emit_id(struct_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
        }
        &ConstPoolEntry::StructField(struct_id, ref source_type_array, field_id) => {
            buffer.emit_u8(ConstPoolOpcode::StructField.into());
            buffer.emit_id(struct_id.index());
            encode_bytecode_type_array(source_type_array, buffer);
            buffer.emit_id(field_id as usize);
        }
        &ConstPoolEntry::TraitObject {
            ref trait_ty,
            ref actual_object_ty,
        } => {
            buffer.emit_u8(ConstPoolOpcode::TraitObject.into());
            encode_bytecode_type(trait_ty, buffer);
            encode_bytecode_type(actual_object_ty, buffer);
        }
        &ConstPoolEntry::TupleElement(ref tuple_ty, element_idx) => {
            buffer.emit_u8(ConstPoolOpcode::TupleElement.into());
            encode_bytecode_type(tuple_ty, buffer);
            buffer.emit_id(element_idx as usize);
        }
        &ConstPoolEntry::Tuple(ref source_type_array) => {
            buffer.emit_u8(ConstPoolOpcode::Tuple.into());
            encode_bytecode_type_array(&source_type_array, buffer);
        }
        &ConstPoolEntry::Lambda(ref params, ref return_type) => {
            buffer.emit_u8(ConstPoolOpcode::Lambda.into());
            encode_bytecode_type_array(params, buffer);
            encode_bytecode_type(return_type, buffer);
        }
        &ConstPoolEntry::JumpTable {
            ref targets,
            default_target,
        } => {
            buffer.emit_u8(ConstPoolOpcode::JumpTable.into());
            buffer.emit_u32(targets.len() as u32);
            for target in targets {
                buffer.emit_u32(*target);
            }
            buffer.emit_u32(default_target);
        }
    }
}

pub fn encode_string(value: &str, buffer: &mut ByteBuffer) {
    buffer.emit_u32(value.len() as u32);

    for byte in value.bytes() {
        buffer.emit_u8(byte);
    }
}

pub fn encode_const_value(const_value: &ConstValue, buffer: &mut ByteBuffer) {
    match const_value {
        ConstValue::None => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_NONE);
        }

        ConstValue::Bool(value) => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_BOOL);
            buffer.emit_bool(*value);
        }

        ConstValue::Char(value) => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_CHAR);
            buffer.emit_u32(*value as u32);
        }

        ConstValue::Float(value) => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_FLOAT);
            buffer.emit_u64(value.to_bits());
        }

        ConstValue::Int(value) => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_INT);
            buffer.emit_u64(*value as u64);
        }

        ConstValue::String(value) => {
            buffer.emit_u8(opc::CONST_VALUE_OPCODE_STRING);
            encode_string(value, buffer);
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

    pub fn emit_bool(&mut self, value: bool) {
        self.emit_u8(if value { 1 } else { 0 });
    }

    pub fn emit_u32(&mut self, data: u32) {
        self.data.write_u32::<LittleEndian>(data).unwrap();
    }

    pub fn emit_u64(&mut self, data: u64) {
        self.data.write_u64::<LittleEndian>(data).unwrap();
    }

    pub fn emit_id(&mut self, data: usize) {
        assert!(data <= i32::MAX as usize);
        self.emit_u32(data as u32);
    }
}
