use std::convert::TryInto;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::boots::data::InstructionSet;
use crate::compiler::codegen::get_bytecode;
use crate::compiler::{CompilationData, CompilationMode};
use crate::gc::Address;
use crate::mirror::{byte_array_from_buffer, Object, Ref, UInt8Array};
use crate::{Shape, SpecializeSelf, VM};
use dora_bytecode::{
    BytecodeFunction, BytecodeTypeArray, ConstPoolEntry, ConstPoolOpcode, EnumData, FunctionData,
    Location, StructData,
};
use dora_bytecode::{BytecodeTraitType, BytecodeType, BytecodeTypeKind};

pub fn allocate_encoded_system_config(vm: &VM) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    encode_system_config(vm, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn encode_system_config(vm: &VM, buffer: &mut ByteBuffer) {
    encode_architecture(get_architecture(), buffer);
    buffer.emit_address(vm.native_methods.safepoint_trampoline());
    buffer.emit_address(vm.native_methods.trap_trampoline());
    buffer.emit_address(vm.native_methods.unreachable_trampoline());
    buffer.emit_address(vm.native_methods.fatal_error_trampoline());
    let ptr = Address::from_ptr(crate::gc::swiper::object_write_barrier_slow_path as *const u8);
    buffer.emit_address(ptr);
    buffer.emit_address(vm.native_methods.gc_allocation_trampoline());
    buffer.emit_address(vm.meta_space_start());
    buffer.emit_u32(Shape::offset_of_vtable() as u32);
    buffer.emit_bool(cfg!(target_family = "windows"));
    buffer.emit_bool(cfg!(target_family = "unix"));
    buffer.emit_bool(vm.gc.needs_write_barrier());
    buffer.emit_bool(!vm.flags.disable_tlab);
    buffer.emit_bool(cfg!(debug_assertions));
    buffer.emit_bool(has_lse_atomics());
    buffer.emit_bool(has_avx2());
}

#[cfg(target_arch = "aarch64")]
fn has_lse_atomics() -> bool {
    crate::cpu::has_lse_atomics()
}

#[cfg(not(target_arch = "aarch64"))]
fn has_lse_atomics() -> bool {
    false
}

#[cfg(target_arch = "x86_64")]
fn has_avx2() -> bool {
    crate::cpu::has_avx2()
}

#[cfg(not(target_arch = "x86_64"))]
fn has_avx2() -> bool {
    false
}

pub fn allocate_encoded_compilation_info(
    vm: &VM,
    compilation_data: &CompilationData,
    mode: CompilationMode,
) -> Ref<Object> {
    let mut buffer = ByteBuffer::new();
    encode_compilation_info(vm, compilation_data, mode, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub(super) fn encode_compilation_info(
    vm: &VM,
    compilation_data: &CompilationData,
    mode: CompilationMode,
    buffer: &mut ByteBuffer,
) {
    encode_bytecode_function(vm, &compilation_data.bytecode_fct, buffer);
    buffer.emit_id(compilation_data.fct_id.0 as usize);
    encode_type_params(vm, &compilation_data.type_params, buffer);
    encode_bytecode_type(vm, &compilation_data.return_type, buffer);
    encode_optional_specialize_self(vm, &compilation_data.specialize_self, buffer);
    encode_location(&compilation_data.loc, buffer);
    buffer.emit_u8(mode as u8);
    buffer.emit_bool(compilation_data.emit_debug);
    buffer.emit_bool(compilation_data.emit_graph);
    buffer.emit_bool(compilation_data.emit_html);
    buffer.emit_bool(compilation_data.emit_code_comments);
}

pub fn encode_optional_specialize_self(
    vm: &VM,
    specialize_self: &Option<SpecializeSelf>,
    buffer: &mut ByteBuffer,
) {
    if let Some(ref specialize_self) = specialize_self {
        buffer.emit_bool(true);
        encode_specialize_self(vm, specialize_self, buffer);
    } else {
        buffer.emit_bool(false);
    }
}

pub fn encode_specialize_self(vm: &VM, specialize_self: &SpecializeSelf, buffer: &mut ByteBuffer) {
    buffer.emit_id(specialize_self.impl_id.0 as usize);
    buffer.emit_u32(specialize_self.container_type_params as u32);
    encode_bytecode_trait_type(vm, &specialize_self.trait_ty, buffer);
    encode_bytecode_type(vm, &specialize_self.extended_ty, buffer);
}

pub fn allocate_encoded_struct_data(vm: &VM, struct_: &StructData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    encode_struct_data(vm, struct_, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn encode_struct_data(vm: &VM, struct_: &StructData, buffer: &mut ByteBuffer) {
    let count = struct_.type_params.names.len() as u32;
    buffer.emit_u32(count);

    let types = struct_
        .fields
        .iter()
        .map(|f| f.ty.clone())
        .collect::<Vec<_>>();
    encode_bytecode_type_slice(vm, &types, buffer);
}

pub fn allocate_encoded_enum_data(vm: &VM, enum_: &EnumData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    encode_enum_data(vm, enum_, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn encode_enum_data(vm: &VM, enum_: &EnumData, buffer: &mut ByteBuffer) {
    let count = enum_.type_params.names.len() as u32;
    buffer.emit_u32(count);

    let variants = enum_.variants.len();
    buffer.emit_u32(variants as u32);

    for variant in &enum_.variants {
        encode_bytecode_type_slice(vm, &variant.arguments, buffer);
    }
}

pub fn allocate_encoded_function_inlining_info(vm: &VM, fct: &FunctionData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    buffer.emit_bool(fct.bytecode.is_some());
    buffer.emit_u32(
        fct.bytecode
            .as_ref()
            .map(|bc| bc.code().len() as u32)
            .unwrap_or(0),
    );
    buffer.emit_bool(fct.is_force_inline);
    buffer.emit_bool(fct.is_never_inline);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub fn encode_function_bytecode_data(vm: &VM, fct: &FunctionData, buffer: &mut ByteBuffer) {
    let (bc, specialize_self) = get_bytecode(vm, fct).expect("missing bytecode");
    encode_bytecode_function(vm, bc, buffer);
    encode_bytecode_type(vm, &fct.return_type, buffer);
    encode_optional_specialize_self(vm, &specialize_self, buffer);
}

fn encode_bytecode_function(vm: &VM, bytecode_fct: &BytecodeFunction, buffer: &mut ByteBuffer) {
    encode_bytecode_array(bytecode_fct, buffer);
    encode_constpool_array(vm, bytecode_fct, buffer);
    encode_registers_array(vm, bytecode_fct, buffer);
    encode_bytecode_locations(bytecode_fct, buffer);
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

fn encode_type_params(vm: &VM, type_params: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    encode_bytecode_type_array(vm, type_params, buffer);
}

pub fn encode_bytecode_type_array(vm: &VM, sta: &BytecodeTypeArray, buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(vm, &ty, buffer);
    }
}

fn encode_bytecode_type_slice(vm: &VM, sta: &[BytecodeType], buffer: &mut ByteBuffer) {
    buffer.emit_u32(sta.len() as u32);

    for ty in sta.iter() {
        encode_bytecode_type(vm, ty, buffer);
    }
}

pub fn encode_bytecode_type(vm: &VM, ty: &BytecodeType, buffer: &mut ByteBuffer) {
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
        BytecodeType::This => {
            buffer.emit_u8(BytecodeTypeKind::This as u8);
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
        BytecodeType::TraitObject(trait_id, ref source_type_array, assoc_types) => {
            buffer.emit_u8(BytecodeTypeKind::TraitObject as u8);
            buffer.emit_id(trait_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
            encode_bytecode_type_array(vm, assoc_types, buffer);
        }
        BytecodeType::Lambda(params, ret) => {
            buffer.emit_u8(BytecodeTypeKind::Lambda as u8);
            encode_bytecode_type_array(vm, params, buffer);
            encode_bytecode_type(vm, ret.as_ref(), buffer);
        }
        BytecodeType::GenericAssoc {
            type_param_id,
            trait_ty,
            assoc_id,
        } => {
            buffer.emit_u8(BytecodeTypeKind::GenericAssoc as u8);
            buffer.emit_u32(*type_param_id);
            encode_bytecode_trait_type(vm, trait_ty, buffer);
            buffer.emit_id(assoc_id.0 as usize);
        }
        BytecodeType::Assoc(assoc_id, assoc_type_params) => {
            buffer.emit_u8(BytecodeTypeKind::Assoc as u8);
            buffer.emit_u32(assoc_id.0);
            encode_bytecode_type_array(vm, assoc_type_params, buffer);
        }
        BytecodeType::TypeAlias(..) => {
            unreachable!()
        }
    }
}

fn encode_bytecode_trait_type(vm: &VM, trait_ty: &BytecodeTraitType, buffer: &mut ByteBuffer) {
    buffer.emit_u32(trait_ty.trait_id.0);
    encode_bytecode_type_array(vm, &trait_ty.type_params, buffer);
    buffer.emit_u32(trait_ty.bindings.len() as u32);

    for (alias_id, ty) in &trait_ty.bindings {
        buffer.emit_u32(alias_id.0);
        encode_bytecode_type(vm, ty, buffer);
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
            buffer.emit_u8(ConstPoolOpcode::String.into());
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
            buffer.emit_id(fct_id.0 as usize);
            encode_bytecode_type_array(vm, source_type_array, buffer);
        }
        &ConstPoolEntry::TraitObjectMethod(ref trait_object_ty, fct_id) => {
            buffer.emit_u8(ConstPoolOpcode::TraitObjectMethod.into());
            encode_bytecode_type(vm, trait_object_ty, buffer);
            buffer.emit_id(fct_id.0 as usize);
        }
        &ConstPoolEntry::Generic(tp_id, fct_id, ref trait_type_params, ref fct_type_params) => {
            buffer.emit_u8(ConstPoolOpcode::Generic.into());
            buffer.emit_id(tp_id as usize);
            buffer.emit_id(fct_id.0 as usize);
            encode_bytecode_type_array(vm, trait_type_params, buffer);
            encode_bytecode_type_array(vm, fct_type_params, buffer);
        }
        &ConstPoolEntry::GenericSelf(fct_id, ref trait_type_params, ref fct_type_params) => {
            buffer.emit_u8(ConstPoolOpcode::GenericSelf.into());
            buffer.emit_id(fct_id.0 as usize);
            encode_bytecode_type_array(vm, trait_type_params, buffer);
            encode_bytecode_type_array(vm, fct_type_params, buffer);
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
        &ConstPoolEntry::TraitObject {
            ref trait_ty,
            ref actual_object_ty,
        } => {
            buffer.emit_u8(ConstPoolOpcode::TraitObject.into());
            encode_bytecode_type(vm, trait_ty, buffer);
            encode_bytecode_type(vm, actual_object_ty, buffer);
        }
        &ConstPoolEntry::TupleElement(ref tuple_ty, element_idx) => {
            buffer.emit_u8(ConstPoolOpcode::TupleElement.into());
            encode_bytecode_type(vm, tuple_ty, buffer);
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

fn get_architecture() -> InstructionSet {
    if cfg!(target_arch = "x86_64") {
        InstructionSet::X64
    } else if cfg!(target_arch = "aarch64") {
        InstructionSet::Arm64
    } else {
        panic!("unsupported architecture")
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

    pub fn emit_address(&mut self, data: Address) {
        self.emit_u64(data.to_usize() as u64);
    }

    pub fn emit_id(&mut self, data: usize) {
        assert!(data <= i32::MAX as usize);
        self.emit_u32(data as u32);
    }
}
