use dora_bytecode::{EnumData, FunctionData, StructData, opcode as opc};
use dora_compiler::CompilationData;
pub use dora_compiler::boots_wire::{
    ByteBuffer, encode_bytecode_type, encode_bytecode_type_array, encode_const_value,
    encode_function_bytecode_data,
};

use crate::compiler::aot::AotCodegenContext;
use crate::mirror::{Object, Ref, UInt8Array, byte_array_from_buffer};
use crate::{Shape, VM};

pub fn allocate_encoded_system_config(
    vm: &VM,
    aot_context: &AotCodegenContext<'_>,
) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    encode_system_config(aot_context, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn encode_system_config(aot_context: &AotCodegenContext<'_>, buffer: &mut ByteBuffer) {
    let target = aot_context.target_arch();
    buffer.emit_u8(get_architecture(target));
    buffer.emit_u32(Shape::offset_of_vtable() as u32);
    buffer.emit_id(aot_context.array_class_id().index());
    buffer.emit_id(aot_context.string_class_id().index());
    buffer.emit_bool(cfg!(target_family = "windows"));
    buffer.emit_bool(cfg!(target_family = "unix"));
    buffer.emit_bool(aot_context.needs_write_barrier());
    buffer.emit_bool(cfg!(debug_assertions));
    buffer.emit_bool(target.is_arm64() && has_lse_atomics());
    buffer.emit_bool(!target.is_arm64() && has_avx2());
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
) -> Ref<Object> {
    let mut buffer = ByteBuffer::new();
    dora_compiler::boots_wire::encode_compilation_info(compilation_data, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub fn allocate_encoded_struct_data(vm: &VM, struct_: &StructData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    dora_compiler::boots_wire::encode_struct_data(struct_, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub fn allocate_encoded_enum_data(vm: &VM, enum_: &EnumData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    dora_compiler::boots_wire::encode_enum_data(enum_, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

pub fn allocate_encoded_function_inlining_info(vm: &VM, fct: &FunctionData) -> Ref<UInt8Array> {
    let mut buffer = ByteBuffer::new();
    dora_compiler::boots_wire::encode_function_inlining_info(fct, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

fn get_architecture(target: crate::vm::TargetArch) -> u8 {
    match target {
        crate::vm::TargetArch::X64 => opc::INSTRUCTION_SET_X64,
        crate::vm::TargetArch::Arm64 => opc::INSTRUCTION_SET_ARM64,
    }
}
