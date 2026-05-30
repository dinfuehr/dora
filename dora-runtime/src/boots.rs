use std::cell::Cell;
use std::mem;
use std::ptr;

use dora_runtime_macros::dora_native;

use dora_bytecode::{
    BytecodeTraitType, ConstId, ConstPoolEntry, ConstPoolIdx, EnumId, FunctionId, FunctionKind,
    GlobalId, Program, StructId, TraitId, display_fct,
};

pub(crate) use crate::boots::deserializer::{
    ByteReader, decode_bytecode_type, decode_bytecode_type_array,
};
use crate::boots::deserializer::{
    decode_bytecode_trait_ty, decode_code_descriptor, decode_specialize_self,
};
use crate::boots::serializer::encode_compilation_info;
pub(crate) use crate::boots::serializer::{
    ByteBuffer, encode_bytecode_type, encode_bytecode_type_array,
};
use crate::compiler::CompilationData;
use crate::compiler::aot::AotCodegenContext;
use crate::gc::Address;
use crate::handle::{Handle, create_handle, handle_scope};
use crate::mirror::{Object, Ref, Str, UInt8Array, byte_array_from_buffer};
use crate::threads::current_thread;
use crate::vm::specialize_ty_in_program;
use crate::vm::{CodeDescriptor, get_vm, impls};

mod deserializer;
mod serializer;

thread_local! {
    static ACTIVE_AOT_CONTEXT: Cell<*const ()> = Cell::new(ptr::null());
}

pub(crate) struct ActiveAotCodegenContextGuard(*const ());

impl Drop for ActiveAotCodegenContextGuard {
    fn drop(&mut self) {
        ACTIVE_AOT_CONTEXT.with(|context| context.set(self.0));
    }
}

pub fn compile(
    compile_address: Address,
    dora_entry_trampoline_address: Address,
    compilation_data: CompilationData,
) -> CodeDescriptor {
    handle_scope(|| {
        let mut buffer = ByteBuffer::new();
        encode_compilation_info(&compilation_data, &mut buffer);

        let encoded_compilation_info: Handle<Object> =
            create_handle(byte_array_from_buffer(get_vm(), buffer.data()).cast());

        let tld_address = current_thread().tld_address();

        let compile_fct_ptr: extern "C" fn(Address, Address, Address) -> Ref<UInt8Array> =
            unsafe { mem::transmute(dora_entry_trampoline_address) };

        let machine_code = create_handle(compile_fct_ptr(
            tld_address,
            compile_address,
            encoded_compilation_info.direct_ptr(),
        ));
        let mut reader = ByteReader::new(handle_to_vec(machine_code));
        let code = decode_code_descriptor(&mut reader);
        assert!(!reader.has_more());
        code
    })
}

pub(crate) fn set_active_aot_context(
    context: &AotCodegenContext<'_>,
) -> ActiveAotCodegenContextGuard {
    let context = context as *const AotCodegenContext<'_> as *const ();
    let previous = ACTIVE_AOT_CONTEXT.with(|active_context| {
        let previous = active_context.get();
        active_context.set(context);
        previous
    });
    ActiveAotCodegenContextGuard(previous)
}

fn active_aot_context() -> &'static AotCodegenContext<'static> {
    let context = ACTIVE_AOT_CONTEXT.with(|context| context.get());
    assert!(!context.is_null(), "boots AOT context is not initialized");

    // The pointer is installed by AOT compilation while the boxed context is alive.
    unsafe { &*(context as *const AotCodegenContext<'static>) }
}

#[dora_native("interface::get_function_vtable_index_raw")]
extern "C" fn get_function_vtable_index(trait_id: u32, trait_fct_id: FunctionId) -> u32 {
    let aot_context = active_aot_context();
    let trait_id: TraitId = (trait_id as usize).into();
    let trait_ = aot_context.program().trait_(trait_id);
    trait_
        .virtual_methods
        .iter()
        .position(|m| *m == trait_fct_id)
        .expect("missing trait function")
        .try_into()
        .expect("overflow")
}

fn handle_to_vec(data: Handle<UInt8Array>) -> Vec<u8> {
    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    serialized_data
}

#[dora_native("interface::get_class_size_for_trait_object_raw")]
extern "C" fn get_class_size_for_trait_object_raw(data: Handle<UInt8Array>) -> i32 {
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let _trait_ty = decode_bytecode_type(&mut reader);
    let actual_object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    aot_context.layout().trait_object_size(actual_object_ty)
}

#[dora_native("interface::get_global_initializer_function_id_raw")]
extern "C" fn get_global_initializer_function_id(id: GlobalId) -> u32 {
    let aot_context = active_aot_context();

    aot_context
        .program()
        .global(id)
        .initial_value
        .expect("missing initializer")
        .index_as_u32()
}

#[dora_native("interface::has_global_initial_value_raw")]
extern "C" fn has_global_initial_value(id: GlobalId) -> bool {
    let aot_context = active_aot_context();
    aot_context.program().global(id).initial_value.is_some()
}

#[dora_native("interface::get_class_size_raw")]
extern "C" fn get_class_size(data: Handle<UInt8Array>) -> u32 {
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    aot_context
        .layout()
        .class_instance_size(cls_id, &type_params)
}

#[dora_native("interface::get_element_size_raw")]
extern "C" fn get_element_size_raw(data: Handle<UInt8Array>) -> u32 {
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    aot_context
        .layout()
        .class_element_size(cls_id, &type_params)
}

#[dora_native("interface::get_field_offset_raw")]
extern "C" fn get_field_offset(data: Handle<UInt8Array>) -> u32 {
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let field_id = reader.read_u32();
    assert!(!reader.has_more());

    aot_context
        .layout()
        .class_field_offset(cls_id, &type_params, field_id)
}

#[dora_native("interface::get_system_config_raw")]
extern "C" fn get_system_config_raw() -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    serializer::allocate_encoded_system_config(vm, aot_context)
}

#[dora_native("interface::get_string_by_const_pool_id_raw")]
extern "C" fn get_string_by_const_pool_id_raw(fct_id: u32, const_pool_id: u32) -> Ref<Str> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let value = const_pool_string(aot_context.program(), fct_id, const_pool_id);
    Str::from_buffer(vm, value.as_bytes())
}

fn const_pool_string(program: &Program, fct_id: u32, const_pool_id: u32) -> &str {
    let fct = &program.functions[fct_id as usize];
    let bytecode = fct.bytecode.as_ref().expect("function has no bytecode");
    let entry = bytecode.const_pool(ConstPoolIdx(const_pool_id));
    match entry {
        ConstPoolEntry::String(value) => value.as_str(),
        _ => panic!("const pool entry is not a string"),
    }
}

#[dora_native("interface::find_trait_impl_raw")]
extern "C" fn find_trait_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let program = aot_context.program();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let trait_fct_id = (reader.read_u32() as usize).into();
    let trait_type_params = decode_bytecode_type_array(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let trait_fct = program.fct(trait_fct_id);
    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };

    let trait_ty = BytecodeTraitType {
        trait_id,
        type_params: trait_type_params,
        bindings: Vec::new(),
    };
    let (callee_id, type_params) =
        impls::find_trait_impl_in_program(program, trait_fct_id, trait_ty, object_ty);

    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(callee_id.index_as_u32());
    serializer::encode_bytecode_type_array(&type_params, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::find_trait_ty_impl_raw")]
extern "C" fn find_trait_ty_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let program = aot_context.program();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let trait_ty = decode_bytecode_trait_ty(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let (impl_id, bindings) =
        impls::find_trait_ty_impl_in_program(program, trait_ty, object_ty).expect("impl not found");

    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(impl_id.index_as_u32());
    serializer::encode_bytecode_type_array(&bindings, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::get_assoc_type_in_impl_raw")]
extern "C" fn get_assoc_type_in_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let program = aot_context.program();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let impl_id = (reader.read_u32() as usize).into();
    let trait_alias_id = (reader.read_u32() as usize).into();
    assert!(!reader.has_more());

    let impl_ = program.impl_(impl_id);
    let impl_alias_id = impl_
        .trait_alias_map
        .iter()
        .find(|(current_trait_alias_id, _)| *current_trait_alias_id == trait_alias_id)
        .expect("missing alias")
        .1;
    let impl_alias_ty = program
        .alias(impl_alias_id)
        .ty
        .clone()
        .expect("missing type");

    let mut buffer = ByteBuffer::new();
    serializer::encode_bytecode_type(&impl_alias_ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::specialize_assoc_ty_raw")]
extern "C" fn specialize_assoc_ty_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let program = aot_context.program();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let specialize_self = decode_specialize_self(&mut reader);
    let ty = decode_bytecode_type(&mut reader);
    assert!(ty.is_assoc());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let ty = specialize_ty_in_program(program, specialize_self.as_ref(), ty, &type_params);

    let mut buffer = ByteBuffer::new();
    serializer::encode_bytecode_type(&ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::get_intrinsic_for_function_raw")]
extern "C" fn get_intrinsic_for_function_raw(id: u32) -> i32 {
    let aot_context = active_aot_context();
    let id: FunctionId = (id as usize).into();
    aot_context
        .intrinsic_for_function(id)
        .map(|intrinsic| intrinsic as u32 as i32)
        .unwrap_or(-1)
}

#[dora_native("interface::get_function_display_name_raw")]
extern "C" fn get_function_display_name_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let name = display_fct(aot_context.program(), id);

    Str::from_buffer(vm, name.as_bytes()).cast()
}

#[dora_native("interface::get_function_info_for_inlining_raw")]
extern "C" fn get_function_info_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let fct = aot_context.program().fct(id);

    serializer::allocate_encoded_function_inlining_info(vm, fct)
}

#[dora_native("interface::get_function_bytecode_data_for_inlining_raw")]
extern "C" fn get_function_bytecode_data_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    let program = aot_context.program();

    let fct = program.fct(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_function_bytecode_data(program, fct, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::get_struct_data_raw")]
extern "C" fn get_struct_data_raw(id: StructId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let struct_ = aot_context.program().struct_(id);
    serializer::allocate_encoded_struct_data(vm, &struct_)
}

#[dora_native("interface::get_enum_data_raw")]
extern "C" fn get_enum_data_raw(id: EnumId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let enum_ = aot_context.program().enum_(id);
    serializer::allocate_encoded_enum_data(vm, &enum_)
}

#[dora_native("interface::get_const_value_raw")]
extern "C" fn get_const_value_raw(id: ConstId) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let const_ = aot_context.program().const_(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_const_value(&const_.value, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::get_class_data_for_enum_variant_raw")]
extern "C" fn get_class_data_for_enum_variant_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let enum_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    assert!(!reader.has_more());

    let alloc_size = aot_context
        .layout()
        .enum_variant_size(enum_id, &type_params, variant_id);

    let mut buffer = ByteBuffer::new();
    buffer.emit_u64(0);
    buffer.emit_u32(alloc_size);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[dora_native("interface::get_field_offset_for_enum_variant_raw")]
extern "C" fn get_field_offset_for_enum_variant_raw(data: Handle<UInt8Array>) -> i32 {
    let aot_context = active_aot_context();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let enum_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    let field_id = reader.read_u32();
    assert!(!reader.has_more());

    aot_context
        .layout()
        .enum_variant_field_offset(enum_id, &type_params, variant_id, field_id)
}
