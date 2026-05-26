use std::cell::Cell;
use std::mem;
use std::ptr;

use dora_bytecode::{
    BytecodeTraitType, BytecodeTypeArray, ClassId, ConstId, ConstPoolEntry, ConstPoolIdx, EnumId,
    FunctionId, FunctionKind, GlobalId, StructId, TraitId, display_fct,
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
use crate::vm::compute_vtable_index;
use crate::vm::specialize_ty;
use crate::vm::{CodeDescriptor, FctImplementation, VM, create_enum_instance, get_vm, impls};

mod deserializer;
mod serializer;

use FctImplementation::Native as N;

thread_local! {
    static ACTIVE_AOT_CONTEXT: Cell<*const ()> = Cell::new(ptr::null());
}

pub(crate) struct ActiveAotCodegenContextGuard(*const ());

impl Drop for ActiveAotCodegenContextGuard {
    fn drop(&mut self) {
        ACTIVE_AOT_CONTEXT.with(|context| context.set(self.0));
    }
}

pub const BOOTS_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    (
        "boots::interface::get_class_size_raw",
        N(get_class_size as *const u8, "dora_boots_get_class_size"),
    ),
    (
        "boots::interface::get_field_offset_raw",
        N(get_field_offset as *const u8, "dora_boots_get_field_offset"),
    ),
    (
        "boots::interface::get_function_vtable_index_raw",
        N(
            get_function_vtable_index as *const u8,
            "dora_boots_get_function_vtable_index",
        ),
    ),
    (
        "boots::interface::has_global_initial_value_raw",
        N(
            has_global_initial_value as *const u8,
            "dora_boots_has_global_initial_value",
        ),
    ),
    (
        "boots::interface::get_global_initializer_function_id_raw",
        N(
            get_global_initializer_function_id as *const u8,
            "dora_boots_get_global_initializer_function_id",
        ),
    ),
    (
        "boots::interface::get_system_config_raw",
        N(
            get_system_config_raw as *const u8,
            "dora_boots_get_system_config_raw",
        ),
    ),
    (
        "boots::interface::get_class_size_for_trait_object_raw",
        N(
            get_class_size_for_trait_object_raw as *const u8,
            "dora_boots_get_class_size_for_trait_object_raw",
        ),
    ),
    (
        "boots::interface::get_string_by_const_pool_id_raw",
        N(
            get_string_by_const_pool_id_raw as *const u8,
            "dora_boots_get_string_by_const_pool_id_raw",
        ),
    ),
    (
        "boots::interface::find_trait_impl_raw",
        N(
            find_trait_impl_raw as *const u8,
            "dora_boots_find_trait_impl_raw",
        ),
    ),
    (
        "boots::interface::find_trait_ty_impl_raw",
        N(
            find_trait_ty_impl_raw as *const u8,
            "dora_boots_find_trait_ty_impl_raw",
        ),
    ),
    (
        "boots::interface::get_assoc_type_in_impl_raw",
        N(
            get_assoc_type_in_impl_raw as *const u8,
            "dora_boots_get_assoc_type_in_impl_raw",
        ),
    ),
    (
        "boots::interface::specialize_assoc_ty_raw",
        N(
            specialize_assoc_ty_raw as *const u8,
            "dora_boots_specialize_assoc_ty_raw",
        ),
    ),
    (
        "boots::interface::get_intrinsic_for_function_raw",
        N(
            get_intrinsic_for_function_raw as *const u8,
            "dora_boots_get_intrinsic_for_function_raw",
        ),
    ),
    (
        "boots::interface::get_struct_data_raw",
        N(
            get_struct_data_raw as *const u8,
            "dora_boots_get_struct_data_raw",
        ),
    ),
    (
        "boots::interface::get_enum_data_raw",
        N(
            get_enum_data_raw as *const u8,
            "dora_boots_get_enum_data_raw",
        ),
    ),
    (
        "boots::interface::get_class_data_for_enum_variant_raw",
        N(
            get_class_data_for_enum_variant_raw as *const u8,
            "dora_boots_get_class_data_for_enum_variant_raw",
        ),
    ),
    (
        "boots::interface::get_field_offset_for_enum_variant_raw",
        N(
            get_field_offset_for_enum_variant_raw as *const u8,
            "dora_boots_get_field_offset_for_enum_variant_raw",
        ),
    ),
    (
        "boots::interface::get_element_size_raw",
        N(
            get_element_size_raw as *const u8,
            "dora_boots_get_element_size_raw",
        ),
    ),
    (
        "boots::interface::get_function_display_name_raw",
        N(
            get_function_display_name_raw as *const u8,
            "dora_boots_get_function_display_name_raw",
        ),
    ),
    (
        "boots::interface::get_function_info_for_inlining_raw",
        N(
            get_function_info_for_inlining_raw as *const u8,
            "dora_boots_get_function_info_for_inlining_raw",
        ),
    ),
    (
        "boots::interface::get_function_bytecode_data_for_inlining_raw",
        N(
            get_function_bytecode_data_for_inlining_raw as *const u8,
            "dora_boots_get_function_bytecode_data_for_inlining_raw",
        ),
    ),
    (
        "boots::interface::get_const_value_raw",
        N(
            get_const_value_raw as *const u8,
            "dora_boots_get_const_value_raw",
        ),
    ),
];

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

#[unsafe(export_name = "dora_boots_get_function_vtable_index")]
extern "C" fn get_function_vtable_index(trait_id: u32, trait_fct_id: FunctionId) -> u32 {
    let vm = get_vm();
    let trait_id: TraitId = (trait_id as usize).into();
    compute_vtable_index(vm, trait_id, trait_fct_id)
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

#[unsafe(export_name = "dora_boots_get_class_size_for_trait_object_raw")]
extern "C" fn get_class_size_for_trait_object_raw(data: Handle<UInt8Array>) -> i32 {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let trait_ty = decode_bytecode_type(&mut reader);
    let actual_object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let shape = vm.shape_for_trait_object(trait_ty, actual_object_ty);
    shape.instance_size() as i32
}

#[unsafe(export_name = "dora_boots_get_global_initializer_function_id")]
extern "C" fn get_global_initializer_function_id(id: GlobalId) -> u32 {
    let vm = get_vm();

    vm.global(id)
        .initial_value
        .expect("missing initializer")
        .index_as_u32()
}

#[unsafe(export_name = "dora_boots_has_global_initial_value")]
extern "C" fn has_global_initial_value(id: GlobalId) -> bool {
    let vm = get_vm();
    vm.global(id).initial_value.is_some()
}

#[unsafe(export_name = "dora_boots_get_class_size")]
extern "C" fn get_class_size(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_class_size_raw(vm, cls_id, type_params)
}

fn get_class_size_raw(vm: &VM, cls_id: ClassId, type_params: BytecodeTypeArray) -> u32 {
    let shape = vm.shape_for_class(cls_id, &type_params);
    shape.instance_size() as u32
}

#[unsafe(export_name = "dora_boots_get_element_size_raw")]
extern "C" fn get_element_size_raw(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let shape = vm.shape_for_class(cls_id, &type_params);
    shape.element_size() as u32
}

#[unsafe(export_name = "dora_boots_get_field_offset")]
extern "C" fn get_field_offset(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let field_id = reader.read_u32();
    assert!(!reader.has_more());

    get_field_offset_raw(vm, cls_id, type_params, field_id)
}

fn get_field_offset_raw(
    vm: &VM,
    cls_id: ClassId,
    type_params: BytecodeTypeArray,
    field_id: u32,
) -> u32 {
    let shape = vm.shape_for_class(cls_id, &type_params);
    let field = &shape.fields[field_id as usize];
    field.offset.try_into().expect("overflow")
}

#[unsafe(export_name = "dora_boots_get_system_config_raw")]
extern "C" fn get_system_config_raw() -> Ref<UInt8Array> {
    let vm = get_vm();
    let aot_context = active_aot_context();
    serializer::allocate_encoded_system_config(vm, aot_context)
}

#[unsafe(export_name = "dora_boots_get_string_by_const_pool_id_raw")]
extern "C" fn get_string_by_const_pool_id_raw(fct_id: u32, const_pool_id: u32) -> Ref<Str> {
    let vm = get_vm();
    let value = const_pool_string(vm, fct_id, const_pool_id);
    Str::from_buffer(vm, value.as_bytes())
}

fn const_pool_string(vm: &VM, fct_id: u32, const_pool_id: u32) -> &str {
    let fct = &vm.program.functions[fct_id as usize];
    let bytecode = fct.bytecode.as_ref().expect("function has no bytecode");
    let entry = bytecode.const_pool(ConstPoolIdx(const_pool_id));
    match entry {
        ConstPoolEntry::String(value) => value.as_str(),
        _ => panic!("const pool entry is not a string"),
    }
}

#[unsafe(export_name = "dora_boots_find_trait_impl_raw")]
extern "C" fn find_trait_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let trait_fct_id = (reader.read_u32() as usize).into();
    let trait_type_params = decode_bytecode_type_array(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let trait_fct = vm.fct(trait_fct_id);
    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };

    let trait_ty = BytecodeTraitType {
        trait_id,
        type_params: trait_type_params,
        bindings: Vec::new(),
    };
    let (callee_id, type_params) = impls::find_trait_impl(vm, trait_fct_id, trait_ty, object_ty);

    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(callee_id.index_as_u32());
    serializer::encode_bytecode_type_array(&type_params, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_find_trait_ty_impl_raw")]
extern "C" fn find_trait_ty_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let trait_ty = decode_bytecode_trait_ty(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let (impl_id, bindings) =
        impls::find_trait_ty_impl(vm, trait_ty, object_ty).expect("impl not found");

    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(impl_id.index_as_u32());
    serializer::encode_bytecode_type_array(&bindings, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_get_assoc_type_in_impl_raw")]
extern "C" fn get_assoc_type_in_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let impl_id = (reader.read_u32() as usize).into();
    let trait_alias_id = (reader.read_u32() as usize).into();
    assert!(!reader.has_more());

    let impl_ = vm.impl_(impl_id);
    let impl_alias_id = impl_
        .trait_alias_map
        .iter()
        .find(|(current_trait_alias_id, _)| *current_trait_alias_id == trait_alias_id)
        .expect("missing alias")
        .1;
    let impl_alias_ty = vm.alias(impl_alias_id).ty.clone().expect("missing type");

    let mut buffer = ByteBuffer::new();
    serializer::encode_bytecode_type(&impl_alias_ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_specialize_assoc_ty_raw")]
extern "C" fn specialize_assoc_ty_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let specialize_self = decode_specialize_self(&mut reader);
    let ty = decode_bytecode_type(&mut reader);
    assert!(ty.is_assoc());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let ty = specialize_ty(vm, specialize_self.as_ref(), ty, &type_params);

    let mut buffer = ByteBuffer::new();
    serializer::encode_bytecode_type(&ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_get_intrinsic_for_function_raw")]
extern "C" fn get_intrinsic_for_function_raw(id: u32) -> i32 {
    let vm = get_vm();
    let id: FunctionId = (id as usize).into();
    vm.intrinsics
        .get(&id)
        .map(|i| *i as u32 as i32)
        .unwrap_or(-1)
}

#[unsafe(export_name = "dora_boots_get_function_display_name_raw")]
extern "C" fn get_function_display_name_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let name = display_fct(&vm.program, id);

    Str::from_buffer(vm, name.as_bytes()).cast()
}

#[unsafe(export_name = "dora_boots_get_function_info_for_inlining_raw")]
extern "C" fn get_function_info_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let fct = vm.fct(id);

    serializer::allocate_encoded_function_inlining_info(vm, fct)
}

#[unsafe(export_name = "dora_boots_get_function_bytecode_data_for_inlining_raw")]
extern "C" fn get_function_bytecode_data_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let fct = vm.fct(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_function_bytecode_data(vm, fct, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_get_struct_data_raw")]
extern "C" fn get_struct_data_raw(id: StructId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let struct_ = vm.struct_(id);
    serializer::allocate_encoded_struct_data(vm, &struct_)
}

#[unsafe(export_name = "dora_boots_get_enum_data_raw")]
extern "C" fn get_enum_data_raw(id: EnumId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let enum_ = vm.enum_(id);
    serializer::allocate_encoded_enum_data(vm, &enum_)
}

#[unsafe(export_name = "dora_boots_get_const_value_raw")]
extern "C" fn get_const_value_raw(id: ConstId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let const_ = vm.const_(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_const_value(&const_.value, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_get_class_data_for_enum_variant_raw")]
extern "C" fn get_class_data_for_enum_variant_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let enum_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    assert!(!reader.has_more());

    let enum_ = vm.enum_(enum_id);

    let enum_instance_id = create_enum_instance(vm, enum_id, type_params.clone());
    let enum_instance = vm.enum_instances.idx(enum_instance_id);

    let shape = vm.shape_for_enum_variant(&*enum_instance, &*enum_, variant_id);
    let alloc_size = shape.instance_size();

    let mut buffer = ByteBuffer::new();
    buffer.emit_u64(0);
    buffer.emit_u32(alloc_size as u32);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

#[unsafe(export_name = "dora_boots_get_field_offset_for_enum_variant_raw")]
extern "C" fn get_field_offset_for_enum_variant_raw(data: Handle<UInt8Array>) -> i32 {
    let vm = get_vm();

    let mut reader = ByteReader::new(handle_to_vec(data));
    let enum_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    let field_id = reader.read_u32();
    assert!(!reader.has_more());

    let enum_ = vm.enum_(enum_id);
    let enum_instance_id = create_enum_instance(vm, enum_id, type_params.clone());
    let enum_instance = vm.enum_instances.idx(enum_instance_id);

    let shape = vm.shape_for_enum_variant(&*enum_instance, &*enum_, variant_id);
    let field_id = enum_instance.field_id(&*enum_, variant_id, field_id);

    shape.fields[field_id as usize].offset
}
