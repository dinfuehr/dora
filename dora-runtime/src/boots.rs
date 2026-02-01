use std::mem;
use std::ptr;

use dora_bytecode::{
    BytecodeTraitType, BytecodeTypeArray, ClassId, ConstId, EnumId, FunctionId, FunctionKind,
    GlobalId, StructId, TraitId, display_fct,
};

use crate::boots::deserializer::{
    ByteReader, decode_bytecode_trait_ty, decode_bytecode_type, decode_bytecode_type_array,
    decode_code_descriptor, decode_specialize_self,
};
use crate::boots::serializer::{ByteBuffer, encode_compilation_info};
use crate::cannon::codegen::get_function_address as get_function_address_raw;
use crate::compiler::{CompilationData, CompilationMode};
use crate::gc::Address;
use crate::handle::{Handle, create_handle, handle_scope};
use crate::mirror::{Object, Ref, Str, UInt8Array, byte_array_from_buffer};
use crate::threads::current_thread;
use crate::vm::compute_vtable_index;
use crate::vm::specialize_ty;
use crate::vm::{CodeDescriptor, FctImplementation, VM, create_enum_instance, get_vm, impls};

mod data;
mod deserializer;
mod serializer;

use FctImplementation::Native as N;

pub const BOOTS_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    (
        "boots::interface::getClassPointerRaw",
        N(get_class_pointer as *const u8),
    ),
    (
        "boots::interface::getClassSizeRaw",
        N(get_class_size as *const u8),
    ),
    (
        "boots::interface::getFieldOffsetRaw",
        N(get_field_offset as *const u8),
    ),
    (
        "boots::interface::getFunctionVtableIndexRaw",
        N(get_function_vtable_index as *const u8),
    ),
    (
        "boots::interface::hasGlobalInitialValueRaw",
        N(has_global_initial_value as *const u8),
    ),
    (
        "boots::interface::getGlobalValueAddressRaw",
        N(get_global_value_address as *const u8),
    ),
    (
        "boots::interface::getGlobalStateAddressRaw",
        N(get_global_state_address as *const u8),
    ),
    (
        "boots::interface::getGlobalInitializerFunctionIdRaw",
        N(get_global_initializer_function_id as *const u8),
    ),
    (
        "boots::interface::getSystemConfigRaw",
        N(get_system_config_raw as *const u8),
    ),
    (
        "boots::interface::getFunctionAddressRaw",
        N(get_function_address as *const u8),
    ),
    (
        "boots::interface::getClassPointerForLambdaRaw",
        N(get_class_pointer_for_lambda as *const u8),
    ),
    (
        "boots::interface::getClassPointerForTraitObjectRaw",
        N(get_class_pointer_for_trait_object_raw as *const u8),
    ),
    (
        "boots::interface::getClassSizeForTraitObjectRaw",
        N(get_class_size_for_trait_object_raw as *const u8),
    ),
    (
        "boots::interface::getReadOnlyStringAddressRaw",
        N(get_read_only_string_address_raw as *const u8),
    ),
    (
        "boots::interface::findTraitImplRaw",
        N(find_trait_impl_raw as *const u8),
    ),
    (
        "boots::interface::findTraitTyImplRaw",
        N(find_trait_ty_impl_raw as *const u8),
    ),
    (
        "boots::interface::getAssocTypeInImplRaw",
        N(get_assoc_type_in_impl_raw as *const u8),
    ),
    (
        "boots::interface::specializeAssocTyRaw",
        N(specialize_assoc_ty_raw as *const u8),
    ),
    (
        "boots::interface::getIntrinsicForFunctionRaw",
        N(get_intrinsic_for_function_raw as *const u8),
    ),
    (
        "boots::interface::getStructDataRaw",
        N(get_struct_data_raw as *const u8),
    ),
    (
        "boots::interface::getEnumDataRaw",
        N(get_enum_data_raw as *const u8),
    ),
    (
        "boots::interface::getClassDataForEnumVariantRaw",
        N(get_class_data_for_enum_variant_raw as *const u8),
    ),
    (
        "boots::interface::getFieldOffsetForEnumVariantRaw",
        N(get_field_offset_for_enum_variant_raw as *const u8),
    ),
    (
        "boots::interface::getElementSizeRaw",
        N(get_element_size_raw as *const u8),
    ),
    (
        "boots::interface::getFunctionDisplayNameRaw",
        N(get_function_display_name_raw as *const u8),
    ),
    (
        "boots::interface::getFunctionInfoForInliningRaw",
        N(get_function_info_for_inlining_raw as *const u8),
    ),
    (
        "boots::interface::getFunctionBytecodeDataForInliningRaw",
        N(get_function_bytecode_data_for_inlining_raw as *const u8),
    ),
    (
        "boots::interface::getConstValueRaw",
        N(get_const_value_raw as *const u8),
    ),
];

pub fn compile(
    vm: &VM,
    compile_address: Address,
    compilation_data: CompilationData,
    mode: CompilationMode,
) -> CodeDescriptor {
    handle_scope(|| {
        let mut buffer = ByteBuffer::new();
        encode_compilation_info(vm, &compilation_data, mode, &mut buffer);

        let encoded_compilation_info: Handle<Object> =
            create_handle(byte_array_from_buffer(vm, buffer.data()).cast());

        let tld_address = current_thread().tld_address();

        let dora_stub_address = vm.native_methods.dora_entry_trampoline();
        let compile_fct_ptr: extern "C" fn(Address, Address, Address) -> Ref<UInt8Array> =
            unsafe { mem::transmute(dora_stub_address) };

        let machine_code = create_handle(compile_fct_ptr(
            tld_address,
            compile_address,
            encoded_compilation_info.direct_ptr(),
        ));
        let mut serialized_data = vec![0; machine_code.len()];

        unsafe {
            ptr::copy_nonoverlapping(
                machine_code.data() as *mut u8,
                serialized_data.as_mut_ptr(),
                machine_code.len(),
            );
        }

        let mut reader = ByteReader::new(serialized_data);
        let code = decode_code_descriptor(&mut reader);
        assert!(!reader.has_more());
        code
    })
}

extern "C" fn get_function_address(data: Handle<UInt8Array>) -> Address {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let fct_id: FunctionId = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_function_address_raw(vm, fct_id, type_params)
}

extern "C" fn get_function_vtable_index(trait_id: u32, trait_fct_id: FunctionId) -> u32 {
    let vm = get_vm();
    let trait_id: TraitId = (trait_id as usize).into();
    compute_vtable_index(vm, trait_id, trait_fct_id)
}

extern "C" fn get_class_pointer_for_lambda(data: Handle<UInt8Array>) -> Address {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let fct_id: FunctionId = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    vm.shape_for_lambda(fct_id, type_params).address()
}

extern "C" fn get_class_pointer_for_trait_object_raw(data: Handle<UInt8Array>) -> Address {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let trait_ty = decode_bytecode_type(&mut reader);
    let actual_object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    vm.shape_for_trait_object(trait_ty, actual_object_ty)
        .address()
}

extern "C" fn get_class_size_for_trait_object_raw(data: Handle<UInt8Array>) -> i32 {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let trait_ty = decode_bytecode_type(&mut reader);
    let actual_object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let shape = vm.shape_for_trait_object(trait_ty, actual_object_ty);
    shape.instance_size() as i32
}

extern "C" fn get_global_value_address(id: GlobalId) -> Address {
    let vm = get_vm();

    vm.global_variable_memory
        .as_ref()
        .unwrap()
        .address_value(id)
}

extern "C" fn get_global_state_address(id: GlobalId) -> Address {
    let vm = get_vm();

    vm.global_variable_memory.as_ref().unwrap().address_init(id)
}

extern "C" fn get_global_initializer_function_id(id: GlobalId) -> u32 {
    let vm = get_vm();

    vm.global(id)
        .initial_value
        .expect("missing initializer")
        .index_as_u32()
}

extern "C" fn has_global_initial_value(id: GlobalId) -> bool {
    let vm = get_vm();
    vm.global(id).initial_value.is_some()
}

extern "C" fn get_class_size(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_class_size_raw(vm, cls_id, type_params)
}

fn get_class_size_raw(vm: &VM, cls_id: ClassId, type_params: BytecodeTypeArray) -> u32 {
    let shape = vm.shape_for_class(cls_id, &type_params);
    shape.instance_size() as u32
}

fn get_element_size_raw(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let shape = vm.shape_for_class(cls_id, &type_params);
    shape.element_size() as u32
}

extern "C" fn get_class_pointer(data: Handle<UInt8Array>) -> Address {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let cls_id = (reader.read_u32() as usize).into();
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_class_pointer_raw(vm, cls_id, type_params)
}

fn get_class_pointer_raw(vm: &VM, cls_id: ClassId, type_params: BytecodeTypeArray) -> Address {
    vm.shape_for_class(cls_id, &type_params).address()
}

extern "C" fn get_field_offset(data: Handle<UInt8Array>) -> u32 {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
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

extern "C" fn get_system_config_raw() -> Ref<UInt8Array> {
    let vm = get_vm();
    serializer::allocate_encoded_system_config(vm)
}

extern "C" fn get_read_only_string_address_raw(data: Handle<Str>) -> Address {
    let vm = get_vm();

    vm.internalize_string_constant(data.content_utf8())
}

extern "C" fn find_trait_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
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
    serializer::encode_bytecode_type_array(vm, &type_params, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn find_trait_ty_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let trait_ty = decode_bytecode_trait_ty(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let (impl_id, bindings) =
        impls::find_trait_ty_impl(vm, trait_ty, object_ty).expect("impl not found");

    let mut buffer = ByteBuffer::new();
    buffer.emit_u32(impl_id.index_as_u32());
    serializer::encode_bytecode_type_array(vm, &bindings, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn get_assoc_type_in_impl_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
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
    serializer::encode_bytecode_type(vm, &impl_alias_ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn specialize_assoc_ty_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
    let specialize_self = decode_specialize_self(&mut reader);
    let ty = decode_bytecode_type(&mut reader);
    assert!(ty.is_assoc());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let ty = specialize_ty(vm, specialize_self.as_ref(), ty, &type_params);

    let mut buffer = ByteBuffer::new();
    serializer::encode_bytecode_type(vm, &ty, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn get_intrinsic_for_function_raw(id: u32) -> i32 {
    let vm = get_vm();
    let id: FunctionId = (id as usize).into();
    vm.intrinsics
        .get(&id)
        .map(|i| *i as u32 as i32)
        .unwrap_or(-1)
}

extern "C" fn get_function_display_name_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let name = display_fct(&vm.program, id);

    Str::from_buffer(vm, name.as_bytes()).cast()
}

extern "C" fn get_function_info_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let fct = vm.fct(id);

    serializer::allocate_encoded_function_inlining_info(vm, fct)
}

extern "C" fn get_function_bytecode_data_for_inlining_raw(id: FunctionId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let fct = vm.fct(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_function_bytecode_data(vm, fct, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn get_struct_data_raw(id: StructId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let struct_ = vm.struct_(id);
    serializer::allocate_encoded_struct_data(vm, &struct_)
}

extern "C" fn get_enum_data_raw(id: EnumId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let enum_ = vm.enum_(id);
    serializer::allocate_encoded_enum_data(vm, &enum_)
}

extern "C" fn get_const_value_raw(id: ConstId) -> Ref<UInt8Array> {
    let vm = get_vm();

    let const_ = vm.const_(id);

    let mut buffer = ByteBuffer::new();
    serializer::encode_const_value(&const_.value, &mut buffer);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn get_class_data_for_enum_variant_raw(data: Handle<UInt8Array>) -> Ref<UInt8Array> {
    let vm = get_vm();
    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
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
    buffer.emit_address(shape.address());
    buffer.emit_u32(alloc_size as u32);
    byte_array_from_buffer(vm, buffer.data()).cast()
}

extern "C" fn get_field_offset_for_enum_variant_raw(data: Handle<UInt8Array>) -> i32 {
    let vm = get_vm();

    let mut serialized_data = vec![0; data.len()];

    unsafe {
        ptr::copy_nonoverlapping(
            data.data() as *mut u8,
            serialized_data.as_mut_ptr(),
            data.len(),
        );
    }

    let mut reader = ByteReader::new(serialized_data);
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
