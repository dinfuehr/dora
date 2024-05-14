use std::mem;
use std::ptr;

use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, EnumId, FunctionId, FunctionKind, GlobalId, TraitId,
};

use crate::boots::deserializer::{decode_code_descriptor, ByteReader};
use crate::boots::serializer::{allocate_encoded_compilation_info, ByteBuffer};
use crate::cannon::codegen::get_function_address as get_function_address_raw;
use crate::cannon::CompilationFlags;
use crate::compiler::codegen::CompilationData;
use crate::gc::Address;
use crate::handle::{create_handle, Handle};
use crate::object::{byte_array_from_buffer, Ref, Str, UInt8Array};
use crate::size::InstanceSize;
use crate::threads::current_thread;
use crate::vm::{
    create_class_instance, create_enum_instance, ensure_class_instance_for_enum_variant, get_vm,
    impls, CodeDescriptor, VM,
};

use self::deserializer::{decode_bytecode_type, decode_bytecode_type_array};

mod data;
mod deserializer;
mod serializer;

pub const BOOTS_NATIVE_FUNCTIONS: &[(&'static str, *const u8)] = &[
    (
        "boots::interface::getClassPointerRaw",
        get_class_pointer as *const u8,
    ),
    (
        "boots::interface::getClassSizeRaw",
        get_class_size as *const u8,
    ),
    (
        "boots::interface::getFieldOffsetRaw",
        get_field_offset as *const u8,
    ),
    (
        "boots::interface::getFunctionVtableIndexRaw",
        get_function_vtable_index as *const u8,
    ),
    (
        "boots::interface::hasGlobalInitialValueRaw",
        has_global_initial_value as *const u8,
    ),
    (
        "boots::interface::getGlobalValueAddressRaw",
        get_global_value_address as *const u8,
    ),
    (
        "boots::interface::getGlobalStateAddressRaw",
        get_global_state_address as *const u8,
    ),
    (
        "boots::interface::getGlobalInitializerFunctionIdRaw",
        get_global_initializer_function_id as *const u8,
    ),
    (
        "boots::interface::getSystemConfig",
        get_system_config as *const u8,
    ),
    (
        "boots::interface::getFunctionAddressRaw",
        get_function_address as *const u8,
    ),
    (
        "boots::interface::getClassPointerForLambdaRaw",
        get_class_pointer_for_lambda as *const u8,
    ),
    (
        "boots::interface::getClassPointerForTraitObjectRaw",
        get_class_pointer_for_trait_object_raw as *const u8,
    ),
    (
        "boots::interface::getClassSizeForTraitObjectRaw",
        get_class_size_for_trait_object_raw as *const u8,
    ),
    (
        "boots::interface::getReadOnlyStringAddressRaw",
        get_read_only_string_address_raw as *const u8,
    ),
    (
        "boots::interface::findTraitImplRaw",
        find_trait_impl_raw as *const u8,
    ),
    (
        "boots::interface::getIntrinsicForFunctionRaw",
        get_intrinsic_for_function_raw as *const u8,
    ),
    (
        "boots::interface::getStructDataRaw",
        get_struct_data_raw as *const u8,
    ),
    (
        "boots::interface::getEnumDataRaw",
        get_enum_data_raw as *const u8,
    ),
    (
        "boots::interface::getClassDataForEnumVariantRaw",
        get_class_data_for_enum_variant_raw as *const u8,
    ),
    (
        "boots::interface::getFieldOffsetForEnumVariantRaw",
        get_field_offset_for_enum_variant_raw as *const u8,
    ),
    (
        "boots::interface::getElementSizeRaw",
        get_element_size_raw as *const u8,
    ),
];

pub fn compile(
    vm: &VM,
    compilation_data: CompilationData,
    _flags: CompilationFlags,
) -> CodeDescriptor {
    let compile_fct_id = vm.known.boots_compile_fct_id();
    let compile_address = vm.ensure_compiled(compile_fct_id);

    let encoded_compilation_info =
        create_handle(allocate_encoded_compilation_info(vm, &compilation_data));

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
    let fct_id = FunctionId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_function_address_raw(vm, fct_id, type_params)
}

extern "C" fn get_function_vtable_index(fct_id: u32) -> u32 {
    let vm = get_vm();
    vm.program.functions[fct_id as usize]
        .vtable_index
        .expect("vtable_index missing")
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
    let fct_id = FunctionId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let id = crate::vm::ensure_class_instance_for_lambda(vm, fct_id, type_params);
    let cls = vm.class_instances.idx(id);
    cls.vtblptr()
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
    let trait_id = TraitId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let id =
        crate::vm::ensure_class_instance_for_trait_object(vm, trait_id, &type_params, object_ty);
    let cls = vm.class_instances.idx(id);
    cls.vtblptr()
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
    let trait_id = TraitId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let id =
        crate::vm::ensure_class_instance_for_trait_object(vm, trait_id, &type_params, object_ty);
    let cls = vm.class_instances.idx(id);

    match cls.size {
        InstanceSize::Fixed(size) => size,
        _ => unreachable!(),
    }
}

extern "C" fn get_global_value_address(id: u32) -> Address {
    let vm = get_vm();

    let global_id = GlobalId(id);

    vm.global_variable_memory
        .as_ref()
        .unwrap()
        .address_value(global_id)
}

extern "C" fn get_global_state_address(id: u32) -> Address {
    let vm = get_vm();

    let global_id = GlobalId(id);

    vm.global_variable_memory
        .as_ref()
        .unwrap()
        .address_init(global_id)
}

extern "C" fn get_global_initializer_function_id(id: u32) -> u32 {
    let vm = get_vm();

    vm.program.globals[id as usize]
        .initial_value
        .expect("missing initializer")
        .0
}

extern "C" fn has_global_initial_value(id: u32) -> bool {
    let vm = get_vm();

    vm.program.globals[id as usize].initial_value.is_some()
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
    let cls_id = ClassId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_class_size_raw(vm, cls_id, type_params)
}

fn get_class_size_raw(vm: &VM, cls_id: ClassId, type_params: BytecodeTypeArray) -> u32 {
    let class_instance_id = create_class_instance(vm, cls_id, &type_params);
    let cls = vm.class_instances.idx(class_instance_id);

    match cls.size {
        InstanceSize::Fixed(size) => size as u32,
        _ => unreachable!(),
    }
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
    let cls_id = ClassId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    let class_instance_id = create_class_instance(vm, cls_id, &type_params);
    let cls = vm.class_instances.idx(class_instance_id);

    match cls.size {
        InstanceSize::StructArray(element_size) => element_size as u32,
        InstanceSize::ObjArray => crate::mem::ptr_width() as u32,
        InstanceSize::PrimitiveArray(element_size) => element_size as u32,
        InstanceSize::UnitArray => 0,
        _ => unreachable!(),
    }
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
    let cls_id = ClassId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    assert!(!reader.has_more());

    get_class_pointer_raw(vm, cls_id, type_params)
}

fn get_class_pointer_raw(vm: &VM, cls_id: ClassId, type_params: BytecodeTypeArray) -> Address {
    let class_instance_id = create_class_instance(vm, cls_id, &type_params);
    let cls = vm.class_instances.idx(class_instance_id);
    cls.vtblptr()
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
    let cls_id = ClassId(reader.read_u32());
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
    let class_instance_id = create_class_instance(vm, cls_id, &type_params);
    let cls = vm.class_instances.idx(class_instance_id);
    let field = &cls.fields[field_id as usize];
    field.offset.try_into().expect("overflow")
}

extern "C" fn get_system_config() -> Ref<UInt8Array> {
    let vm = get_vm();
    serializer::allocate_encoded_system_config(vm)
}

extern "C" fn get_read_only_string_address_raw(data: Handle<Str>) -> Address {
    let vm = get_vm();
    let content = data.content();
    let content = content.to_vec();

    let handle = Str::from_buffer_in_perm(vm, &content);
    handle.address()
}

extern "C" fn find_trait_impl_raw(data: Handle<UInt8Array>) -> u32 {
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
    let trait_fct_id = FunctionId(reader.read_u32());
    let trait_type_params = decode_bytecode_type_array(&mut reader);
    let object_ty = decode_bytecode_type(&mut reader);
    assert!(!reader.has_more());

    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];
    let trait_id = match trait_fct.kind {
        FunctionKind::Trait(trait_id) => trait_id,
        _ => unreachable!(),
    };

    let trait_ty = BytecodeType::Trait(trait_id, trait_type_params);
    let callee_id = impls::find_trait_impl(vm, trait_fct_id, trait_ty, object_ty);

    callee_id.0
}

extern "C" fn get_intrinsic_for_function_raw(id: u32) -> i32 {
    let vm = get_vm();
    let fct = &vm.program.functions[id as usize];
    fct.intrinsic.map(|i| i as u32 as i32).unwrap_or(-1)
}

extern "C" fn get_struct_data_raw(id: u32) -> Ref<UInt8Array> {
    let vm = get_vm();

    let struct_ = &vm.program.structs[id as usize];
    serializer::allocate_encoded_struct_data(vm, &struct_)
}

extern "C" fn get_enum_data_raw(id: u32) -> Ref<UInt8Array> {
    let vm = get_vm();

    let enum_ = &vm.program.enums[id as usize];
    serializer::allocate_encoded_enum_data(vm, &enum_)
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
    let enum_id = EnumId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    assert!(!reader.has_more());

    let enum_ = &vm.program.enums[enum_id.0 as usize];

    let enum_instance_id = create_enum_instance(vm, enum_id, type_params.clone());
    let enum_instance = vm.enum_instances.idx(enum_instance_id);

    let cls_def_id =
        ensure_class_instance_for_enum_variant(vm, &*enum_instance, &*enum_, variant_id);

    let cls = vm.class_instances.idx(cls_def_id);

    let alloc_size = match cls.size {
        InstanceSize::Fixed(size) => size as usize,
        _ => unreachable!(
            "class size type {:?} for new object not supported",
            cls.size
        ),
    };

    let mut buffer = ByteBuffer::new();
    buffer.emit_address(cls.vtblptr());
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
    let enum_id = EnumId(reader.read_u32());
    let type_params = decode_bytecode_type_array(&mut reader);
    let variant_id = reader.read_u32();
    let field_id = reader.read_u32();
    assert!(!reader.has_more());

    let enum_ = &vm.program.enums[enum_id.0 as usize];

    let enum_instance_id = create_enum_instance(vm, enum_id, type_params.clone());
    let enum_instance = vm.enum_instances.idx(enum_instance_id);

    let cls_def_id =
        ensure_class_instance_for_enum_variant(vm, &*enum_instance, &*enum_, variant_id);

    let field_id = enum_instance.field_id(&*enum_, variant_id, field_id);

    let cls = vm.class_instances.idx(cls_def_id);
    cls.fields[field_id as usize].offset
}
