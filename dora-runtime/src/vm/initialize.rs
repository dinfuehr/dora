use crate::gc::Address;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance, create_class_instance_with_vtable, setup_builtin_natives, stdlib_lookup,
    ClassInstanceId, ShapeKind, VM,
};

use dora_bytecode::{BytecodeType, BytecodeTypeArray};

pub(super) fn setup(vm: &mut VM) {
    stdlib_lookup::lookup(vm);
    create_special_classes(vm);
    setup_builtin_natives(vm);
}

fn create_special_classes(vm: &mut VM) {
    let (_, filler_word_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerWord,
        Vec::new(),
        0,
    );
    vm.known.filler_word_vtable = Address::from_ptr(filler_word_class_address);

    let (_, filler_array_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerArray,
        Vec::new(),
        0,
    );
    vm.known.filler_array_vtable = Address::from_ptr(filler_array_class_address);

    let (_, free_space_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeSpace,
        Vec::new(),
        0,
    );
    vm.known.free_space_vtable = Address::from_ptr(free_space_class_address);

    let (_, code_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_vtable = Address::from_ptr(code_class_address);

    let type_args = BytecodeTypeArray::one(BytecodeType::UInt8);
    let cls_id = create_class_instance(vm, vm.known.array_class_id(), &type_args);
    vm.known.byte_array_vtable = address_from_class_instance_id(vm, cls_id);

    let type_args = BytecodeTypeArray::one(BytecodeType::Int32);
    let cls_id = create_class_instance(vm, vm.known.array_class_id(), &type_args);
    vm.known.int32_array_vtable = address_from_class_instance_id(vm, cls_id);

    let cls_id = create_class_instance(vm, vm.known.string_class_id(), &BytecodeTypeArray::empty());
    vm.known.string_vtable = address_from_class_instance_id(vm, cls_id);

    let cls_id = create_class_instance(vm, vm.known.thread_class_id(), &BytecodeTypeArray::empty());
    vm.known.thread_vtable = address_from_class_instance_id(vm, cls_id);
}

fn address_from_class_instance_id(vm: &VM, id: ClassInstanceId) -> Address {
    let cls = vm.class_instances.idx(id);
    let vtable = cls.vtable();

    Address::from_ptr(vtable)
}
