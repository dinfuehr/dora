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
    let (_, filler_word_vtable) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerWord,
        Vec::new(),
        0,
    );
    vm.known.filler_word_vtable = filler_word_vtable;

    let (_, filler_array_vtable) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerArray,
        Vec::new(),
        0,
    );
    vm.known.filler_array_vtable = filler_array_vtable;

    let (_, free_space_vtable) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeSpace,
        Vec::new(),
        0,
    );
    vm.known.free_space_vtable = free_space_vtable;

    let (_, code_vtable) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_vtable = code_vtable;

    let type_args = BytecodeTypeArray::one(BytecodeType::UInt8);
    let vtable = create_class_instance(vm, vm.known.array_class_id(), &type_args);
    vm.known.byte_array_vtable = vtable;

    let type_args = BytecodeTypeArray::one(BytecodeType::Int32);
    let vtable = create_class_instance(vm, vm.known.array_class_id(), &type_args);
    vm.known.int32_array_vtable = vtable;

    let vtable = create_class_instance(vm, vm.known.string_class_id(), &BytecodeTypeArray::empty());
    vm.known.string_vtable = vtable;

    let vtable = create_class_instance(vm, vm.known.thread_class_id(), &BytecodeTypeArray::empty());
    vm.known.thread_vtable = vtable;
}

fn address_from_class_instance_id(vm: &VM, id: ClassInstanceId) -> Address {
    let cls = vm.class_instances.idx(id);
    let vtable = cls.vtable();

    Address::from_ptr(vtable)
}
