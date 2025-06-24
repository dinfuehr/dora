use crate::size::InstanceSize;
use crate::vm::{
    ShapeKind, VM, create_shape, create_shape_for_class, setup_builtin_natives, stdlib_lookup,
};

use dora_bytecode::{BytecodeType, BytecodeTypeArray};

pub(super) fn setup(vm: &mut VM) {
    stdlib_lookup::lookup(vm);
    create_special_classes(vm);
    setup_builtin_natives(vm);
}

fn create_special_classes(vm: &mut VM) {
    let filler_word_shape = create_shape(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerWord,
        Vec::new(),
        0,
    );
    vm.known.filler_word_shape = filler_word_shape;

    let filler_array_shape = create_shape(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerArray,
        Vec::new(),
        0,
    );
    vm.known.filler_array_shape = filler_array_shape;

    let free_space_shape = create_shape(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeSpace,
        Vec::new(),
        0,
    );
    vm.known.free_space_shape = free_space_shape;

    let code_shape = create_shape(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_shape = code_shape;

    let type_args = BytecodeTypeArray::one(BytecodeType::UInt8);
    let shape = create_shape_for_class(vm, vm.known.array_class_id(), &type_args);
    vm.known.byte_array_shape = shape;

    let type_args = BytecodeTypeArray::one(BytecodeType::Int32);
    let shape = create_shape_for_class(vm, vm.known.array_class_id(), &type_args);
    vm.known.int32_array_shape = shape;

    let shape = create_shape_for_class(vm, vm.known.string_class_id(), &BytecodeTypeArray::empty());
    vm.known.string_shape = shape;

    let shape = create_shape_for_class(vm, vm.known.thread_class_id(), &BytecodeTypeArray::empty());
    vm.known.thread_shape = shape;
}
