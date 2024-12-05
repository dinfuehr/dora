use crate::gc::Address;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance_with_vtable, setup_builtin_natives, stdlib_lookup, ClassInstanceId,
    ShapeKind, VM,
};

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
    vm.known.filler_word_class_address = Address::from_ptr(filler_word_class_address);

    let (_, filler_array_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerArray,
        Vec::new(),
        0,
    );
    vm.known.filler_array_class_address = Address::from_ptr(filler_array_class_address);

    let (_, free_space_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeSpace,
        Vec::new(),
        0,
    );
    vm.known.free_space_class_address = Address::from_ptr(free_space_class_address);

    let (_, code_class_address) = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_class_address = Address::from_ptr(code_class_address);
}

fn address_from_class_instance_id(vm: &VM, id: ClassInstanceId) -> Address {
    let cls = vm.class_instances.idx(id);
    let vtable = cls.vtable();

    Address::from_ptr(vtable)
}
