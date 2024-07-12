use crate::gc::Address;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance_with_vtable, setup_builtin_natives, stdlib_lookup, ClassInstanceId,
    ShapeKind, VM,
};
use crate::vtable::VTable;

pub(super) fn setup(vm: &mut VM) {
    stdlib_lookup::connect_native_functions_to_implementation(vm);
    stdlib_lookup::lookup_known_classes(vm);
    stdlib_lookup::lookup_known_functions(vm);
    create_special_classes(vm);
    setup_builtin_natives(vm);
}

fn create_special_classes(vm: &mut VM) {
    let free_word_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerWord,
        Vec::new(),
        0,
    );
    vm.known.filler_word_class_instance_id = Some(free_word_class_id);
    vm.known.filler_word_class_address = address_from_class_instance_id(vm, free_word_class_id);

    let filler_array_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FillerArray,
        Vec::new(),
        0,
    );
    vm.known.filler_array_class_instance_id = Some(filler_array_class_id);
    vm.known.filler_array_class_address = address_from_class_instance_id(vm, filler_array_class_id);

    let free_space_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeSpace,
        Vec::new(),
        0,
    );
    vm.known.free_space_class_instance_id = Some(free_space_class_id);
    vm.known.free_space_class_address = address_from_class_instance_id(vm, free_space_class_id);

    let code_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_class_instance_id = Some(code_class_id);
}

fn address_from_class_instance_id(vm: &VM, id: ClassInstanceId) -> Address {
    let cls = vm.class_instances.idx(id);
    let vtable = cls.vtable.read();
    let vtable: &VTable = vtable.as_ref().unwrap();

    Address::from_ptr(vtable)
}
