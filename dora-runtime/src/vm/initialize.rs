use crate::gc::Address;
use crate::size::InstanceSize;
use crate::vm::{
    create_class_instance_with_vtable, setup_builtin_natives, stdlib, ClassInstanceId, ShapeKind,
    VM,
};
use crate::vtable::VTable;

pub(super) fn setup(vm: &mut VM) {
    stdlib::connect_native_functions_to_implementation(vm);
    stdlib::lookup_known_classes(vm);
    stdlib::lookup_known_functions(vm);
    create_special_classes(vm);
    setup_builtin_natives(vm);
}

fn create_special_classes(vm: &mut VM) {
    let free_word_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeWord,
        Vec::new(),
        0,
    );
    vm.known.free_word_class_instance_id = Some(free_word_class_id);
    vm.known.free_word_class_address = address_from_class_instance_id(vm, free_word_class_id);

    let free_object_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeObject,
        Vec::new(),
        0,
    );
    vm.known.free_object_class_instance_id = Some(free_object_class_id);
    vm.known.free_object_class_address = address_from_class_instance_id(vm, free_object_class_id);

    let free_array_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeArray,
        Vec::new(),
        0,
    );
    vm.known.free_array_class_instance_id = Some(free_array_class_id);
    vm.known.free_array_class_address = address_from_class_instance_id(vm, free_array_class_id);

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
