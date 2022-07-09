use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{create_class_instance_with_vtable, setup_stubs, ShapeKind, VM};

pub(super) fn setup(vm: &mut VM) {
    create_special_classes(vm);
    setup_stubs(vm);
}

fn create_special_classes(vm: &mut VM) {
    let free_object_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::Fixed(Header::size()),
        Vec::new(),
        0,
    );
    vm.known.free_object_class_instance = Some(free_object_class_id);

    let free_array_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::FreeArray,
        Vec::new(),
        0,
    );
    vm.known.free_array_class_instance = Some(free_array_class_id);

    let code_class_id = create_class_instance_with_vtable(
        vm,
        ShapeKind::Builtin,
        InstanceSize::CodeObject,
        Vec::new(),
        0,
    );
    vm.known.code_class_instance = Some(code_class_id);
}
