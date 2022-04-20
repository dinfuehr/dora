use parking_lot::RwLock;

use crate::language::ty::SourceTypeArray;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{create_class_instance_with_vtable, setup_stubs, ClassInstance, VM};

pub(super) fn setup(vm: &mut VM) {
    create_special_classes(vm);
    setup_stubs(vm);
}

fn create_special_classes(vm: &mut VM) {
    let free_object_class_id = create_class_instance_with_vtable(
        vm,
        ClassInstance {
            id: None,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(Header::size()),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        },
        0,
    );
    vm.known.free_object_class_instance = Some(free_object_class_id);

    let free_array_class_id = create_class_instance_with_vtable(
        vm,
        ClassInstance {
            id: None,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::FreeArray,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        },
        0,
    );
    vm.known.free_array_class_instance = Some(free_array_class_id);

    let code_class_id = create_class_instance_with_vtable(
        vm,
        ClassInstance {
            id: None,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::CodeObject,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        },
        0,
    );
    vm.known.code_class_instance = Some(code_class_id);
}
