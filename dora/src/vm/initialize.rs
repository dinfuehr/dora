use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::ty::SourceTypeArray;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{create_class_instance_with_vtable, setup_stubs, ClassInstance, VM};
use crate::vtable::VTableBox;

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
        Header::size() as usize,
        0,
        Vec::new(),
    );

    let free_array_class_id = vm.class_instances.push(ClassInstance {
        id: None,
        cls_id: None,
        trait_object: None,
        type_params: SourceTypeArray::empty(),
        parent_id: None,
        size: InstanceSize::FreeArray,
        fields: Vec::new(),
        ref_fields: Vec::new(),
        vtable: RwLock::new(None),
    });

    let code_class_id = vm.class_instances.push(ClassInstance {
        id: None,
        cls_id: None,
        trait_object: None,
        type_params: SourceTypeArray::empty(),
        parent_id: None,
        size: InstanceSize::CodeObject,
        fields: Vec::new(),
        ref_fields: Vec::new(),
        vtable: RwLock::new(None),
    });

    {
        let free_array_class_instance = vm.class_instances.idx(free_array_class_id);
        let clsptr = Arc::as_ptr(&free_array_class_instance);
        let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
        *free_array_class_instance.vtable.write() = Some(vtable);
    }

    {
        let code_class_instance = vm.class_instances.idx(code_class_id);
        let clsptr = Arc::as_ptr(&code_class_instance);
        let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
        *code_class_instance.vtable.write() = Some(vtable);
    }

    vm.known.free_object_class_instance = Some(free_object_class_id);
    vm.known.free_array_class_instance = Some(free_array_class_id);
    vm.known.code_class_instance = Some(code_class_id);
}
