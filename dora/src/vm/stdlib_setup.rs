use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::ty::SourceTypeArray;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::vm::{ClassInstance, ClassInstanceId, VM};
use crate::vtable::VTableBox;

pub(super) fn setup(vm: &mut VM) {
    let free_object: ClassInstanceId;
    let free_array: ClassInstanceId;
    let code_class_id: ClassInstanceId;

    {
        let mut class_defs = vm.class_defs.lock();
        let next = class_defs.len();

        free_object = next.into();
        free_array = (next + 1).into();
        code_class_id = (next + 2).into();

        class_defs.push(Arc::new(ClassInstance {
            id: free_object,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(Header::size()),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        }));

        class_defs.push(Arc::new(ClassInstance {
            id: free_array,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::FreeArray,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        }));

        class_defs.push(Arc::new(ClassInstance {
            id: code_class_id,
            cls_id: None,
            trait_object: None,
            type_params: SourceTypeArray::empty(),
            parent_id: None,
            size: InstanceSize::CodeObject,
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: RwLock::new(None),
        }));

        {
            let free_object_class_def = &class_defs[free_object.to_usize()];
            let clsptr = Arc::as_ptr(free_object_class_def);
            let vtable = VTableBox::new(clsptr, Header::size() as usize, 0, &[]);
            *free_object_class_def.vtable.write() = Some(vtable);
        }

        {
            let free_array_class_def = &class_defs[free_array.to_usize()];
            let clsptr = Arc::as_ptr(free_array_class_def);
            let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
            *free_array_class_def.vtable.write() = Some(vtable);
        }

        {
            let code_class_def = &class_defs[free_array.to_usize()];
            let clsptr = Arc::as_ptr(code_class_def);
            let vtable = VTableBox::new(clsptr, 0, mem::ptr_width_usize(), &[]);
            *code_class_def.vtable.write() = Some(vtable);
        }
    }

    vm.known.free_object_class_def = free_object;
    vm.known.free_array_class_def = free_array;
    vm.known.code_class_def = code_class_id;
}
