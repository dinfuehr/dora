use parking_lot::{Mutex, RwLock};
use std::cmp::max;
use std::ptr;
use std::sync::Arc;

use crate::field::FieldDef;
use crate::mem;
use crate::object::Header;
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{
    CallType, Class, ClassDef, ClassDefId, ClassId, StructData, StructDef, StructDefId,
    StructFieldDef, StructId, VM,
};
use crate::vtable::{VTableBox, DISPLAY_SIZE};

pub fn specialize_type(
    vm: &VM,
    ty: BuiltinType,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, id) => cls_type_params[id.idx()],

        BuiltinType::FctTypeParam(_, id) => fct_type_params[id.idx()],

        BuiltinType::Struct(struct_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params = TypeList::with(
                params
                    .iter()
                    .map(|t| specialize_type(vm, t, cls_type_params, fct_type_params))
                    .collect::<Vec<_>>(),
            );

            let list_id = vm.lists.lock().insert(params);

            BuiltinType::Struct(struct_id, list_id)
        }

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params = TypeList::with(
                params
                    .iter()
                    .map(|t| specialize_type(vm, t, cls_type_params, fct_type_params))
                    .collect(),
            );

            let list_id = vm.lists.lock().insert(params);

            BuiltinType::Class(cls_id, list_id)
        }

        BuiltinType::Lambda(_) => unimplemented!(),

        _ => ty,
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SpecializeFor {
    Fct,
    Class,
}

pub fn specialize_struct_id(vm: &VM, struct_id: StructId) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.lock();
    specialize_struct(vm, &*struc, TypeList::empty())
}

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructId,
    type_params: TypeList,
) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.lock();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(vm: &VM, struc: &StructData, type_params: TypeList) -> StructDefId {
    if let Some(&id) = struc.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_struct(vm, struc, type_params)
}

fn create_specialized_struct(vm: &VM, struc: &StructData, type_params: TypeList) -> StructDefId {
    let id = {
        let mut struct_defs = vm.struct_defs.lock();
        let id: StructDefId = struct_defs.len().into();

        let old = struc
            .specializations
            .write()
            .insert(type_params.clone(), id);
        assert!(old.is_none());

        struct_defs.push(Arc::new(Mutex::new(StructDef {
            size: 0,
            align: 0,
            fields: Vec::new(),
            ref_fields: Vec::new(),
        })));

        id
    };

    let mut size = 0;
    let mut align = 0;
    let mut fields = Vec::with_capacity(struc.fields.len());
    let mut ref_fields = Vec::new();

    for f in &struc.fields {
        let ty = specialize_type(vm, f.ty, &type_params, &TypeList::empty());
        debug_assert!(!ty.contains_type_param(vm));

        let field_size = ty.size(vm);
        let field_align = ty.align(vm);

        let offset = mem::align_i32(size, field_align);
        fields.push(StructFieldDef { offset, ty });

        size = offset + field_size;
        align = max(align, field_align);

        if ty.reference_type() {
            ref_fields.push(offset);
        }
    }

    let struct_def = vm.struct_defs.idx(id);
    let mut struct_def = struct_def.lock();
    struct_def.size = size;
    struct_def.align = align;
    struct_def.fields = fields;
    struct_def.ref_fields = ref_fields;

    id
}

pub fn specialize_class_id(vm: &VM, cls_id: ClassId) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &TypeList::empty())
}

pub fn specialize_class_id_params(vm: &VM, cls_id: ClassId, type_params: &TypeList) -> ClassDefId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();
    specialize_class(vm, &*cls, &type_params)
}

pub fn specialize_class_ty(vm: &VM, ty: BuiltinType) -> ClassDefId {
    match ty {
        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);
            specialize_class_id_params(vm, cls_id, &params)
        }

        _ => unreachable!(),
    }
}

pub fn specialize_class(vm: &VM, cls: &Class, type_params: &TypeList) -> ClassDefId {
    if let Some(&id) = cls.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(vm: &VM, cls: &Class, type_params: &TypeList) -> ClassDefId {
    let id = {
        let mut class_defs = vm.class_defs.lock();
        let id: ClassDefId = class_defs.len().into();

        let old = cls.specializations.write().insert(type_params.clone(), id);
        assert!(old.is_none());

        class_defs.push(Arc::new(RwLock::new(ClassDef {
            id,
            cls_id: Some(cls.id),
            type_params: type_params.clone(),
            parent_id: None,
            size: InstanceSize::Fixed(0),
            fields: Vec::new(),
            ref_fields: Vec::new(),
            vtable: None,
        })));

        id
    };

    let mut fields;
    let mut ref_fields;
    let size;
    let parent_id;

    if cls.is_array || cls.is_str {
        fields = Vec::new();
        ref_fields = Vec::new();

        size = if cls.is_array {
            if type_params[0].reference_type() {
                InstanceSize::ObjArray
            } else {
                InstanceSize::Array(type_params[0].size(vm))
            }
        } else {
            InstanceSize::Str
        };

        let parent_class = cls
            .parent_class
            .expect("Array & String should have super class");
        let id = specialize_class_ty(vm, parent_class);
        parent_id = Some(id);
    } else {
        let mut csize;

        if let Some(parent_class) = cls.parent_class {
            let id = specialize_class_ty(vm, parent_class);
            let cls_def = vm.class_defs.idx(id);
            let cls_def = cls_def.read();

            fields = Vec::new();
            ref_fields = cls_def.ref_fields.clone();
            csize = match cls_def.size {
                InstanceSize::Fixed(size) => size,
                _ => unreachable!(),
            };
            parent_id = Some(id);
        } else {
            fields = Vec::with_capacity(cls.fields.len());
            ref_fields = Vec::new();
            csize = Header::size();
            parent_id = None;
        };

        for f in &cls.fields {
            let ty = specialize_type(vm, f.ty, &type_params, &TypeList::empty());
            debug_assert!(!ty.contains_type_param(vm));

            let field_size = ty.size(vm);
            let field_align = ty.align(vm);

            let offset = mem::align_i32(csize, field_align);
            fields.push(FieldDef { offset, ty });

            csize = offset + field_size;

            if ty.reference_type() {
                ref_fields.push(offset);
            }
        }

        size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));
    }

    let stub = vm.compiler_thunk().to_usize();
    let vtable_entries = vec![stub; cls.virtual_fcts.len()];

    let cls_def = vm.class_defs.idx(id);
    let mut cls_def = cls_def.write();
    cls_def.size = size;
    cls_def.fields = fields;
    cls_def.ref_fields = ref_fields;
    cls_def.parent_id = parent_id;

    let (instance_size, element_size) = match size {
        InstanceSize::Fixed(instance_size) => (instance_size as usize, 0),
        InstanceSize::Array(element_size) => (0, element_size as usize),
        InstanceSize::ObjArray => (0, mem::ptr_width_usize()),
        InstanceSize::FreeArray => (0, mem::ptr_width_usize()),
        InstanceSize::Str => (0, 1),
    };

    let clsptr = (&*cls_def) as *const ClassDef as *mut ClassDef;
    let vtable = VTableBox::new(clsptr, instance_size, element_size, &vtable_entries);
    cls_def.vtable = Some(vtable);

    ensure_display(vm, &mut cls_def);

    id
}

fn ensure_display<'ast>(vm: &VM<'ast>, cls_def: &mut ClassDef) -> usize {
    let vtable = cls_def.vtable.as_mut().unwrap();

    // if subtype_display[0] is set, vtable was already initialized
    if !vtable.subtype_display[0].is_null() {
        return vtable.subtype_depth as usize;
    }

    if let Some(parent_id) = cls_def.parent_id {
        let parent = vm.class_defs.idx(parent_id);
        let mut parent = parent.write();
        let depth = 1 + ensure_display(vm, &mut *parent);

        let parent_vtable = parent.vtable.as_ref().unwrap();
        let depth_fixed;

        if depth >= DISPLAY_SIZE {
            depth_fixed = DISPLAY_SIZE;

            vtable.allocate_overflow(depth as usize - DISPLAY_SIZE + 1);

            unsafe {
                if depth > DISPLAY_SIZE {
                    ptr::copy_nonoverlapping(
                        parent_vtable.subtype_overflow,
                        vtable.subtype_overflow as *mut _,
                        depth as usize - DISPLAY_SIZE,
                    );
                }

                let ptr = vtable
                    .subtype_overflow
                    .offset(depth as isize - DISPLAY_SIZE as isize)
                    as *mut _;

                *ptr = &**vtable as *const _;
            }
        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = &**vtable as *const _;
        }

        vtable.subtype_depth = depth;
        vtable.subtype_display[0..depth_fixed]
            .clone_from_slice(&parent_vtable.subtype_display[0..depth_fixed]);

        depth
    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

        0
    }
}

pub fn replace_type_param(
    vm: &VM,
    ty: BuiltinType,
    cls_tp: &TypeList,
    fct_tp: &TypeList,
    self_ty: Option<BuiltinType>,
) -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, tpid) => cls_tp[tpid.idx()],
        BuiltinType::FctTypeParam(_, tpid) => fct_tp[tpid.idx()],

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params = TypeList::with(
                params
                    .iter()
                    .map(|p| replace_type_param(vm, p, cls_tp, fct_tp, self_ty))
                    .collect::<Vec<_>>(),
            );

            let list_id = vm.lists.lock().insert(params);
            BuiltinType::Class(cls_id, list_id)
        }

        BuiltinType::This => self_ty.expect("no type for Self given"),

        BuiltinType::Lambda(_) => unimplemented!(),

        _ => ty,
    }
}

pub fn specialize_for_call_type(call_type: &CallType, ty: BuiltinType, vm: &VM) -> BuiltinType {
    match *call_type {
        CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
            specialize_type(vm, ty, cls_type_params, fct_type_params)
        }

        CallType::Method(cls_ty, _, ref fct_type_params) => match cls_ty {
            BuiltinType::Class(_, list_id) => {
                let cls_type_params = vm.lists.lock().get(list_id);
                specialize_type(vm, ty, &cls_type_params, fct_type_params)
            }

            _ => ty,
        },

        CallType::Expr(ty, _) => {
            let cls_type_params = ty.type_params(vm);
            specialize_type(vm, ty, &cls_type_params, &TypeList::empty())
        }

        CallType::Ctor(cls_ty, _) | CallType::CtorNew(cls_ty, _) => {
            let cls_type_params = cls_ty.type_params(vm);
            specialize_type(vm, ty, &cls_type_params, &TypeList::empty())
        }

        CallType::Trait(_, _) => unimplemented!(),

        CallType::Intrinsic(_) => unimplemented!(),

        CallType::TraitStatic(_, _, _) => {
            assert_ne!(ty, BuiltinType::This);

            ty
        }
    }
}
