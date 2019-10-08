use parking_lot::{Mutex, RwLock};
use std::cmp::max;
use std::ptr;
use std::sync::Arc;

use crate::class::{self, ClassDef, ClassDefId, ClassId, ClassSize, FieldDef, TypeParams};
use crate::mem;
use crate::object::Header;
use crate::ty::BuiltinType;
use crate::vm::{StructData, StructDef, StructDefId, StructFieldDef, StructId, VM};
use crate::vtable::{VTableBox, DISPLAY_SIZE};

pub fn specialize_type(
    vm: &VM,
    ty: BuiltinType,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, id) => cls_type_params[id.idx()],

        BuiltinType::FctTypeParam(_, id) => fct_type_params[id.idx()],

        BuiltinType::Struct(struct_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params: TypeParams = params
                .iter()
                .map(|t| specialize_type(vm, t, cls_type_params, fct_type_params))
                .collect::<Vec<_>>()
                .into();

            let list_id = vm.lists.lock().insert(params);

            BuiltinType::Struct(struct_id, list_id)
        }

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params: Vec<_> = params
                .iter()
                .map(|t| specialize_type(vm, t, cls_type_params, fct_type_params))
                .collect();

            let list_id = vm.lists.lock().insert(params.into());

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
    specialize_struct(vm, &*struc, TypeParams::empty())
}

pub fn specialize_struct_id_params(
    vm: &VM,
    struct_id: StructId,
    type_params: TypeParams,
) -> StructDefId {
    let struc = vm.structs.idx(struct_id);
    let struc = struc.lock();
    specialize_struct(vm, &*struc, type_params)
}

pub fn specialize_struct(vm: &VM, struc: &StructData, type_params: TypeParams) -> StructDefId {
    if let Some(&id) = struc.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_struct(vm, struc, type_params)
}

fn create_specialized_struct(vm: &VM, struc: &StructData, type_params: TypeParams) -> StructDefId {
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
        let ty = specialize_type(vm, f.ty, &type_params, &TypeParams::empty());
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
    specialize_class(vm, &*cls, &TypeParams::empty())
}

pub fn specialize_class_id_params(
    vm: &VM,
    cls_id: ClassId,
    type_params: &TypeParams,
) -> ClassDefId {
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

pub fn specialize_class(vm: &VM, cls: &class::Class, type_params: &TypeParams) -> ClassDefId {
    if let Some(&id) = cls.specializations.read().get(&type_params) {
        return id;
    }

    create_specialized_class(vm, cls, type_params)
}

fn create_specialized_class(vm: &VM, cls: &class::Class, type_params: &TypeParams) -> ClassDefId {
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
            size: ClassSize::Fixed(0),
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
                ClassSize::ObjArray
            } else {
                ClassSize::Array(type_params[0].size(vm))
            }
        } else {
            ClassSize::Str
        };

        let super_id = cls
            .parent_class
            .expect("Array & String should have super class");
        let id = specialize_class_id(vm, super_id);
        parent_id = Some(id);
    } else {
        let mut csize;

        if let Some(super_id) = cls.parent_class {
            let id = specialize_class_id(vm, super_id);
            let cls_def = vm.class_defs.idx(id);
            let cls_def = cls_def.read();

            fields = Vec::new();
            ref_fields = cls_def.ref_fields.clone();
            csize = match cls_def.size {
                ClassSize::Fixed(size) => size,
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
            let ty = specialize_type(vm, f.ty, &type_params, &TypeParams::empty());
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

        size = ClassSize::Fixed(mem::align_i32(csize, mem::ptr_width()));
    }

    let stub = vm.compiler_thunk().to_usize();
    let vtable_entries = vec![stub; cls.virtual_fcts.len()];

    let cls_def = vm.class_defs.idx(id);
    let mut cls_def = cls_def.write();
    cls_def.size = size;
    cls_def.fields = fields;
    cls_def.ref_fields = ref_fields;
    cls_def.parent_id = parent_id;

    let clsptr = (&*cls_def) as *const class::ClassDef as *mut class::ClassDef;
    let vtable = VTableBox::new(clsptr, &vtable_entries);
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

        vtable.subtype_depth = depth as i32;
        vtable.subtype_display[0..depth_fixed]
            .clone_from_slice(&parent_vtable.subtype_display[0..depth_fixed]);

        depth
    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

        0
    }
}
