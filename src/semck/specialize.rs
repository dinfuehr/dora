use std::ptr;

use baseline::stub::ensure_stub;
use class::{self, ClassDef, ClassDefId, ClassId, ClassSize};
use ctxt::SemContext;
use mem;
use object::Header;
use vtable::{DISPLAY_SIZE, VTableBox};
use ty::BuiltinType;

pub fn specialize_type<'ast>(ctxt: &SemContext<'ast>,
                             ty: BuiltinType,
                             specialize_for: SpecializeFor,
                             type_params: &[BuiltinType])
                             -> BuiltinType {
    match ty {
        BuiltinType::ClassTypeParam(_, id) => {
            if specialize_for == SpecializeFor::Class {
                type_params[id.idx()]
            } else {
                ty
            }
        }

        BuiltinType::FctTypeParam(_, id) => {
            if specialize_for == SpecializeFor::Fct {
                type_params[id.idx()]
            } else {
                ty
            }
        }

        BuiltinType::Generic(type_id) => {
            let ty = ctxt.types.borrow().get(type_id);

            let params: Vec<_> = ty.params
                .iter()
                .map(|&t| specialize_type(ctxt, t, specialize_for, type_params))
                .collect();

            let type_id = ctxt.types.borrow_mut().insert(ty.cls_id, params);

            BuiltinType::Generic(type_id)
        }

        _ => ty,
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SpecializeFor {
    Fct,
    Class,
}

pub fn specialize_class_id(ctxt: &SemContext, cls_id: ClassId) -> ClassDefId {
    let cls = ctxt.classes[cls_id].borrow();
    specialize_class(ctxt, &*cls, &[])
}

pub fn specialize_class_id_params(ctxt: &SemContext,
                                  cls_id: ClassId,
                                  type_params: &[BuiltinType])
                                  -> ClassDefId {
    let cls = ctxt.classes[cls_id].borrow();
    specialize_class(ctxt, &*cls, type_params)
}

pub fn specialize_class_ty(ctxt: &SemContext, ty: BuiltinType) -> ClassDefId {
    match ty {
        BuiltinType::Class(cls_id) => specialize_class_id(ctxt, cls_id),
        BuiltinType::Generic(type_id) => {
            let t = ctxt.types.borrow().get(type_id);

            specialize_class_id_params(ctxt, t.cls_id, &t.params)
        }

        _ => unreachable!(),
    }
}

pub fn specialize_class(ctxt: &SemContext,
                        cls: &class::Class,
                        type_params: &[BuiltinType])
                        -> ClassDefId {
    if let Some(&id) = cls.specializations.borrow().get(type_params) {
        return id;
    }

    create_specialized_class(ctxt, cls, type_params)
}

fn create_specialized_class(ctxt: &SemContext,
                            cls: &class::Class,
                            type_params: &[BuiltinType])
                            -> ClassDefId {
    let id: ClassDefId = ctxt.class_defs.len().into();

    let old = cls.specializations
        .borrow_mut()
        .insert(type_params.to_vec(), id);
    assert!(old.is_none());

    ctxt.class_defs
        .push(ClassDef {
                  id: id,
                  cls_id: cls.id,
                  type_params: type_params.to_vec(),
                  parent_id: None,
                  size: ClassSize::Fixed(0),
                  fields: Vec::new(),
                  ref_fields: Vec::new(),
                  vtable: None,
              });

    let mut fields;
    let mut ref_fields;
    let size;
    let mut vtable_entries = Vec::new();
    let parent_id;

    if cls.is_array || cls.is_str {
        fields = Vec::new();
        ref_fields = Vec::new();

        size = if cls.is_array {
            if type_params[0].reference_type() {
                ClassSize::ObjArray
            } else {
                ClassSize::Array(type_params[0].size(ctxt))
            }
        } else {
            ClassSize::Str
        };

        let super_id = cls.parent_class
            .expect("Array & Str should have super class");
        let id = specialize_class_id(ctxt, super_id);
        parent_id = Some(id);

    } else {
        let mut csize;

        if let Some(super_id) = cls.parent_class {
            let id = specialize_class_id(ctxt, super_id);
            let cls_def = ctxt.class_defs[id].borrow();

            fields = cls_def.fields.clone();
            ref_fields = cls_def.ref_fields.clone();
            csize = match cls_def.size {
                ClassSize::Fixed(size) => size,
                _ => unreachable!(),
            };
            parent_id = Some(id);

            let vtable = cls_def.vtable.as_ref().unwrap();
            vtable_entries.extend_from_slice(vtable.table());

        } else {
            fields = Vec::with_capacity(cls.fields.len());
            ref_fields = Vec::new();
            csize = Header::size();
            parent_id = None;
        };

        for f in &cls.fields {
            let ty = specialize_type(ctxt, f.ty, SpecializeFor::Class, &type_params);
            debug_assert!(!ty.contains_type_param(ctxt));

            let field_size = ty.size(ctxt);
            let field_align = ty.align(ctxt);

            let offset = mem::align_i32(csize, field_align);
            fields.push(offset);

            csize = offset + field_size;

            if ty.reference_type() {
                ref_fields.push(offset);
            }
        }

        size = ClassSize::Fixed(mem::align_i32(csize, mem::ptr_width()));
    }

    for &mid in &cls.methods {
        let mut fct = ctxt.fcts[mid].borrow_mut();

        if fct.vtable_index.is_some() {
            continue;
        }

        if fct.is_virtual() {
            let vtable_index = if let Some(overrides) = fct.overrides {
                ctxt.fcts[overrides].borrow().vtable_index.unwrap()

            } else {
                let vtable_index = vtable_entries.len();
                vtable_entries.push(ensure_stub(ctxt) as usize);

                vtable_index as u32
            };

            fct.vtable_index = Some(vtable_index);
        }
    }

    let mut cls_def = ctxt.class_defs[id].borrow_mut();
    cls_def.size = size;
    cls_def.fields = fields;
    cls_def.ref_fields = ref_fields;
    cls_def.parent_id = parent_id;

    let clsptr = (&*cls_def) as *const class::ClassDef as *mut class::ClassDef;
    let vtable = VTableBox::new(clsptr, &vtable_entries);
    cls_def.vtable = Some(vtable);

    ensure_display(ctxt, &mut cls_def);

    id
}

fn ensure_display<'ast>(ctxt: &SemContext<'ast>, cls_def: &mut ClassDef) -> usize {
    let vtable = cls_def.vtable.as_mut().unwrap();

    // if subtype_display[0] is set, vtable was already initialized
    if !vtable.subtype_display[0].is_null() {
        return vtable.subtype_depth as usize;
    }

    if let Some(parent_id) = cls_def.parent_id {
        let mut parent = ctxt.class_defs[parent_id].borrow_mut();
        let depth = 1 + ensure_display(ctxt, &mut *parent);

        let parent_vtable = parent.vtable.as_ref().unwrap();
        let depth_fixed;

        if depth >= DISPLAY_SIZE {
            depth_fixed = DISPLAY_SIZE;

            vtable.allocate_overflow(depth as usize - DISPLAY_SIZE + 1);

            unsafe {
                if depth > DISPLAY_SIZE {
                    ptr::copy_nonoverlapping(parent_vtable.subtype_overflow,
                                             vtable.subtype_overflow as *mut _,
                                             depth as usize - DISPLAY_SIZE);
                }

                let ptr = vtable
                    .subtype_overflow
                    .offset(depth as isize - DISPLAY_SIZE as isize) as
                          *mut _;

                *ptr = &**vtable as *const _;
            }

        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = &**vtable as *const _;
        }

        vtable.subtype_depth = depth as i32;
        vtable.subtype_display[0..depth_fixed].clone_from_slice(&parent_vtable.subtype_display
                                                                     [0..depth_fixed]);

        depth

    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

        0
    }
}
