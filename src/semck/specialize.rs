use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::sync::RwLock;

use baseline::stub::ensure_stub;
use class::{self, ClassDef, ClassDefId, ClassId};
use ctxt::{SemContext, Fct, FctId, FctKind, FctParent, FctSrc, NodeMap, Var};
use mem;
use object::Header;
use vtable::{DISPLAY_SIZE, VTableBox};
use ty::{BuiltinType, TypeId};

pub fn specialize_class(ctxt: &SemContext,
                        cls: &class::Class,
                        type_params: Vec<BuiltinType>)
                        -> (ClassId, TypeId) {
    if let Some(&id) = cls.specializations.borrow().get(&type_params) {
        let type_id = ctxt.types.borrow_mut().insert(cls.id, type_params);
        return (id, type_id);
    }

    cls.specializations
        .borrow_mut()
        .insert(type_params.clone(), ClassId::max());

    let id = create_specialized_class(ctxt, cls, type_params.clone());
    cls.specializations
        .borrow_mut()
        .insert(type_params.clone(), id);

    let type_id = ctxt.types.borrow_mut().insert(cls.id, type_params);
    ctxt.types.borrow().set_cls_id(type_id, id);

    (id, type_id)
}

fn create_specialized_class(ctxt: &SemContext,
                            cls: &class::Class,
                            type_params: Vec<BuiltinType>)
                            -> ClassId {
    let id: ClassId = ctxt.classes.len().into();
    let specialize_for = SpecializeFor::Class;

    let mut is_array = false;
    let mut is_object_array = false;
    let mut element_size = 0;

    match cls.ty {
        BuiltinType::Generic(type_id) => {
            let ty = ctxt.types.borrow().get(type_id);

            if ty.cls_id == ctxt.primitive_classes.generic_array {
                is_array = true;

                let type_param = type_params[0].to_specialized(ctxt);

                if !type_param.is_type_param() {
                    is_object_array = type_param.reference_type();
                    element_size = type_param.size(ctxt);
                }
            }
        }

        _ => {}
    }

    let is_generic = type_params.iter().any(|t| t.is_type_param());

    ctxt.classes
        .push(class::Class {
                  id: id,
                  pos: cls.pos,
                  name: cls.name,
                  ty: BuiltinType::Class(id),
                  parent_class: cls.parent_class,
                  has_open: cls.has_open,
                  is_abstract: cls.is_abstract,
                  internal: cls.internal,
                  internal_resolved: true,
                  primary_ctor: cls.primary_ctor,

                  ctors: Vec::new(),
                  fields: Vec::new(),
                  methods: Vec::new(),
                  size: 0,
                  vtable: None,

                  traits: cls.traits.clone(),
                  impls: cls.impls.clone(),

                  type_params: Vec::new(),
                  is_generic: is_generic,
                  specialization_for: Some(cls.id),
                  specialization_params: type_params.clone(),
                  specializations: RefCell::new(HashMap::new()),
                  def_specializations: RefCell::new(HashMap::new()),

                  is_array: is_array,
                  is_object_array: is_object_array,
                  element_size: element_size,

                  ref_fields: Vec::new(),
              });

    let cloned_ctors = cls.ctors
        .iter()
        .map(|&ctor_id| {
                 let ctor = ctxt.fcts[ctor_id].borrow();
                 specialize_fct(ctxt,
                                FctParent::Class(id),
                                &*ctor,
                                specialize_for,
                                &type_params)
             })
        .collect();
    ctxt.classes[id].borrow_mut().ctors = cloned_ctors;

    let cloned_methods = cls.methods
        .iter()
        .map(|&method_id| {
                 let mtd = ctxt.fcts[method_id].borrow();
                 specialize_fct(ctxt,
                                FctParent::Class(id),
                                &*mtd,
                                specialize_for,
                                &type_params)
             })
        .collect();
    ctxt.classes[id].borrow_mut().methods = cloned_methods;

    let cloned_fields = cls.fields
        .iter()
        .map(|field| {
            class::Field {
                id: field.id,
                name: field.name,
                ty: specialize_type(ctxt, field.ty, specialize_for, &type_params),
                offset: field.offset,
                reassignable: field.reassignable,
            }
        })
        .collect();
    ctxt.classes[id].borrow_mut().fields = cloned_fields;

    id
}

pub fn specialize_fct<'ast>(ctxt: &SemContext<'ast>,
                            parent: FctParent,
                            fct: &Fct<'ast>,
                            specialize_for: SpecializeFor,
                            type_params: &[BuiltinType])
                            -> FctId {
    let fct_id = ctxt.fcts.len().into();

    let mut param_types: Vec<_> = fct.param_types
        .iter()
        .map(|&t| specialize_type(ctxt, t, specialize_for, &type_params))
        .collect();

    if fct.has_self() && fct.initialized {
        match parent {
            FctParent::Class(cls_id) => {
                param_types[0] = BuiltinType::Class(cls_id);
            }

            _ => unreachable!(),
        }
    }

    let cloned_kind = match fct.kind {
        FctKind::Source(ref src) => {
            let src = src.borrow();

            let cloned_vars = src.vars
                .iter()
                .map(|v| {
                    Var {
                        id: v.id,
                        name: v.name,
                        ty: specialize_type(ctxt, v.ty, specialize_for, &type_params),
                        reassignable: v.reassignable,
                        node_id: v.node_id,
                    }
                })
                .collect();

            let mut cloned_map_tys = NodeMap::new();

            for (&id, &ty) in src.map_tys.iter() {
                let ty = specialize_type(ctxt, ty, specialize_for, &type_params);
                cloned_map_tys.insert(id, ty);
            }

            FctKind::Source(RefCell::new(FctSrc {
                                             map_calls: src.map_calls.clone(),
                                             map_idents: src.map_idents.clone(),
                                             map_tys: cloned_map_tys,
                                             map_vars: src.map_vars.clone(),
                                             map_convs: src.map_convs.clone(),
                                             map_cls: src.map_cls.clone(),

                                             always_returns: src.always_returns,
                                             jit_fct: RwLock::new(None),
                                             specializations: RwLock::new(HashMap::new()),
                                             vars: cloned_vars,
                                         }))
        }

        FctKind::Definition => FctKind::Definition,
        FctKind::Native(ptr) => FctKind::Native(ptr),
        FctKind::Builtin(intr) => FctKind::Builtin(intr),
    };

    let fct = Fct {
        id: fct_id,
        ast: fct.ast,
        pos: fct.pos,
        name: fct.name,
        parent: parent,
        has_open: fct.has_open,
        has_override: fct.has_override,
        has_final: fct.has_final,
        is_static: fct.is_static,
        is_pub: fct.is_pub,
        is_abstract: fct.is_abstract,
        internal: fct.internal,
        internal_resolved: true,
        overrides: fct.overrides,
        param_types: param_types,
        return_type: specialize_type(ctxt, fct.return_type, specialize_for, &type_params),
        ctor: fct.ctor,

        vtable_index: fct.vtable_index,
        initialized: fct.initialized,
        throws: fct.throws,

        type_params: Vec::new(),
        kind: cloned_kind,
    };

    ctxt.fcts.push(fct);

    fct_id
}

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

            let cls_id = ty.cls_id;
            let cls = ctxt.classes[cls_id].borrow();

            let (_, type_id) = specialize_class(ctxt, &*cls, params.clone());
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

pub fn specialize_class_def(ctxt: &SemContext,
                            cls: &class::Class,
                            type_params: &[BuiltinType])
                            -> ClassDefId {
    if let Some(&id) = cls.def_specializations.borrow().get(type_params) {
        return id;
    }

    create_specialized_class_def(ctxt, cls, type_params)
}

fn create_specialized_class_def(ctxt: &SemContext,
                                cls: &class::Class,
                                type_params: &[BuiltinType])
                                -> ClassDefId {
    let id: ClassDefId = ctxt.class_defs.len().into();

    let old = cls.def_specializations.borrow_mut().insert(type_params.to_vec(), id);
    assert!(old.is_none());

    ctxt.class_defs
        .push(ClassDef {
                  id: id,
                  cls_id: cls.id,
                  type_params: type_params.to_vec(),
                  parent_id: None,
                  size: 0,
                  fields: Vec::new(),
                  ref_fields: Vec::new(),
                  vtable: None,
              });

    let mut fields;
    let mut ref_fields;
    let mut size;
    let mut vtable_entries = Vec::new();
    let parent_id;

    if let Some(super_id) = cls.parent_class {
        let sup = ctxt.classes[super_id].borrow();
        let id = specialize_class_def(ctxt, &*sup, &[]);
        let cls_def = ctxt.class_defs[id].borrow();

        fields = cls_def.fields.clone();
        ref_fields = cls_def.ref_fields.clone();
        size = cls_def.size;
        parent_id = Some(id);

        let vtable = sup.vtable.as_ref().unwrap();
        vtable_entries.extend_from_slice(vtable.table());

    } else {
        fields = Vec::with_capacity(cls.fields.len());
        ref_fields = Vec::new();
        size = Header::size();
        parent_id = None;
    };

    for f in &cls.fields {
        let ty = specialize_type(ctxt, f.ty, SpecializeFor::Class, &type_params);
        debug_assert!(!ty.contains_type_param(ctxt));

        let field_size = f.ty.size(ctxt);
        let field_align = f.ty.align(ctxt);

        let offset = mem::align_i32(size, field_align);
        fields.push(offset);

        size = offset + field_size;

        if ty.reference_type() {
            ref_fields.push(offset);
        }
    }

    size = mem::align_i32(size, mem::ptr_width());

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

    let clsptr = cls as *const class::Class as *mut class::Class;
    let vtable = VTableBox::new(clsptr, &vtable_entries);

    let mut cls_def = ctxt.class_defs[id].borrow_mut();
    cls_def.size = size;
    cls_def.fields = fields;
    cls_def.ref_fields = ref_fields;
    cls_def.vtable = Some(vtable);
    cls_def.parent_id = parent_id;

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