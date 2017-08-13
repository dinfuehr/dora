use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::RwLock;

use class::{self, ClassId};
use ctxt::{SemContext, Fct, FctId, FctKind, FctParent, FctSrc, NodeMap, Var};
use ty::{BuiltinType, TypeId};

pub fn specialize_class(ctxt: &SemContext,
                        cls: &class::Class,
                        type_params: Vec<BuiltinType>)
                        -> (ClassId, TypeId) {
    if let Some(&id) = cls.specializations.borrow().get(&type_params) {
        let type_id = ctxt.types
            .borrow_mut()
            .insert(BuiltinType::Class(cls.id), type_params);
        return (id, type_id);
    }

    cls.specializations
        .borrow_mut()
        .insert(type_params.clone(), ClassId::max());

    let id = create_specialized_class(ctxt, cls, type_params.clone());
    cls.specializations
        .borrow_mut()
        .insert(type_params.clone(), id);

    let type_id = ctxt.types
        .borrow_mut()
        .insert(BuiltinType::Class(cls.id), type_params);
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

            if ty.base.cls_id(ctxt).unwrap() == ctxt.primitive_classes.generic_array {
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
        specialization_for: Some(fct.id),
        specialization_params: type_params.to_owned(),
        specializations: HashMap::new(),

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

            let cls_id = ty.base.cls_id(ctxt).unwrap();
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
