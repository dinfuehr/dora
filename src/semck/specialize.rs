use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::RwLock;

use class::{self, ClassId};
use ctxt::{Context, Fct, FctId, FctKind, FctParent, FctSrc, Var};
use ty::BuiltinType;

pub fn specialize_class(ctxt: &Context,
                    cls: &mut class::Class,
                    type_params: Vec<BuiltinType>)
                    -> ClassId {
    if let Some(&id) = cls.specializations.get(&type_params) {
        return id;
    }

    let id = create_specialized_class(ctxt, cls, type_params.clone());
    cls.specializations.insert(type_params, id);

    id
}

fn create_specialized_class(ctxt: &Context,
                            cls: &mut class::Class,
                            type_params: Vec<BuiltinType>)
                            -> ClassId {
    let id = ctxt.classes.len().into();

    let cloned_ctors = cls.ctors
        .iter()
        .map(|&ctor_id| {
            let ctor = ctxt.fcts[ctor_id].borrow();
            specialize_fct(ctxt, id, &*ctor, &type_params)
        })
        .collect();

    let cloned_methods = cls.methods.iter().map(|&method_id| {
        let mtd = ctxt.fcts[method_id].borrow();
        specialize_fct(ctxt, id, &*mtd, &type_params)
    }).collect();

    let cloned_fields = cls.fields
        .iter()
        .map(|field| {
            class::Field {
                id: field.id,
                name: field.name,
                ty: specialize_type(field.ty, &type_params),
                offset: field.offset,
                reassignable: field.reassignable,
            }
        })
        .collect();

    ctxt.classes.push(class::Class {
                          id: id,
                          pos: cls.pos,
                          name: cls.name,
                          ty: BuiltinType::Class(id),
                          parent_class: cls.parent_class,
                          has_open: cls.has_open,
                          internal: cls.internal,
                          internal_resolved: cls.internal_resolved,
                          primary_ctor: cls.primary_ctor,

                          ctors: cloned_ctors,
                          fields: cloned_fields,
                          methods: cloned_methods,
                          size: 0,
                          vtable: None,

                          traits: cls.traits.clone(),
                          impls: cls.impls.clone(),

                          type_params: Vec::new(),
                          specialization_for: Some(cls.id),
                          specialization_params: type_params,
                          specializations: HashMap::new(),

                          ref_fields: Vec::new(),
                      });

    id
}

fn specialize_fct<'a, 'ast: 'a>(ctxt: &Context<'ast>, id: ClassId, fct: &Fct<'ast>, type_params: &[BuiltinType]) -> FctId {
    let fct_id = ctxt.fcts.len().into();

    let mut param_types: Vec<_> = fct.param_types
        .iter()
        .map(|&t| specialize_type(t, &type_params))
        .collect();

    if fct.has_self() && fct.initialized {
        param_types[0] = BuiltinType::Class(id);
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
                        ty: specialize_type(v.ty, &type_params),
                        reassignable: v.reassignable,
                        node_id: v.node_id,
                    }
                })
                .collect();

            FctKind::Source(RefCell::new(FctSrc {
                                                map_calls: src.map_calls.clone(),
                                                map_idents: src.map_idents.clone(),
                                                map_tys: src.map_tys.clone(),
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
        parent: FctParent::Class(id),
        has_open: fct.has_open,
        has_override: fct.has_override,
        has_final: fct.has_final,
        is_static: fct.is_static,
        is_pub: fct.is_pub,
        internal: fct.internal,
        internal_resolved: fct.internal_resolved,
        overrides: fct.overrides,
        param_types: param_types,
        return_type: specialize_type(fct.return_type, &type_params),
        ctor: fct.ctor,

        ctor_allocates: fct.ctor_allocates,

        vtable_index: fct.vtable_index,
        initialized: fct.initialized,
        throws: fct.throws,
        kind: cloned_kind,
    };

    ctxt.fcts.push(fct);

    fct_id
}

fn specialize_type(ty: BuiltinType, type_params: &[BuiltinType]) -> BuiltinType {
    match ty {
        BuiltinType::TypeParam(id) => type_params[id.idx()],
        _ => ty,
    }
}