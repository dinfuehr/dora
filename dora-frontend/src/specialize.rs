use crate::sema::{
    find_impl, AliasParent, Element, ImplDefinition, Sema, TraitDefinitionId, TypeParamId,
};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn specialize_trait_type(sa: &Sema, ty: TraitType, type_params: &SourceTypeArray) -> TraitType {
    TraitType {
        trait_id: ty.trait_id,
        type_params: replace_sta(sa, ty.type_params, Some(type_params), None),
        bindings: ty
            .bindings
            .into_iter()
            .map(|(id, ty)| (id, replace_type(sa, ty, Some(type_params), None)))
            .collect(),
    }
}

pub fn specialize_type(sa: &Sema, ty: SourceType, type_params: &SourceTypeArray) -> SourceType {
    replace_type(sa, ty, Some(type_params), None)
}

pub fn specialize_type_array(
    sa: &Sema,
    types: &SourceTypeArray,
    type_params: &SourceTypeArray,
) -> SourceTypeArray {
    let new_types = types
        .iter()
        .map(|ty| specialize_type(sa, ty, type_params))
        .collect();

    SourceTypeArray::with(new_types)
}

pub fn replace_type(
    sa: &Sema,
    ty: SourceType,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            replace_sta(sa, cls_type_params, type_params, self_ty),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            replace_sta(sa, trait_type_params, type_params, self_ty.clone()),
            replace_sta(sa, bindings, type_params, self_ty),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            replace_sta(sa, struct_type_params, type_params, self_ty),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            replace_sta(sa, enum_type_params, type_params, self_ty),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            replace_sta(sa, alias_type_params, type_params, self_ty),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => SourceType::Assoc(
            alias_id,
            replace_sta(sa, alias_type_params, type_params, self_ty),
        ),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            replace_sta(sa, params, type_params, self_ty.clone()),
            Box::new(replace_type(sa, *return_type, type_params, self_ty)),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(replace_sta(sa, subtypes, type_params, self_ty))
        }

        SourceType::This => {
            if let Some(self_ty) = self_ty {
                self_ty
            } else {
                ty
            }
        }

        SourceType::TypeParam(id) => {
            if let Some(type_params) = type_params {
                type_params[id.index()].clone()
            } else {
                ty
            }
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::GenericAssoc { .. } => ty,

        SourceType::Any | SourceType::Ptr => unreachable!(),
    }
}

fn replace_sta(
    sa: &Sema,
    array: SourceTypeArray,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| replace_type(sa, ty, type_params.clone(), self_ty.clone()))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub struct CallSpecializationData {
    pub object_ty: SourceType,
    pub type_params: SourceTypeArray,
}

pub fn specialize_ty_for_call(
    sa: &Sema,
    ty: SourceType,
    caller_element: &dyn Element,
    call_data: &CallSpecializationData,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_call_array(sa, cls_type_params, caller_element, call_data),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_call_array(sa, trait_type_params, caller_element, call_data),
            specialize_ty_for_call_array(sa, bindings, caller_element, call_data),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_call_array(sa, struct_type_params, caller_element, call_data),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_call_array(sa, enum_type_params, caller_element, call_data),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_call_array(sa, alias_type_params, caller_element, call_data),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => {
            let alias = sa.alias(alias_id);
            assert!(alias_type_params.is_empty());

            match &call_data.object_ty {
                SourceType::TraitObject(_trait_id, _type_params, assoc_types) => {
                    assoc_types[alias.idx_in_trait()].clone()
                }

                // Associated types should only be used in traits.
                _ => unreachable!(),
            }
        }

        SourceType::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let type_param_ty = call_data.type_params[tp_id.index()].clone();

            if type_param_ty.is_type_param() {
                SourceType::GenericAssoc {
                    tp_id: type_param_ty.type_param_id().expect("missing"),
                    trait_ty,
                    assoc_id,
                }
            } else if type_param_ty.is_self() {
                let caller_fct = caller_element.to_fct().expect("expected function");

                assert_eq!(
                    caller_fct.trait_id(),
                    assoc.parent.to_trait_id().expect("expected trait")
                );
                SourceType::Assoc(assoc_id, SourceTypeArray::empty())
            } else if let Some(impl_match) = find_impl(
                sa,
                caller_element,
                type_param_ty,
                caller_element.type_param_definition(),
                trait_ty,
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                let ty = specialize_type(sa, ty, &impl_match.bindings);
                specialize_ty_for_call(sa, ty, caller_element, call_data)
            } else {
                unimplemented!()
            }
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_for_call_array(sa, params, caller_element, call_data),
            Box::new(specialize_ty_for_call(
                sa,
                *return_type,
                caller_element,
                call_data,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_call_array(
            sa,
            subtypes,
            caller_element,
            call_data,
        )),

        SourceType::TypeParam(id) => call_data.type_params[id.index()].clone(),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::This | SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_call_array(
    sa: &Sema,
    array: SourceTypeArray,
    caller_element: &dyn Element,
    call_data: &CallSpecializationData,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_call(sa, ty, caller_element, call_data))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_trait_object(
    sa: &Sema,
    ty: SourceType,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    assoc_types: &SourceTypeArray,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_trait_object_array(
                sa,
                cls_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_trait_object_array(
                sa,
                trait_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
            specialize_ty_for_trait_object_array(sa, bindings, trait_id, type_params, assoc_types),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_trait_object_array(
                sa,
                struct_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_trait_object_array(
                sa,
                enum_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_trait_object_array(
                sa,
                alias_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => {
            let alias = sa.alias(alias_id);
            assert!(alias_type_params.is_empty());
            assoc_types[alias.idx_in_trait()].clone()
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_for_trait_object_array(sa, params, trait_id, type_params, assoc_types),
            Box::new(specialize_ty_for_trait_object(
                sa,
                *return_type,
                trait_id,
                type_params,
                assoc_types,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_trait_object_array(
            sa,
            subtypes,
            trait_id,
            type_params,
            assoc_types,
        )),

        SourceType::TypeParam(id) => type_params[id.index()].clone(),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::GenericAssoc { .. } => ty,

        SourceType::This | SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_trait_object_array(
    sa: &Sema,
    array: SourceTypeArray,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    assoc_types: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_trait_object(sa, ty, trait_id, type_params, assoc_types))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_default_trait_method(
    sa: &Sema,
    ty: SourceType,
    impl_: &ImplDefinition,
    trait_ty: &TraitType,
    extended_ty: &SourceType,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_array_for_default_trait_method(
                sa,
                cls_type_params,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_array_for_default_trait_method(
                sa,
                trait_type_params,
                impl_,
                trait_ty,
                extended_ty,
            ),
            specialize_ty_array_for_default_trait_method(
                sa,
                bindings,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_array_for_default_trait_method(
                sa,
                struct_type_params,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_array_for_default_trait_method(
                sa,
                enum_type_params,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_array_for_default_trait_method(
                sa,
                alias_type_params,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::Assoc(assoc_id, assoc_type_params) => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            assert!(assoc_type_params.is_empty());

            let impl_alias_id = impl_.trait_alias_map().get(&assoc_id).cloned();

            if let Some(impl_alias_id) = impl_alias_id {
                let impl_alias = sa.alias(impl_alias_id);
                impl_alias.ty()
            } else {
                SourceType::Error
            }
        }

        SourceType::GenericAssoc { .. } => unimplemented!(),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_array_for_default_trait_method(sa, params, impl_, trait_ty, extended_ty),
            Box::new(specialize_ty_for_default_trait_method(
                sa,
                *return_type,
                impl_,
                trait_ty,
                extended_ty,
            )),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(specialize_ty_array_for_default_trait_method(
                sa,
                subtypes,
                impl_,
                trait_ty,
                extended_ty,
            ))
        }

        SourceType::TypeParam(id) => {
            let container_type_params = sa
                .trait_(trait_ty.trait_id)
                .type_param_definition()
                .container_type_params();

            if id.index() < container_type_params {
                // This is a container/trait-type parameter.
                trait_ty.type_params[id.index()].clone()
            } else {
                // This is a function-type parameter.
                let id = impl_.type_param_definition().type_param_count()
                    + (id.index() - container_type_params);
                SourceType::TypeParam(TypeParamId(id))
            }
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::This => extended_ty.clone(),

        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
    }
}

fn specialize_ty_array_for_default_trait_method(
    sa: &Sema,
    array: SourceTypeArray,
    impl_: &ImplDefinition,
    trait_ty: &TraitType,
    extended_ty: &SourceType,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_default_trait_method(sa, ty, impl_, trait_ty, extended_ty))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_generic(
    sa: &Sema,
    ty: SourceType,
    type_param_id: TypeParamId,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    object_type: &SourceType,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_generic_array(
                sa,
                cls_type_params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_generic_array(
                sa,
                trait_type_params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
            specialize_ty_for_generic_array(
                sa,
                bindings,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_generic_array(
                sa,
                struct_type_params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_generic_array(
                sa,
                enum_type_params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_generic_array(
                sa,
                alias_type_params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => {
            let alias = sa.alias(alias_id);
            assert_eq!(alias.parent, AliasParent::Trait(trait_id));
            assert!(alias_type_params.is_empty());
            SourceType::GenericAssoc {
                tp_id: type_param_id,
                trait_ty: TraitType::from_trait_id(trait_id),
                assoc_id: alias_id,
            }
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_for_generic_array(
                sa,
                params,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            ),
            Box::new(specialize_ty_for_generic(
                sa,
                *return_type,
                type_param_id,
                trait_id,
                type_params,
                object_type,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_generic_array(
            sa,
            subtypes,
            type_param_id,
            trait_id,
            type_params,
            object_type,
        )),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::TypeParam(id) => type_params[id.index()].clone(),

        SourceType::This => object_type.clone(),

        SourceType::Any | SourceType::Ptr | SourceType::GenericAssoc { .. } => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_generic_array(
    sa: &Sema,
    array: SourceTypeArray,
    type_param_id: TypeParamId,
    trait_id: TraitDefinitionId,
    trait_type_params: &SourceTypeArray,
    object_type: &SourceType,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| {
            specialize_ty_for_generic(
                sa,
                ty,
                type_param_id,
                trait_id,
                trait_type_params,
                object_type,
            )
        })
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_for_element(
    sa: &Sema,
    ty: SourceType,
    element: &dyn Element,
    type_params_for_element: &SourceTypeArray,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_for_element_array(sa, cls_type_params, element, type_params_for_element),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_for_element_array(sa, trait_type_params, element, type_params_for_element),
            specialize_for_element_array(sa, bindings, element, type_params_for_element),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_for_element_array(sa, struct_type_params, element, type_params_for_element),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_for_element_array(sa, enum_type_params, element, type_params_for_element),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_for_element_array(sa, alias_type_params, element, type_params_for_element),
        ),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_for_element_array(sa, params, element, type_params_for_element),
            Box::new(specialize_for_element(
                sa,
                *return_type,
                element,
                type_params_for_element,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_for_element_array(
            sa,
            subtypes,
            element,
            type_params_for_element,
        )),

        SourceType::This => {
            assert!(element.is_trait());
            SourceType::This
        }

        SourceType::TypeParam(id) => type_params_for_element[id.index()].clone(),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let type_param_ty = type_params_for_element[tp_id.index()].clone();

            if type_param_ty.is_type_param() {
                SourceType::GenericAssoc {
                    tp_id: type_param_ty.type_param_id().expect("missing"),
                    trait_ty,
                    assoc_id,
                }
            } else if let Some(impl_match) = find_impl(
                sa,
                element,
                type_param_ty,
                element.type_param_definition(),
                trait_ty,
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                specialize_for_element(sa, ty, element, type_params_for_element)
            } else {
                unimplemented!()
            }
        }

        SourceType::Any | SourceType::Ptr | SourceType::Assoc(..) => unreachable!(),
    }
}

fn specialize_for_element_array(
    sa: &Sema,
    array: SourceTypeArray,
    element: &dyn Element,
    type_params_for_element: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_for_element(sa, ty, element, type_params_for_element))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}
