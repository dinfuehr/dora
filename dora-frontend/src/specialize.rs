use crate::sema::{
    Element, FctDefinition, ImplDefinition, Sema, TraitDefinitionId, TypeParamId, find_impl,
};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn specialize_trait_type(sa: &Sema, ty: TraitType, type_params: &SourceTypeArray) -> TraitType {
    specialize_trait_type_generic(sa, ty, &|ty| replace_type(sa, ty, Some(type_params), None))
}

pub fn specialize_trait_type_generic<S>(
    _sa: &Sema,
    trait_ty: TraitType,
    specialize: &S,
) -> TraitType
where
    S: Fn(SourceType) -> SourceType,
{
    let type_params = trait_ty
        .type_params
        .iter()
        .map(|ty| specialize(ty))
        .collect::<Vec<_>>();

    let mut new_bindings = Vec::with_capacity(trait_ty.bindings.len());

    for (id, ty) in &trait_ty.bindings {
        let ty = specialize(ty.clone());

        match ty {
            SourceType::Assoc {
                trait_ty: assoc_trait_ty,
                assoc_id,
            } if assoc_trait_ty.trait_id == trait_ty.trait_id
                && assoc_trait_ty.type_params == trait_ty.type_params
                && assoc_id == *id =>
            {
                // Associated type binds to itself.
            }

            _ => new_bindings.push((*id, ty)),
        }
    }

    TraitType {
        trait_id: trait_ty.trait_id,
        type_params: type_params.into(),
        bindings: new_bindings,
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
        | SourceType::Assoc { .. }
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

        SourceType::Assoc { assoc_id, .. } => {
            let alias = sa.alias(assoc_id);

            match &call_data.object_ty {
                SourceType::TraitObject(_trait_id, _type_params, assoc_types) => {
                    assoc_types[alias.idx_in_trait()].clone()
                }

                // Associated types should only be used in traits.
                _ => unreachable!(),
            }
        }

        SourceType::GenericAssoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let specialized_ty = specialize_ty_for_call(sa, *ty, caller_element, call_data);

            if specialized_ty.is_type_param() {
                SourceType::GenericAssoc {
                    ty: Box::new(specialized_ty),
                    trait_ty,
                    assoc_id,
                }
            } else if specialized_ty.is_self() {
                let caller_fct = caller_element.to_fct().expect("expected function");
                let current_trait_id = caller_fct.trait_id();
                let assoc_trait_id = assoc.parent.to_trait_id().expect("expected trait");

                let trait_ = sa.trait_(current_trait_id);
                let trait_param_count = trait_.type_param_definition().type_param_count();
                let trait_type_params = call_data
                    .type_params
                    .iter()
                    .take(trait_param_count)
                    .collect::<Vec<_>>();

                let current_trait_ty = TraitType {
                    trait_id: current_trait_id,
                    type_params: SourceTypeArray::with(trait_type_params),
                    bindings: Vec::new(),
                };

                let trait_ty = find_super_trait_ty(sa, &current_trait_ty, assoc_trait_id)
                    .expect("super trait not found for associated type on Self");
                SourceType::Assoc { trait_ty, assoc_id }
            } else if let Some(impl_match) = find_impl(
                sa,
                caller_element,
                specialized_ty,
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

        SourceType::Assoc { assoc_id, .. } => {
            let alias = sa.alias(assoc_id);
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
    trait_method: &FctDefinition,
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
                trait_method,
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
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            ),
            specialize_ty_array_for_default_trait_method(
                sa,
                bindings,
                trait_method,
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
                trait_method,
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
                trait_method,
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
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            ),
        ),

        SourceType::Assoc { assoc_id, .. } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());

            let impl_alias_id = impl_.trait_alias_map().get(&assoc_id).cloned();

            if let Some(impl_alias_id) = impl_alias_id {
                let impl_alias = sa.alias(impl_alias_id);
                impl_alias.ty()
            } else {
                // Associated type is from a super trait, find the impl for that super trait
                let assoc_trait_id = assoc.parent.to_trait_id().expect("trait expected");

                // Build a TraitType for the super trait
                // We need to get the type arguments from the trait's bounds
                let super_trait_ty = find_super_trait_ty(sa, trait_ty, assoc_trait_id);

                if let Some(super_trait_ty) = super_trait_ty {
                    if let Some(impl_match) = find_impl(
                        sa,
                        trait_method,
                        extended_ty.clone(),
                        trait_method.type_param_definition(),
                        super_trait_ty.clone(),
                    ) {
                        let found_impl = sa.impl_(impl_match.id);
                        let ty = found_impl
                            .trait_alias_map()
                            .get(&assoc_id)
                            .map(|a| sa.alias(*a).ty())
                            .unwrap_or(SourceType::Error);
                        specialize_ty_for_default_trait_method(
                            sa,
                            ty,
                            trait_method,
                            found_impl,
                            &super_trait_ty,
                            extended_ty,
                        )
                    } else {
                        SourceType::Error
                    }
                } else {
                    SourceType::Error
                }
            }
        }

        SourceType::GenericAssoc {
            ty,
            trait_ty: local_trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let specialized_ty = specialize_ty_for_default_trait_method(
                sa,
                *ty,
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            );

            if specialized_ty.is_type_param() {
                SourceType::GenericAssoc {
                    ty: Box::new(specialized_ty),
                    trait_ty: local_trait_ty,
                    assoc_id,
                }
            } else if let Some(impl_match) = find_impl(
                sa,
                trait_method,
                specialized_ty.clone(),
                trait_method.type_param_definition(),
                local_trait_ty.clone(),
            ) {
                let found_impl = sa.impl_(impl_match.id);
                let ty = found_impl
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                specialize_ty_for_default_trait_method(
                    sa,
                    ty,
                    trait_method,
                    found_impl,
                    trait_ty,
                    extended_ty,
                )
            } else {
                unimplemented!()
            }
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_array_for_default_trait_method(
                sa,
                params,
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            ),
            Box::new(specialize_ty_for_default_trait_method(
                sa,
                *return_type,
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            )),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(specialize_ty_array_for_default_trait_method(
                sa,
                subtypes,
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            ))
        }

        SourceType::TypeParam(id) => {
            let trait_type_params = sa
                .trait_(trait_ty.trait_id)
                .type_param_definition()
                .type_param_count();

            if id.index() < trait_type_params {
                // This is a trait type parameter.
                trait_ty.type_params[id.index()].clone()
            } else {
                // This is a function-type parameter.
                let id = impl_.type_param_definition().type_param_count()
                    + (id.index() - trait_type_params);
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

/// Find the TraitType for a super trait in the trait's bounds.
/// This is used when an associated type comes from a super trait.
pub fn find_super_trait_ty(
    sa: &Sema,
    trait_ty: &TraitType,
    target_trait_id: TraitDefinitionId,
) -> Option<TraitType> {
    let trait_ = sa.trait_(trait_ty.trait_id);

    // Check if the target is the current trait
    if trait_ty.trait_id == target_trait_id {
        return Some(trait_ty.clone());
    }

    // Search in the trait's bounds for Self
    for bound in trait_.type_param_definition().bounds_for_self() {
        if bound.trait_id == target_trait_id {
            // Found it - specialize the type parameters
            let specialized_type_params = bound
                .type_params
                .iter()
                .map(|ty| replace_type(sa, ty, Some(&trait_ty.type_params), None))
                .collect::<Vec<_>>();
            return Some(TraitType {
                trait_id: target_trait_id,
                type_params: SourceTypeArray::with(specialized_type_params),
                bindings: Vec::new(),
            });
        }

        // Recursively search in super traits
        let super_trait_ty = TraitType {
            trait_id: bound.trait_id,
            type_params: bound.type_params.clone(),
            bindings: Vec::new(),
        };
        if let Some(found) = find_super_trait_ty(sa, &super_trait_ty, target_trait_id) {
            // Specialize the found trait type
            let specialized_type_params = found
                .type_params
                .iter()
                .map(|ty| replace_type(sa, ty, Some(&trait_ty.type_params), None))
                .collect::<Vec<_>>();
            return Some(TraitType {
                trait_id: target_trait_id,
                type_params: SourceTypeArray::with(specialized_type_params),
                bindings: Vec::new(),
            });
        }
    }

    None
}

fn specialize_ty_array_for_default_trait_method(
    sa: &Sema,
    array: SourceTypeArray,
    trait_method: &FctDefinition,
    impl_: &ImplDefinition,
    trait_ty: &TraitType,
    extended_ty: &SourceType,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| {
            specialize_ty_for_default_trait_method(
                sa,
                ty,
                trait_method,
                impl_,
                trait_ty,
                extended_ty,
            )
        })
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_generic(
    sa: &Sema,
    ty: SourceType,
    element: &dyn Element,
    type_param_id: TypeParamId,
    trait_ty: &TraitType,
    type_params: &SourceTypeArray,
    object_type: &SourceType,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_generic_array(
                sa,
                cls_type_params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_generic_array(
                sa,
                trait_type_params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
            specialize_ty_for_generic_array(
                sa,
                bindings,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_generic_array(
                sa,
                struct_type_params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_generic_array(
                sa,
                enum_type_params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_generic_array(
                sa,
                alias_type_params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
        ),

        SourceType::Assoc { assoc_id, .. } => {
            let alias = sa.alias(assoc_id);
            let assoc_trait_id = alias.parent.to_trait_id().expect("expected trait");

            // Find the appropriate trait type - either the current trait or a super trait
            let assoc_trait_ty = if assoc_trait_id == trait_ty.trait_id {
                trait_ty.clone()
            } else {
                find_super_trait_ty(sa, trait_ty, assoc_trait_id)
                    .expect("super trait not found for associated type")
            };

            if let Some((_, ty)) = assoc_trait_ty.bindings.iter().find(|(x, _)| *x == assoc_id) {
                ty.clone()
            } else {
                SourceType::GenericAssoc {
                    ty: Box::new(SourceType::TypeParam(type_param_id)),
                    trait_ty: assoc_trait_ty,
                    assoc_id,
                }
            }
        }

        SourceType::GenericAssoc {
            ty,
            trait_ty: local_trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let specialized_ty = specialize_ty_for_generic(
                sa,
                *ty,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            );

            if specialized_ty.is_type_param() {
                SourceType::GenericAssoc {
                    ty: Box::new(specialized_ty),
                    trait_ty: local_trait_ty,
                    assoc_id,
                }
            } else if let Some(impl_match) = find_impl(
                sa,
                element,
                specialized_ty,
                element.type_param_definition(),
                local_trait_ty.clone(),
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                specialize_ty_for_generic(
                    sa,
                    ty,
                    element,
                    type_param_id,
                    trait_ty,
                    type_params,
                    object_type,
                )
            } else {
                unimplemented!()
            }
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_for_generic_array(
                sa,
                params,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            ),
            Box::new(specialize_ty_for_generic(
                sa,
                *return_type,
                element,
                type_param_id,
                trait_ty,
                type_params,
                object_type,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_generic_array(
            sa,
            subtypes,
            element,
            type_param_id,
            trait_ty,
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

        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_generic_array(
    sa: &Sema,
    array: SourceTypeArray,
    element: &dyn Element,
    type_param_id: TypeParamId,
    trait_ty: &TraitType,
    trait_type_params: &SourceTypeArray,
    object_type: &SourceType,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| {
            specialize_ty_for_generic(
                sa,
                ty,
                element,
                type_param_id,
                trait_ty,
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
            ty,
            trait_ty,
            assoc_id,
        } => {
            let assoc = sa.alias(assoc_id);
            assert!(assoc.parent.is_trait());
            let specialized_ty = specialize_for_element(sa, *ty, element, type_params_for_element);

            if specialized_ty.is_type_param() {
                SourceType::GenericAssoc {
                    ty: Box::new(specialized_ty),
                    trait_ty,
                    assoc_id,
                }
            } else if let Some(impl_match) = find_impl(
                sa,
                element,
                specialized_ty,
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

        SourceType::Any | SourceType::Ptr | SourceType::Assoc { .. } => unreachable!(),
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

/// Helper function to specialize a TraitType by replacing type parameters.
/// This is used by TraitType::implements_trait.
pub fn specialize_trait_type_for_implements(
    trait_ty: TraitType,
    type_params: &SourceTypeArray,
) -> TraitType {
    if type_params.is_empty() {
        return trait_ty;
    }

    let new_type_params = trait_ty
        .type_params
        .iter()
        .map(|ty| specialize_type_for_implements(ty, type_params))
        .collect::<Vec<_>>();

    let new_bindings = trait_ty
        .bindings
        .iter()
        .map(|(id, ty)| (*id, specialize_type_for_implements(ty.clone(), type_params)))
        .collect();

    TraitType {
        trait_id: trait_ty.trait_id,
        type_params: SourceTypeArray::with(new_type_params),
        bindings: new_bindings,
    }
}

/// Helper function to specialize a SourceType by replacing type parameters.
pub fn specialize_type_for_implements(ty: SourceType, type_params: &SourceTypeArray) -> SourceType {
    match ty {
        SourceType::TypeParam(id) => type_params[id.index()].clone(),
        SourceType::Class(cls_id, cls_params) => {
            let new_params = specialize_type_array_for_implements(&cls_params, type_params);
            SourceType::Class(cls_id, new_params)
        }
        SourceType::Struct(struct_id, struct_params) => {
            let new_params = specialize_type_array_for_implements(&struct_params, type_params);
            SourceType::Struct(struct_id, new_params)
        }
        SourceType::Enum(enum_id, enum_params) => {
            let new_params = specialize_type_array_for_implements(&enum_params, type_params);
            SourceType::Enum(enum_id, new_params)
        }
        SourceType::TraitObject(trait_id, trait_params, bindings) => {
            let new_params = specialize_type_array_for_implements(&trait_params, type_params);
            let new_bindings = specialize_type_array_for_implements(&bindings, type_params);
            SourceType::TraitObject(trait_id, new_params, new_bindings)
        }
        SourceType::Alias(alias_id, alias_params) => {
            let new_params = specialize_type_array_for_implements(&alias_params, type_params);
            SourceType::Alias(alias_id, new_params)
        }
        SourceType::Assoc { trait_ty, assoc_id } => SourceType::Assoc {
            trait_ty: specialize_trait_type_for_implements(trait_ty, type_params),
            assoc_id,
        },
        SourceType::GenericAssoc {
            ty,
            trait_ty,
            assoc_id,
        } => SourceType::GenericAssoc {
            ty: Box::new(specialize_type_for_implements(*ty, type_params)),
            trait_ty: specialize_trait_type_for_implements(trait_ty, type_params),
            assoc_id,
        },
        SourceType::Tuple(subtypes) => {
            let new_subtypes = specialize_type_array_for_implements(&subtypes, type_params);
            SourceType::Tuple(new_subtypes)
        }
        SourceType::Lambda(params, return_type) => {
            let new_params = specialize_type_array_for_implements(&params, type_params);
            let new_return_type = specialize_type_for_implements(*return_type, type_params);
            SourceType::Lambda(new_params, Box::new(new_return_type))
        }
        // Types that don't need specialization
        SourceType::Unit
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::This
        | SourceType::Any
        | SourceType::Ptr => ty,
    }
}

fn specialize_type_array_for_implements(
    array: &SourceTypeArray,
    type_params: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_type_for_implements(ty, type_params))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}
