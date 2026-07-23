use std::collections::HashMap;

use crate::sema::{
    Element, FctDefinition, ImplDefinition, Sema, SuperTraitWitness, TraitDefinitionId,
    TypeParamId, find_impl,
};
use crate::{SourceType, SourceTypeArray, TraitType, TypeArgs};

pub fn specialize_trait_type(sa: &Sema, ty: TraitType, type_args: &TypeArgs) -> TraitType {
    specialize_trait_type_generic(sa, ty, &|ty| replace_type(sa, ty, type_args))
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

pub fn specialize_type(sa: &Sema, ty: SourceType, type_args: &TypeArgs) -> SourceType {
    replace_type(sa, ty, type_args)
}

pub fn specialize_type_array(
    sa: &Sema,
    types: &SourceTypeArray,
    type_args: &TypeArgs,
) -> SourceTypeArray {
    let new_types = types
        .iter()
        .map(|ty| specialize_type(sa, ty, type_args))
        .collect();

    SourceTypeArray::with(new_types)
}

pub fn replace_type(sa: &Sema, ty: SourceType, type_args: &TypeArgs) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => {
            SourceType::Class(cls_id, replace_sta(sa, cls_type_params, type_args))
        }

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            replace_sta(sa, trait_type_params, type_args),
            replace_sta(sa, bindings, type_args),
        ),

        SourceType::Struct(struct_id, struct_type_params) => {
            SourceType::Struct(struct_id, replace_sta(sa, struct_type_params, type_args))
        }

        SourceType::Enum(enum_id, enum_type_params) => {
            SourceType::Enum(enum_id, replace_sta(sa, enum_type_params, type_args))
        }

        SourceType::Alias(alias_id, alias_type_params) => {
            SourceType::Alias(alias_id, replace_sta(sa, alias_type_params, type_args))
        }

        SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
            replace_sta(sa, params, type_args),
            Box::new(replace_type(sa, *return_type, type_args)),
            is_variadic,
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(replace_sta(sa, subtypes, type_args)),

        SourceType::This => type_args.self_ty().cloned().unwrap_or(ty),

        SourceType::TypeParam(id) => type_args[id].clone(),

        SourceType::Assoc { trait_ty, assoc_id } => SourceType::Assoc {
            trait_ty: specialize_trait_type(sa, trait_ty, type_args),
            assoc_id,
        },

        SourceType::GenericAssoc {
            ty,
            trait_ty,
            assoc_id,
        } => SourceType::GenericAssoc {
            ty: Box::new(replace_type(sa, *ty, type_args)),
            trait_ty: specialize_trait_type(sa, trait_ty, type_args),
            assoc_id,
        },

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::TypeVar(..) => ty,

        SourceType::Ref(inner) => SourceType::Ref(Box::new(replace_type(sa, *inner, type_args))),

        SourceType::Any | SourceType::Ptr => unreachable!(),
    }
}

fn replace_sta(sa: &Sema, array: SourceTypeArray, type_args: &TypeArgs) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| replace_type(sa, ty, type_args))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_call(
    sa: &Sema,
    ty: SourceType,
    caller_element: &dyn Element,
    type_params: &TypeArgs,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_call_array(sa, cls_type_params, caller_element, type_params),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_call_array(sa, trait_type_params, caller_element, type_params),
            specialize_ty_for_call_array(sa, bindings, caller_element, type_params),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_call_array(sa, struct_type_params, caller_element, type_params),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_call_array(sa, enum_type_params, caller_element, type_params),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_call_array(sa, alias_type_params, caller_element, type_params),
        ),

        SourceType::Assoc { assoc_id, .. } => {
            let alias = sa.alias(assoc_id);
            let object_ty = type_params.self_ty().expect("object_ty required for Assoc");

            match object_ty {
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
            let specialized_ty = specialize_ty_for_call(sa, *ty, caller_element, type_params);

            if matches!(specialized_ty, SourceType::Any | SourceType::TypeVar(_)) {
                SourceType::Any
            } else if specialized_ty.is_error() {
                SourceType::Error
            } else if specialized_ty.is_type_param() {
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
                let trait_param_count = trait_.type_param_definition(sa).type_param_count();
                let trait_type_params = type_params
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
                caller_element.type_param_definition(sa),
                trait_ty,
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                let type_args =
                    TypeArgs::from_own(sa, impl_.type_param_definition(sa), &impl_match.bindings);
                let ty = specialize_type(sa, ty, &type_args);
                specialize_ty_for_call(sa, ty, caller_element, type_params)
            } else {
                // This is reachable when an inferred or explicit type argument doesn't implement
                // the trait defining this associated type. Bounds are checked after inference.
                SourceType::Error
            }
        }

        SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
            specialize_ty_for_call_array(sa, params, caller_element, type_params),
            Box::new(specialize_ty_for_call(
                sa,
                *return_type,
                caller_element,
                type_params,
            )),
            is_variadic,
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_call_array(
            sa,
            subtypes,
            caller_element,
            type_params,
        )),

        SourceType::TypeParam(id) => type_params[id].clone(),

        SourceType::This => type_params.self_ty().cloned().unwrap_or(ty),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::TypeVar(..) => ty,

        SourceType::Ref(inner) => SourceType::Ref(Box::new(specialize_ty_for_call(
            sa,
            *inner,
            caller_element,
            type_params,
        ))),

        SourceType::Any => SourceType::Any,

        SourceType::Ptr => unreachable!(),
    }
}

fn specialize_ty_for_call_array(
    sa: &Sema,
    array: SourceTypeArray,
    caller_element: &dyn Element,
    type_params: &TypeArgs,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_call(sa, ty, caller_element, type_params))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_trait_object(
    sa: &Sema,
    ty: SourceType,
    trait_id: TraitDefinitionId,
    type_args: &TypeArgs,
    assoc_types: &SourceTypeArray,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_trait_object_array(
                sa,
                cls_type_params,
                trait_id,
                type_args,
                assoc_types,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_trait_object_array(
                sa,
                trait_type_params,
                trait_id,
                type_args,
                assoc_types,
            ),
            specialize_ty_for_trait_object_array(sa, bindings, trait_id, type_args, assoc_types),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_trait_object_array(
                sa,
                struct_type_params,
                trait_id,
                type_args,
                assoc_types,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_trait_object_array(
                sa,
                enum_type_params,
                trait_id,
                type_args,
                assoc_types,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_trait_object_array(
                sa,
                alias_type_params,
                trait_id,
                type_args,
                assoc_types,
            ),
        ),

        SourceType::Assoc { assoc_id, .. } => {
            let alias = sa.alias(assoc_id);
            assoc_types[alias.idx_in_trait()].clone()
        }

        SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
            specialize_ty_for_trait_object_array(sa, params, trait_id, type_args, assoc_types),
            Box::new(specialize_ty_for_trait_object(
                sa,
                *return_type,
                trait_id,
                type_args,
                assoc_types,
            )),
            is_variadic,
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_trait_object_array(
            sa,
            subtypes,
            trait_id,
            type_args,
            assoc_types,
        )),

        SourceType::TypeParam(id) => type_args[id].clone(),

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

        SourceType::This
        | SourceType::Any
        | SourceType::Ptr
        | SourceType::Ref(..)
        | SourceType::TypeVar(..) => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_trait_object_array(
    sa: &Sema,
    array: SourceTypeArray,
    trait_id: TraitDefinitionId,
    type_args: &TypeArgs,
    assoc_types: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_trait_object(sa, ty, trait_id, type_args, assoc_types))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

#[derive(Clone)]
struct TypeParamSubstitution {
    bindings: HashMap<TypeParamId, SourceType>,
}

impl TypeParamSubstitution {
    fn new() -> TypeParamSubstitution {
        TypeParamSubstitution {
            bindings: HashMap::new(),
        }
    }

    fn insert(&mut self, type_param_id: TypeParamId, ty: SourceType) {
        let previous = self.bindings.insert(type_param_id, ty.clone());
        assert!(previous.is_none() || previous == Some(ty));
    }

    fn insert_definition(
        &mut self,
        sa: &Sema,
        definition: &crate::sema::TypeParamDefinition,
        type_args: &SourceTypeArray,
    ) {
        assert_eq!(definition.type_param_count(), type_args.len());

        for ((type_param_id, _), ty) in definition.names(sa).zip(type_args.iter()) {
            self.insert(type_param_id, ty);
        }
    }

    fn insert_own_definition(
        &mut self,
        sa: &Sema,
        definition: &crate::sema::TypeParamDefinition,
        type_args: &SourceTypeArray,
    ) {
        assert_eq!(definition.own_type_params_len(), type_args.len());

        for ((type_param_id, _), ty) in definition
            .names(sa)
            .skip(definition.container_type_params())
            .zip(type_args.iter())
        {
            self.insert(type_param_id, ty);
        }
    }

    fn insert_bindings(&mut self, bindings: &HashMap<TypeParamId, SourceType>) {
        for (&type_param_id, ty) in bindings {
            self.insert(type_param_id, ty.clone());
        }
    }

    fn get(&self, type_param_id: TypeParamId) -> SourceType {
        self.bindings
            .get(&type_param_id)
            .cloned()
            .expect("type parameter missing from default trait method substitution")
    }
}

pub(crate) struct DefaultTraitMethodSpecialization<'a> {
    sa: &'a Sema,
    type_param_context: &'a dyn Element,
    impl_: &'a ImplDefinition,
    extended_ty: SourceType,
    type_params: TypeParamSubstitution,
}

impl<'a> DefaultTraitMethodSpecialization<'a> {
    pub(crate) fn new(
        sa: &'a Sema,
        trait_method: &'a FctDefinition,
        impl_: &'a ImplDefinition,
        trait_ty: &'a TraitType,
        extended_ty: &'a SourceType,
        adapter_method_type_params: &SourceTypeArray,
    ) -> DefaultTraitMethodSpecialization<'a> {
        let mut type_params = TypeParamSubstitution::new();
        let trait_definition = sa.trait_(trait_ty.trait_id).type_param_definition(sa);
        // Rewrite the trait's type parameters to the type arguments from the implemented trait.
        type_params.insert_definition(sa, trait_definition, &trait_ty.type_params);
        // Rewrite the trait method's own type parameters to the adapter method's fresh parameters.
        type_params.insert_own_definition(
            sa,
            trait_method.type_param_definition(sa),
            adapter_method_type_params,
        );

        DefaultTraitMethodSpecialization {
            sa,
            type_param_context: trait_method,
            impl_,
            extended_ty: extended_ty.clone(),
            type_params,
        }
    }

    pub(crate) fn specialize(&self, ty: SourceType) -> SourceType {
        match ty {
            SourceType::Class(cls_id, cls_type_params) => {
                SourceType::Class(cls_id, self.specialize_array(cls_type_params))
            }

            SourceType::TraitObject(trait_id, trait_type_params, bindings) => {
                SourceType::TraitObject(
                    trait_id,
                    self.specialize_array(trait_type_params),
                    self.specialize_array(bindings),
                )
            }

            SourceType::Struct(struct_id, struct_type_params) => {
                SourceType::Struct(struct_id, self.specialize_array(struct_type_params))
            }

            SourceType::Enum(enum_id, enum_type_params) => {
                SourceType::Enum(enum_id, self.specialize_array(enum_type_params))
            }

            SourceType::Alias(alias_id, alias_type_params) => {
                SourceType::Alias(alias_id, self.specialize_array(alias_type_params))
            }

            SourceType::Assoc { trait_ty, assoc_id } => {
                let assoc = self.sa.alias(assoc_id);
                assert!(assoc.parent.is_trait());

                let impl_alias_id = self.impl_.trait_alias_map().get(&assoc_id).cloned();

                if let Some(impl_alias_id) = impl_alias_id {
                    let impl_alias = self.sa.alias(impl_alias_id);
                    impl_alias.ty()
                } else {
                    let trait_ty = self.specialize_trait_ty(trait_ty);
                    assert_eq!(
                        trait_ty.trait_id,
                        assoc.parent.to_trait_id().expect("trait expected")
                    );

                    if let Some((_, ty)) = trait_ty.bindings.iter().find(|(id, _)| *id == assoc_id)
                    {
                        return ty.clone();
                    }

                    // Super-trait validation recorded how this exact specialized obligation was
                    // satisfied before default method adapters were created.
                    match self.impl_.super_trait_witness(&trait_ty) {
                        Some(SuperTraitWitness::Impl {
                            impl_id,
                            type_param_bindings,
                        }) => {
                            let found_impl = self.sa.impl_(*impl_id);
                            let ty = found_impl
                                .trait_alias_map()
                                .get(&assoc_id)
                                .map(|a| self.sa.alias(*a).ty())
                                .unwrap_or(SourceType::Error);
                            let mut type_params = TypeParamSubstitution::new();
                            type_params.insert_bindings(type_param_bindings);
                            let nested = self.new_for_impl(found_impl, type_params);
                            nested.specialize(ty)
                        }

                        Some(SuperTraitWitness::TypeParamBound { type_param_id }) => {
                            // No concrete impl is known until the adapter's type parameter is
                            // instantiated, so preserve the associated-type projection.
                            SourceType::GenericAssoc {
                                ty: Box::new(SourceType::TypeParam(*type_param_id)),
                                trait_ty,
                                assoc_id,
                            }
                        }

                        Some(SuperTraitWitness::Intrinsic) | None => SourceType::Error,
                    }
                }
            }

            SourceType::GenericAssoc {
                ty,
                trait_ty: local_trait_ty,
                assoc_id,
            } => {
                let assoc = self.sa.alias(assoc_id);
                assert!(assoc.parent.is_trait());
                let specialized_ty = self.specialize(*ty);

                if specialized_ty.is_type_param() {
                    SourceType::GenericAssoc {
                        ty: Box::new(specialized_ty),
                        trait_ty: local_trait_ty,
                        assoc_id,
                    }
                } else if let Some(impl_match) = find_impl(
                    self.sa,
                    self.type_param_context,
                    specialized_ty.clone(),
                    self.type_param_context.type_param_definition(self.sa),
                    local_trait_ty.clone(),
                ) {
                    let found_impl = self.sa.impl_(impl_match.id);
                    let ty = found_impl
                        .trait_alias_map()
                        .get(&assoc_id)
                        .map(|a| self.sa.alias(*a).ty())
                        .unwrap_or(SourceType::Error);
                    let mut type_params = self.type_params.clone();
                    type_params.insert_definition(
                        self.sa,
                        found_impl.type_param_definition(self.sa),
                        &impl_match.bindings,
                    );
                    let nested = self.new_for_impl(found_impl, type_params);
                    nested.specialize(ty)
                } else {
                    unimplemented!()
                }
            }

            SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
                self.specialize_array(params),
                Box::new(self.specialize(*return_type)),
                is_variadic,
            ),

            SourceType::Tuple(subtypes) => SourceType::Tuple(self.specialize_array(subtypes)),

            SourceType::Ref(inner) => SourceType::Ref(Box::new(self.specialize(*inner))),

            SourceType::TypeParam(type_param_id) => self.type_params.get(type_param_id),

            SourceType::Unit
            | SourceType::UInt8
            | SourceType::Bool
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Error => ty,

            SourceType::This => self.extended_ty.clone(),

            SourceType::Any | SourceType::Ptr | SourceType::TypeVar(..) => {
                unreachable!()
            }
        }
    }

    fn new_for_impl<'b>(
        &'b self,
        impl_: &'b ImplDefinition,
        type_params: TypeParamSubstitution,
    ) -> DefaultTraitMethodSpecialization<'b> {
        DefaultTraitMethodSpecialization {
            sa: self.sa,
            type_param_context: impl_,
            impl_,
            extended_ty: self.extended_ty.clone(),
            type_params,
        }
    }

    fn specialize_trait_ty(&self, trait_ty: TraitType) -> TraitType {
        specialize_trait_type_generic(self.sa, trait_ty, &|ty| self.specialize(ty))
    }

    fn specialize_array(&self, array: SourceTypeArray) -> SourceTypeArray {
        SourceTypeArray::with(array.iter().map(|ty| self.specialize(ty)).collect())
    }
}

/// Find the TraitType for a super trait in the trait's bounds.
/// This is used when an associated type comes from a super trait.
pub fn find_super_trait_ty(
    sa: &Sema,
    trait_ty: &TraitType,
    target_trait_id: TraitDefinitionId,
) -> Option<TraitType> {
    // Check if the target is the current trait
    if trait_ty.trait_id == target_trait_id {
        return Some(trait_ty.clone());
    }

    let trait_ = sa.trait_(trait_ty.trait_id);
    let type_param_definition = trait_.type_param_definition(sa);
    let type_args = TypeArgs::from_own(sa, type_param_definition, &trait_ty.type_params);

    // Search in the trait's bounds for Self
    for bound in type_param_definition.bounds_for_self(sa) {
        let bound = specialize_trait_type(sa, bound.clone(), &type_args);

        if bound.trait_id == target_trait_id {
            return Some(bound);
        }

        if let Some(found) = find_super_trait_ty(sa, &bound, target_trait_id) {
            return Some(found);
        }
    }

    None
}

pub fn specialize_ty_for_generic(
    sa: &Sema,
    ty: SourceType,
    element: &dyn Element,
    type_param_id: TypeParamId,
    trait_ty: &TraitType,
    type_params: &TypeArgs,
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
            ),
            specialize_ty_for_generic_array(
                sa,
                bindings,
                element,
                type_param_id,
                trait_ty,
                type_params,
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
            let specialized_ty =
                specialize_ty_for_generic(sa, *ty, element, type_param_id, trait_ty, type_params);

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
                element.type_param_definition(sa),
                local_trait_ty.clone(),
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                specialize_ty_for_generic(sa, ty, element, type_param_id, trait_ty, type_params)
            } else {
                unimplemented!()
            }
        }

        SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
            specialize_ty_for_generic_array(
                sa,
                params,
                element,
                type_param_id,
                trait_ty,
                type_params,
            ),
            Box::new(specialize_ty_for_generic(
                sa,
                *return_type,
                element,
                type_param_id,
                trait_ty,
                type_params,
            )),
            is_variadic,
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_generic_array(
            sa,
            subtypes,
            element,
            type_param_id,
            trait_ty,
            type_params,
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

        SourceType::TypeParam(id) => type_params[id].clone(),

        SourceType::This => type_params
            .self_ty()
            .cloned()
            .expect("Self type required for generic specialization"),

        SourceType::Any | SourceType::Ptr | SourceType::Ref(..) | SourceType::TypeVar(..) => {
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
    type_params: &TypeArgs,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_generic(sa, ty, element, type_param_id, trait_ty, type_params))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_for_element(
    sa: &Sema,
    ty: SourceType,
    element: &dyn Element,
    type_args: &TypeArgs,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_for_element_array(sa, cls_type_params, element, type_args),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_for_element_array(sa, trait_type_params, element, type_args),
            specialize_for_element_array(sa, bindings, element, type_args),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_for_element_array(sa, struct_type_params, element, type_args),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_for_element_array(sa, enum_type_params, element, type_args),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_for_element_array(sa, alias_type_params, element, type_args),
        ),

        SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
            specialize_for_element_array(sa, params, element, type_args),
            Box::new(specialize_for_element(sa, *return_type, element, type_args)),
            is_variadic,
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_for_element_array(
            sa, subtypes, element, type_args,
        )),

        SourceType::This => type_args.self_ty().cloned().unwrap_or_else(|| {
            assert!(element.is_trait());
            SourceType::This
        }),

        SourceType::TypeParam(id) => type_args[id].clone(),

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
            let specialized_ty = specialize_for_element(sa, *ty, element, type_args);

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
                element.type_param_definition(sa),
                trait_ty,
            ) {
                let impl_ = sa.impl_(impl_match.id);
                let ty = impl_
                    .trait_alias_map()
                    .get(&assoc_id)
                    .map(|a| sa.alias(*a).ty())
                    .unwrap_or(SourceType::Error);
                specialize_for_element(sa, ty, element, type_args)
            } else {
                unimplemented!()
            }
        }

        SourceType::Any
        | SourceType::Ptr
        | SourceType::Assoc { .. }
        | SourceType::Ref(..)
        | SourceType::TypeVar(..) => {
            unreachable!()
        }
    }
}

fn specialize_for_element_array(
    sa: &Sema,
    array: SourceTypeArray,
    element: &dyn Element,
    type_args: &TypeArgs,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_for_element(sa, ty, element, type_args))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

/// Helper function to specialize a TraitType by replacing type parameters.
/// This is used by TraitType::implements_trait.
pub fn specialize_trait_type_for_implements(
    trait_ty: TraitType,
    type_args: &TypeArgs,
) -> TraitType {
    if type_args.is_empty() {
        return trait_ty;
    }

    let new_type_params = trait_ty
        .type_params
        .iter()
        .map(|ty| specialize_type_for_implements(ty, type_args))
        .collect::<Vec<_>>();

    let new_bindings = trait_ty
        .bindings
        .iter()
        .map(|(id, ty)| (*id, specialize_type_for_implements(ty.clone(), type_args)))
        .collect();

    TraitType {
        trait_id: trait_ty.trait_id,
        type_params: SourceTypeArray::with(new_type_params),
        bindings: new_bindings,
    }
}

/// Helper function to specialize a SourceType by replacing type parameters.
pub fn specialize_type_for_implements(ty: SourceType, type_args: &TypeArgs) -> SourceType {
    match ty {
        SourceType::TypeParam(id) => type_args[id].clone(),
        SourceType::Class(cls_id, cls_params) => {
            let new_params = specialize_type_array_for_implements(&cls_params, type_args);
            SourceType::Class(cls_id, new_params)
        }
        SourceType::Struct(struct_id, struct_params) => {
            let new_params = specialize_type_array_for_implements(&struct_params, type_args);
            SourceType::Struct(struct_id, new_params)
        }
        SourceType::Enum(enum_id, enum_params) => {
            let new_params = specialize_type_array_for_implements(&enum_params, type_args);
            SourceType::Enum(enum_id, new_params)
        }
        SourceType::TraitObject(trait_id, trait_params, bindings) => {
            let new_params = specialize_type_array_for_implements(&trait_params, type_args);
            let new_bindings = specialize_type_array_for_implements(&bindings, type_args);
            SourceType::TraitObject(trait_id, new_params, new_bindings)
        }
        SourceType::Alias(alias_id, alias_params) => {
            let new_params = specialize_type_array_for_implements(&alias_params, type_args);
            SourceType::Alias(alias_id, new_params)
        }
        SourceType::Assoc { trait_ty, assoc_id } => SourceType::Assoc {
            trait_ty: specialize_trait_type_for_implements(trait_ty, type_args),
            assoc_id,
        },
        SourceType::GenericAssoc {
            ty,
            trait_ty,
            assoc_id,
        } => SourceType::GenericAssoc {
            ty: Box::new(specialize_type_for_implements(*ty, type_args)),
            trait_ty: specialize_trait_type_for_implements(trait_ty, type_args),
            assoc_id,
        },
        SourceType::Tuple(subtypes) => {
            let new_subtypes = specialize_type_array_for_implements(&subtypes, type_args);
            SourceType::Tuple(new_subtypes)
        }
        SourceType::Lambda(params, return_type, is_variadic) => {
            let new_params = specialize_type_array_for_implements(&params, type_args);
            let new_return_type = specialize_type_for_implements(*return_type, type_args);
            SourceType::Lambda(new_params, Box::new(new_return_type), is_variadic)
        }
        SourceType::Ref(inner) => {
            SourceType::Ref(Box::new(specialize_type_for_implements(*inner, type_args)))
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
        | SourceType::Any
        | SourceType::Ptr => ty,

        SourceType::This => type_args.self_ty().cloned().unwrap_or(ty),

        SourceType::TypeVar(..) => unreachable!(),
    }
}

fn specialize_type_array_for_implements(
    array: &SourceTypeArray,
    type_args: &TypeArgs,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_type_for_implements(ty, type_args))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}
