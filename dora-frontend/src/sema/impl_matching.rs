use crate::sema::{
    block_matches_ty, match_arrays, ImplDefinition, ImplDefinitionId, Sema, TypeParamDefinition,
};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn impl_matches(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = sa.impl_(impl_id);
    let bindings = block_matches_ty(
        sa,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty(),
        impl_.type_param_definition(),
    );

    bindings.map(|bindings| {
        SourceTypeArray::with(
            bindings
                .into_iter()
                .map(|t| t.expect("missing binding"))
                .collect(),
        )
    })
}

pub fn implements_trait(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: TraitType,
) -> bool {
    let check_ty = maybe_alias_ty(sa, check_ty);

    if check_ty.is_primitive() && sa.known.traits.zero() == trait_ty.trait_id {
        assert!(trait_ty.type_params.is_empty());
        return true;
    }

    match check_ty {
        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Struct(_, _)
        | SourceType::Enum(_, _)
        | SourceType::Class(_, _)
        | SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => {
            find_impl(sa, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(tp_id, trait_ty),

        SourceType::Alias(..) => unreachable!(),

        SourceType::Error => false,

        SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn maybe_alias_ty(sa: &Sema, mut ty: SourceType) -> SourceType {
    while let SourceType::Alias(id, type_params) = ty {
        assert!(type_params.is_empty());
        let alias = sa.alias(id);
        ty = alias.ty();
    }

    ty
}

pub struct ImplMatch {
    pub id: ImplDefinitionId,
    pub bindings: SourceTypeArray,
}

pub fn find_impl(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_definition: &TypeParamDefinition,
    trait_ty: TraitType,
) -> Option<ImplMatch> {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(impl_trait_ty) = impl_.trait_ty() {
            if impl_trait_ty.trait_id != trait_ty.trait_id {
                continue;
            }

            if let Some(mut bindings) = block_matches_ty(
                sa,
                check_ty.clone(),
                check_type_param_definition,
                impl_.extended_ty(),
                impl_.type_param_definition(),
            ) {
                if !trait_ty_match(
                    sa,
                    impl_,
                    &impl_trait_ty,
                    &trait_ty,
                    check_type_param_definition,
                    &mut bindings,
                ) {
                    continue;
                }

                let bindings = SourceTypeArray::with(
                    bindings
                        .into_iter()
                        .map(|t| t.expect("missing binding"))
                        .collect(),
                );

                return Some(ImplMatch {
                    id: impl_.id(),
                    bindings,
                });
            }
        }
    }

    None
}

fn trait_ty_match(
    sa: &Sema,
    impl_: &ImplDefinition,
    impl_trait_ty: &TraitType,
    check_trait_ty: &TraitType,
    check_type_param_definition: &TypeParamDefinition,
    bindings: &mut Vec<Option<SourceType>>,
) -> bool {
    assert_eq!(impl_trait_ty.trait_id, check_trait_ty.trait_id);
    assert_eq!(
        impl_trait_ty.type_params.len(),
        check_trait_ty.type_params.len()
    );

    if !match_arrays(
        sa,
        &check_trait_ty.type_params,
        check_type_param_definition,
        &impl_trait_ty.type_params,
        impl_.type_param_definition(),
        bindings,
    ) {
        return false;
    }

    let trait_alias_map = impl_.trait_alias_map();

    for (trait_alias_id, type_binding) in &check_trait_ty.bindings {
        let impl_alias_id = trait_alias_map.get(&trait_alias_id).expect("missing alias");
        let impl_alias_ty = sa.alias(*impl_alias_id).ty();

        if type_binding != &impl_alias_ty {
            return false;
        }
    }

    true
}
