use crate::sema::{
    Element, ImplDefinition, ImplDefinitionId, Sema, TypeParamDefinition, block_matches_ty,
    match_arrays,
};
use crate::{
    SourceType, SourceTypeArray, TraitType, TypeArgs, specialize_trait_type, specialize_type,
};

pub fn impl_matches(
    sa: &Sema,
    check_ty: SourceType,
    check_element: &dyn Element,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = sa.impl_(impl_id);
    let bindings = block_matches_ty(
        sa,
        check_ty,
        check_element,
        check_type_param_defs,
        impl_.extended_ty(),
        impl_.type_param_definition(sa),
    );

    bindings.map(|bindings| SourceTypeArray::with(bindings))
}

pub fn implements_trait(
    sa: &Sema,
    check_ty: SourceType,
    check_element: &dyn Element,
    trait_ty: TraitType,
) -> bool {
    let check_ty = maybe_alias_ty(sa, check_ty);
    let check_type_param_defs = check_element.type_param_definition(sa);

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
        | SourceType::Struct(..)
        | SourceType::Enum(..)
        | SourceType::Class(..)
        | SourceType::Tuple(..)
        | SourceType::Unit
        | SourceType::TraitObject(..)
        | SourceType::Lambda(..) => {
            find_impl(sa, check_element, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(sa, tp_id, trait_ty),

        check_ty @ SourceType::Assoc { .. } | check_ty @ SourceType::GenericAssoc { .. } => {
            associated_type_bounds(sa, &check_ty, check_type_param_defs)
                .into_iter()
                .any(|bound| bound.implements_trait(sa, &trait_ty))
        }

        SourceType::Alias(..) | SourceType::Ref(..) => {
            unreachable!()
        }

        SourceType::Error => false,

        SourceType::This => {
            // First check the function's where clause bounds on Self
            for bound_trait_ty in check_type_param_defs.bounds_for_self(sa) {
                if bound_trait_ty.implements_trait(sa, &trait_ty) {
                    return true;
                }
            }

            // Then check the trait's super traits
            let fct = check_element.to_fct().expect("fct expected");
            let trait_id = fct.trait_id();
            let trait_ = sa.trait_(trait_id);

            // Create identity type params so that super traits can be properly specialized
            let definition = trait_.type_param_definition(sa);
            let type_params = definition.identity_type_params(sa);
            let self_trait_ty = TraitType {
                trait_id,
                type_params,
                bindings: Vec::new(),
            };

            self_trait_ty.implements_trait(sa, &trait_ty)
        }

        SourceType::Ptr | SourceType::Any => unreachable!(),
    }
}

pub fn associated_type_bounds(
    sa: &Sema,
    check_ty: &SourceType,
    type_param_definition: &TypeParamDefinition,
) -> Vec<TraitType> {
    let (assoc_trait_ty, assoc_id) = match check_ty {
        SourceType::Assoc { trait_ty, assoc_id }
        | SourceType::GenericAssoc {
            trait_ty, assoc_id, ..
        } => (trait_ty, *assoc_id),
        _ => unreachable!(),
    };

    let mut bounds = type_param_definition
        .bounds(sa)
        .filter(|bound| bound.ty() == *check_ty)
        .filter_map(|bound| bound.trait_ty())
        .collect::<Vec<_>>();

    let alias = sa.alias(assoc_id);
    let owner_trait_id = alias
        .parent
        .to_trait_id()
        .expect("associated type should belong to a trait");
    let owner_trait = sa.trait_(owner_trait_id);
    let type_args = TypeArgs::from_own(
        sa,
        owner_trait.type_param_definition(sa),
        &assoc_trait_ty.type_params,
    );

    bounds.extend(
        alias
            .bounds()
            .iter()
            .filter_map(|bound| bound.ty())
            .map(|bound| specialize_trait_type(sa, bound, &type_args)),
    );

    bounds
}

pub fn maybe_alias_ty(sa: &Sema, mut ty: SourceType) -> SourceType {
    loop {
        match ty {
            SourceType::Alias(id, type_params) => {
                assert!(type_params.is_empty());
                let alias = sa.alias(id);
                ty = alias.ty();
            }

            _ => return ty,
        }
    }
}

pub struct ImplMatch {
    pub id: ImplDefinitionId,
    pub bindings: SourceTypeArray,
}

pub fn find_impl(
    sa: &Sema,
    check_element: &dyn Element,
    check_ty: SourceType,
    check_type_param_definition: &TypeParamDefinition,
    trait_ty: TraitType,
) -> Option<ImplMatch> {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(impl_trait_ty) = impl_.trait_ty() {
            if impl_trait_ty.trait_id != trait_ty.trait_id {
                continue;
            }

            if let Some(opt_bindings) = block_matches_ty(
                sa,
                check_ty.clone(),
                check_element,
                check_type_param_definition,
                impl_.extended_ty(),
                impl_.type_param_definition(sa),
            ) {
                let mut bindings_for_types =
                    opt_bindings.iter().cloned().map(|t| Some(t)).collect();

                if !trait_ty_match(
                    sa,
                    impl_,
                    &impl_trait_ty,
                    &trait_ty,
                    check_element,
                    check_type_param_definition,
                    &mut bindings_for_types,
                ) {
                    continue;
                }

                return Some(ImplMatch {
                    id: impl_.id(),
                    bindings: SourceTypeArray::with(opt_bindings),
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
    check_element: &dyn Element,
    check_type_param_definition: &TypeParamDefinition,
    opt_bindings: &mut Vec<Option<SourceType>>,
) -> bool {
    assert_eq!(impl_trait_ty.trait_id, check_trait_ty.trait_id);
    assert_eq!(
        impl_trait_ty.type_params.len(),
        check_trait_ty.type_params.len()
    );

    if !match_arrays(
        sa,
        &check_trait_ty.type_params,
        check_element,
        check_type_param_definition,
        &impl_trait_ty.type_params,
        impl_.type_param_definition(sa),
        opt_bindings,
    ) {
        return false;
    }

    if opt_bindings.iter().any(|t| t.is_none()) {
        return false;
    }

    let bindings = SourceTypeArray::with(
        opt_bindings
            .clone()
            .into_iter()
            .map(|t| t.expect("missing binding"))
            .collect(),
    );
    let type_args = TypeArgs::from_own(sa, impl_.type_param_definition(sa), &bindings);

    let trait_alias_map = impl_.trait_alias_map();

    for (trait_alias_id, type_binding) in &check_trait_ty.bindings {
        let impl_alias_id = trait_alias_map.get(&trait_alias_id).expect("missing alias");
        let impl_alias_ty = sa.alias(*impl_alias_id).ty();
        let impl_alias_ty = specialize_type(sa, impl_alias_ty, &type_args);

        if type_binding != &impl_alias_ty {
            return false;
        }
    }

    true
}
