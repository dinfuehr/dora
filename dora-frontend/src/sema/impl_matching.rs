use std::collections::{HashMap, HashSet};

use crate::sema::{
    Element, ExtensionDefinition, ImplDefinition, ImplDefinitionId, Sema, TypeParamDefinition,
    TypeParamId,
};
use crate::{
    SourceType, SourceTypeArray, TraitType, TypeArgs, specialize_trait_type, specialize_type,
};

use super::matching::{block_matches_ty, block_matches_ty_with_context, match_arrays_with_context};

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
    let mut context = TraitMatchingContext::default();
    implements_trait_with_context(sa, check_ty, check_element, trait_ty, &mut context)
}

pub(super) fn implements_trait_with_context(
    sa: &Sema,
    check_ty: SourceType,
    check_element: &dyn Element,
    trait_ty: TraitType,
    context: &mut TraitMatchingContext,
) -> bool {
    let check_ty = maybe_alias_ty(sa, check_ty);
    let query = (check_ty.clone(), trait_ty.clone());

    if !context.active_trait_queries.insert(query.clone()) {
        // A recursive bound does not prove itself. Return false for this candidate
        // so that trait lookup can continue with another implementation.
        return false;
    }

    let result = implements_trait_inner(sa, check_ty, check_element, trait_ty, context);
    assert!(context.active_trait_queries.remove(&query));
    result
}

fn implements_trait_inner(
    sa: &Sema,
    check_ty: SourceType,
    check_element: &dyn Element,
    trait_ty: TraitType,
    context: &mut TraitMatchingContext,
) -> bool {
    let check_type_param_defs = check_element.type_param_definition(sa);

    if check_ty.is_primitive() && sa.known.traits.zero() == trait_ty.trait_id {
        assert!(trait_ty.type_params.is_empty());
        return true;
    }

    match check_ty {
        SourceType::Never
        | SourceType::Bool
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
        | SourceType::Lambda(..) => find_impl_with_context(
            sa,
            check_element,
            check_ty,
            check_type_param_defs,
            trait_ty,
            context,
        )
        .is_some(),

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(sa, tp_id, trait_ty),

        check_ty @ SourceType::Assoc { .. } => {
            associated_type_bounds(sa, &check_ty, check_type_param_defs)
                .into_iter()
                .any(|bound| bound.implements_trait(sa, &trait_ty))
        }

        SourceType::Alias(..) | SourceType::Ref { .. } => {
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

        SourceType::Any | SourceType::TypeVar(..) => unreachable!(),
    }
}

#[derive(Default)]
pub(super) struct TraitMatchingContext {
    active_trait_queries: HashSet<(SourceType, TraitType)>,
}

pub fn associated_type_bounds(
    sa: &Sema,
    check_ty: &SourceType,
    type_param_definition: &TypeParamDefinition,
) -> Vec<TraitType> {
    let (assoc_trait_ty, assoc_id) = match check_ty {
        SourceType::Assoc {
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
    let mut context = TraitMatchingContext::default();
    find_impl_with_context(
        sa,
        check_element,
        check_ty,
        check_type_param_definition,
        trait_ty,
        &mut context,
    )
}

fn find_impl_with_context(
    sa: &Sema,
    check_element: &dyn Element,
    check_ty: SourceType,
    check_type_param_definition: &TypeParamDefinition,
    trait_ty: TraitType,
    context: &mut TraitMatchingContext,
) -> Option<ImplMatch> {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(impl_trait_ty) = impl_.trait_ty() {
            if impl_trait_ty.trait_id != trait_ty.trait_id {
                continue;
            }

            if let Some(opt_bindings) = block_matches_ty_with_context(
                sa,
                check_ty.clone(),
                check_element,
                check_type_param_definition,
                impl_.extended_ty(),
                impl_.type_param_definition(sa),
                context,
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
                    context,
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

pub fn impls_overlap(sa: &Sema, first: &ImplDefinition, second: &ImplDefinition) -> bool {
    let Some(first_trait_ty) = first.trait_ty() else {
        return false;
    };
    let Some(second_trait_ty) = second.trait_ty() else {
        return false;
    };

    if first_trait_ty.trait_id != second_trait_ty.trait_id {
        return false;
    }

    let mut unifier = TypeUnifier::new();

    if !unifier.unify(first.extended_ty(), second.extended_ty())
        || !unifier.unify_trait_types(first_trait_ty, second_trait_ty)
    {
        return false;
    }

    // For example, unifying these impls binds `T` to `Int32`, so overlap is only
    // possible if `Int32` implements `Bound`:
    //
    //     impl[T: Bound] Trait for Value[T] {}
    //     impl Trait for Value[Int32] {}
    //
    // In contrast, unifying these impls only equates `T` and `U`. A type could
    // implement both `First` and `Second`, so the remaining bounds do not rule
    // out overlap:
    //
    //     impl[T: First] Trait for Value[T] {}
    //     impl[U: Second] Trait for Value[U] {}
    type_param_bounds_allow_overlap(sa, first, &unifier)
        && type_param_bounds_allow_overlap(sa, second, &unifier)
}

pub fn extensions_overlap(
    sa: &Sema,
    first: &ExtensionDefinition,
    second: &ExtensionDefinition,
) -> bool {
    let mut unifier = TypeUnifier::new();

    if !unifier.unify(first.ty(), second.ty()) {
        return false;
    }

    // Generic bounds do not rule out an overlap when one type could implement
    // both of them:
    //
    //     impl[T: First] Vec[T] { fn foo() {} }
    //     impl[U: Second] Vec[U] { fn foo() {} }
    //
    // If unification resolves a parameter to a concrete type, however, the
    // extensions overlap only when that type satisfies the bound:
    //
    //     impl[T: Bound] Vec[T] { fn foo() {} }
    //     impl Vec[Int32] { fn foo() {} }
    type_param_bounds_allow_overlap(sa, first, &unifier)
        && type_param_bounds_allow_overlap(sa, second, &unifier)
}

fn type_param_bounds_allow_overlap(
    sa: &Sema,
    element: &dyn Element,
    unifier: &TypeUnifier,
) -> bool {
    let type_param_definition = element.type_param_definition(sa);
    let bindings = type_param_definition
        .identity_type_params(sa)
        .iter()
        .map(|ty| unifier.resolve(ty))
        .collect::<Vec<_>>();
    let type_args = TypeArgs::from_own(sa, type_param_definition, &SourceTypeArray::with(bindings));

    for bound in type_param_definition.bounds(sa) {
        let Some(trait_ty) = bound.trait_ty() else {
            continue;
        };
        let bound_ty = specialize_type(sa, bound.ty(), &type_args);
        let trait_ty = specialize_trait_type(sa, trait_ty, &type_args);

        if bound_ty.contains_type_param() || trait_ty.contains_type_param() {
            continue;
        }

        if !implements_trait(sa, bound_ty, element, trait_ty) {
            return false;
        }
    }

    true
}

struct TypeUnifier {
    bindings: HashMap<TypeParamId, SourceType>,
}

impl TypeUnifier {
    fn new() -> TypeUnifier {
        TypeUnifier {
            bindings: HashMap::new(),
        }
    }

    fn unify(&mut self, first: SourceType, second: SourceType) -> bool {
        let first = self.resolve(first);
        let second = self.resolve(second);

        match (first, second) {
            (SourceType::Error, _) | (_, SourceType::Error) => false,
            (SourceType::Any | SourceType::TypeVar(_), _)
            | (_, SourceType::Any | SourceType::TypeVar(_)) => {
                unreachable!("unexpected inference type in generic declaration")
            }
            (SourceType::TypeParam(id), ty) => self.bind(id, ty),
            (ty, SourceType::TypeParam(id)) => self.bind(id, ty),
            (
                SourceType::Class(first_id, first_params),
                SourceType::Class(second_id, second_params),
            ) => first_id == second_id && self.unify_arrays(first_params, second_params),
            (
                SourceType::Struct(first_id, first_params),
                SourceType::Struct(second_id, second_params),
            ) => first_id == second_id && self.unify_arrays(first_params, second_params),
            (
                SourceType::Enum(first_id, first_params),
                SourceType::Enum(second_id, second_params),
            ) => first_id == second_id && self.unify_arrays(first_params, second_params),
            (
                SourceType::Alias(first_id, first_params),
                SourceType::Alias(second_id, second_params),
            ) => first_id == second_id && self.unify_arrays(first_params, second_params),
            (SourceType::Tuple(first_params), SourceType::Tuple(second_params)) => {
                self.unify_arrays(first_params, second_params)
            }
            (
                SourceType::TraitObject(first_id, first_params, first_bindings),
                SourceType::TraitObject(second_id, second_params, second_bindings),
            ) => {
                first_id == second_id
                    && self.unify_arrays(first_params, second_params)
                    && self.unify_arrays(first_bindings, second_bindings)
            }
            (
                SourceType::Assoc {
                    ty: first_ty,
                    trait_ty: first_trait_ty,
                    assoc_id: first_assoc_id,
                },
                SourceType::Assoc {
                    ty: second_ty,
                    trait_ty: second_trait_ty,
                    assoc_id: second_assoc_id,
                },
            ) => {
                first_assoc_id == second_assoc_id
                    && self.unify(*first_ty, *second_ty)
                    && self.unify_trait_types(first_trait_ty, second_trait_ty)
            }
            (
                SourceType::Lambda(first_params, first_return, first_variadic),
                SourceType::Lambda(second_params, second_return, second_variadic),
            ) => {
                first_variadic == second_variadic
                    && self.unify_arrays(first_params, second_params)
                    && self.unify(*first_return, *second_return)
            }
            (
                SourceType::Ref {
                    ty: first,
                    is_mut: first_is_mut,
                },
                SourceType::Ref {
                    ty: second,
                    is_mut: second_is_mut,
                },
            ) => first_is_mut == second_is_mut && self.unify(*first, *second),
            (first, second) => first == second,
        }
    }

    fn unify_arrays(&mut self, first: SourceTypeArray, second: SourceTypeArray) -> bool {
        first.len() == second.len()
            && first
                .iter()
                .zip(second.iter())
                .all(|(first, second)| self.unify(first, second))
    }

    fn unify_trait_types(&mut self, first: TraitType, second: TraitType) -> bool {
        first.trait_id == second.trait_id
            && self.unify_arrays(first.type_params, second.type_params)
            && first.bindings.len() == second.bindings.len()
            && first.bindings.into_iter().zip(second.bindings).all(
                |((first_id, first_ty), (second_id, second_ty))| {
                    first_id == second_id && self.unify(first_ty, second_ty)
                },
            )
    }

    fn bind(&mut self, id: TypeParamId, ty: SourceType) -> bool {
        if ty == SourceType::TypeParam(id) {
            return true;
        }

        // Reject recursive bindings such as `T = Foo[T]`. They have no finite
        // solution and would make recursive type resolution loop indefinitely.
        if self.contains_type_param(&ty, id) {
            return false;
        }

        self.bindings.insert(id, ty);
        true
    }

    fn resolve(&self, ty: SourceType) -> SourceType {
        match ty {
            SourceType::TypeParam(id) => match self.bindings.get(&id) {
                Some(binding) => self.resolve(binding.clone()),
                None => SourceType::TypeParam(id),
            },
            SourceType::Class(id, params) => SourceType::Class(id, self.resolve_array(params)),
            SourceType::Struct(id, params) => SourceType::Struct(id, self.resolve_array(params)),
            SourceType::Enum(id, params) => SourceType::Enum(id, self.resolve_array(params)),
            SourceType::Alias(id, params) => SourceType::Alias(id, self.resolve_array(params)),
            SourceType::Tuple(params) => SourceType::Tuple(self.resolve_array(params)),
            SourceType::TraitObject(id, params, bindings) => SourceType::TraitObject(
                id,
                self.resolve_array(params),
                self.resolve_array(bindings),
            ),
            SourceType::Assoc {
                ty,
                trait_ty,
                assoc_id,
            } => SourceType::Assoc {
                ty: Box::new(self.resolve(*ty)),
                trait_ty: self.resolve_trait_type(trait_ty),
                assoc_id,
            },
            SourceType::Lambda(params, return_type, variadic) => SourceType::Lambda(
                self.resolve_array(params),
                Box::new(self.resolve(*return_type)),
                variadic,
            ),
            SourceType::Ref { ty, is_mut } => SourceType::Ref {
                ty: Box::new(self.resolve(*ty)),
                is_mut,
            },
            ty @ (SourceType::Never
            | SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::This
            | SourceType::Error) => ty,
            SourceType::Any | SourceType::TypeVar(_) => {
                unreachable!("unexpected inference type in generic declaration")
            }
        }
    }

    fn resolve_array(&self, types: SourceTypeArray) -> SourceTypeArray {
        SourceTypeArray::with(types.iter().map(|ty| self.resolve(ty)).collect())
    }

    fn resolve_trait_type(&self, trait_ty: TraitType) -> TraitType {
        TraitType {
            trait_id: trait_ty.trait_id,
            type_params: self.resolve_array(trait_ty.type_params),
            bindings: trait_ty
                .bindings
                .into_iter()
                .map(|(id, ty)| (id, self.resolve(ty)))
                .collect(),
        }
    }

    fn contains_type_param(&self, ty: &SourceType, expected_id: TypeParamId) -> bool {
        let ty = self.resolve(ty.clone());

        match ty {
            SourceType::TypeParam(id) => id == expected_id,
            SourceType::Class(_, params)
            | SourceType::Struct(_, params)
            | SourceType::Enum(_, params)
            | SourceType::Alias(_, params)
            | SourceType::Tuple(params) => params
                .iter()
                .any(|ty| self.contains_type_param(&ty, expected_id)),
            SourceType::TraitObject(_, params, bindings) => {
                params
                    .iter()
                    .any(|ty| self.contains_type_param(&ty, expected_id))
                    || bindings
                        .iter()
                        .any(|ty| self.contains_type_param(&ty, expected_id))
            }
            SourceType::Assoc { ty, trait_ty, .. } => {
                self.contains_type_param(&ty, expected_id)
                    || self.trait_contains_type_param(&trait_ty, expected_id)
            }
            SourceType::Lambda(params, return_type, _) => {
                params
                    .iter()
                    .any(|ty| self.contains_type_param(&ty, expected_id))
                    || self.contains_type_param(&return_type, expected_id)
            }
            SourceType::Ref { ty, .. } => self.contains_type_param(&ty, expected_id),
            SourceType::Never
            | SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::This
            | SourceType::Error => false,
            SourceType::Any | SourceType::TypeVar(_) => {
                unreachable!("unexpected inference type in generic declaration")
            }
        }
    }

    fn trait_contains_type_param(&self, trait_ty: &TraitType, expected_id: TypeParamId) -> bool {
        trait_ty
            .type_params
            .iter()
            .any(|ty| self.contains_type_param(&ty, expected_id))
            || trait_ty
                .bindings
                .iter()
                .any(|(_, ty)| self.contains_type_param(ty, expected_id))
    }
}

fn trait_ty_match(
    sa: &Sema,
    impl_: &ImplDefinition,
    impl_trait_ty: &TraitType,
    check_trait_ty: &TraitType,
    check_element: &dyn Element,
    check_type_param_definition: &TypeParamDefinition,
    opt_bindings: &mut Vec<Option<SourceType>>,
    context: &mut TraitMatchingContext,
) -> bool {
    assert_eq!(impl_trait_ty.trait_id, check_trait_ty.trait_id);
    assert_eq!(
        impl_trait_ty.type_params.len(),
        check_trait_ty.type_params.len()
    );

    if !match_arrays_with_context(
        sa,
        &check_trait_ty.type_params,
        check_element,
        check_type_param_definition,
        &impl_trait_ty.type_params,
        impl_.type_param_definition(sa),
        opt_bindings,
        context,
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
