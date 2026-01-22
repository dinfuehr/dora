use crate::args;
use crate::error::diagnostics::{SELF_TYPE_UNAVAILABLE, TRAIT_NOT_OBJECT_SAFE};
use crate::parsety::{check_trait_type_param_definition, check_type_params};
use crate::sema::{Element, Sema, TraitDefinitionId, is_trait_object_safe};
use crate::sym::SymbolKind;
use crate::{SourceType, SourceTypeArray};

use super::{TypeArgument, TypeRef, TypeRefArena, TypeRefId, TypeSymbol, type_ref_span};

pub(crate) fn check_type_ref(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    ty: SourceType,
    allow_self: bool,
) -> SourceType {
    check_type_ref_inner(sa, type_refs, ctxt_element, type_ref_id, ty, allow_self)
}

fn check_type_ref_inner(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    ty: SourceType,
    allow_self: bool,
) -> SourceType {
    // Handle error types early - nothing to check
    if ty.is_error() {
        return ty;
    }

    match (type_refs.type_ref(type_ref_id), &ty) {
        (TypeRef::This, _) | (TypeRef::Assoc { .. }, _) => {
            if !allow_self {
                sa.report(
                    ctxt_element.file_id(),
                    type_ref_span(sa, type_refs, ctxt_element.file_id(), type_ref_id),
                    &SELF_TYPE_UNAVAILABLE,
                    args!(),
                );
                return SourceType::Error;
            }
            ty
        }

        (TypeRef::Error, _) | (TypeRef::Ref { .. }, _) => ty,

        // Empty tuple becomes Unit
        (TypeRef::Tuple { subtypes }, SourceType::Unit) if subtypes.is_empty() => ty,

        (TypeRef::Tuple { subtypes }, SourceType::Tuple(source_subtypes)) => {
            let mut has_error = false;
            for (subtype_ref, subtype_ty) in subtypes.iter().zip(source_subtypes.iter()) {
                let result = check_type_ref_inner(
                    sa,
                    type_refs,
                    ctxt_element,
                    *subtype_ref,
                    subtype_ty.clone(),
                    allow_self,
                );
                if result.is_error() {
                    has_error = true;
                }
            }
            if has_error { SourceType::Error } else { ty }
        }

        (TypeRef::Lambda { params, return_ty }, SourceType::Lambda(source_params, source_ret)) => {
            let mut has_error = false;
            for (param_ref, param_ty) in params.iter().zip(source_params.iter()) {
                let result = check_type_ref_inner(
                    sa,
                    type_refs,
                    ctxt_element,
                    *param_ref,
                    param_ty.clone(),
                    allow_self,
                );
                if result.is_error() {
                    has_error = true;
                }
            }
            let result = check_type_ref_inner(
                sa,
                type_refs,
                ctxt_element,
                *return_ty,
                source_ret.as_ref().clone(),
                allow_self,
            );
            if result.is_error() {
                has_error = true;
            }
            if has_error { SourceType::Error } else { ty }
        }

        (TypeRef::Path { type_arguments, .. }, _) => {
            let type_symbol = match type_refs.symbol(type_ref_id) {
                Some(sym) => sym,
                None => return ty,
            };

            match type_symbol {
                TypeSymbol::Symbol(symbol) => check_symbol(
                    sa,
                    type_refs,
                    ctxt_element,
                    type_ref_id,
                    symbol,
                    type_arguments,
                    ty,
                    allow_self,
                ),
                TypeSymbol::Assoc(_) | TypeSymbol::GenericAssoc { .. } => ty,
            }
        }

        (TypeRef::QualifiedPath { .. }, _) => {
            // For qualified paths, there's nothing to check for type params
            ty
        }

        _ => unreachable!(),
    }
}

fn check_symbol(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    symbol: SymbolKind,
    type_arguments: &[TypeArgument],
    ty: SourceType,
    allow_self: bool,
) -> SourceType {
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    match symbol {
        SymbolKind::TypeParam(_) => ty,

        SymbolKind::Trait(trait_id) => {
            let (type_params, bindings) = match &ty {
                SourceType::TraitObject(_, tp, b) => (tp, b),
                _ => return ty,
            };

            // First recursively check type arguments
            let mut has_error = false;
            for (arg, arg_ty) in type_arguments.iter().zip(type_params.iter()) {
                let result = check_type_ref_inner(
                    sa,
                    type_refs,
                    ctxt_element,
                    arg.ty,
                    arg_ty.clone(),
                    allow_self,
                );
                if result.is_error() {
                    has_error = true;
                }
            }

            check_trait_object(
                sa,
                ctxt_element,
                type_ref_id,
                type_refs,
                trait_id,
                type_params,
                bindings,
            );
            if has_error { SourceType::Error } else { ty }
        }

        SymbolKind::Class(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Alias(..) => {
            let type_params = match &ty {
                SourceType::Class(_, tp) => tp,
                SourceType::Struct(_, tp) => tp,
                SourceType::Enum(_, tp) => tp,
                SourceType::Alias(_, tp) => tp,
                _ => return ty,
            };

            // First recursively check type arguments
            let mut has_error = false;
            for (arg, arg_ty) in type_arguments.iter().zip(type_params.iter()) {
                if arg.name.is_some() {
                    return if has_error { SourceType::Error } else { ty };
                }
                let result = check_type_ref_inner(
                    sa,
                    type_refs,
                    ctxt_element,
                    arg.ty,
                    arg_ty.clone(),
                    allow_self,
                );
                if result.is_error() {
                    has_error = true;
                }
            }

            let callee_element = get_symbol_element(sa, symbol);
            let callee_type_param_definition = callee_element.type_param_definition();

            if callee_type_param_definition.type_param_count() != type_params.len() {
                return SourceType::Error;
            }

            check_type_params(
                sa,
                callee_element,
                callee_type_param_definition,
                type_params.types(),
                ctxt_element,
                span,
            );
            if has_error { SourceType::Error } else { ty }
        }

        _ => unreachable!(),
    }
}

fn check_trait_object(
    sa: &Sema,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    type_refs: &TypeRefArena,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    bindings: &SourceTypeArray,
) {
    let trait_ = sa.trait_(trait_id);
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    if !is_trait_object_safe(sa, trait_id) {
        sa.report(file_id, span, &TRAIT_NOT_OBJECT_SAFE, args!());
        return;
    }

    let mut binding_pairs = Vec::with_capacity(bindings.len());
    for (idx, ty) in bindings.iter().enumerate() {
        if idx < trait_.aliases().len() {
            binding_pairs.push((trait_.aliases()[idx], ty.clone()));
        }
    }

    check_trait_type_param_definition(
        sa,
        ctxt_element,
        trait_,
        type_params.types(),
        &binding_pairs,
        file_id,
        span,
        ctxt_element.type_param_definition(),
    );
}

fn get_symbol_element(sa: &Sema, sym: SymbolKind) -> &dyn Element {
    match sym {
        SymbolKind::Class(id) => sa.class(id),
        SymbolKind::Struct(id) => sa.struct_(id),
        SymbolKind::Enum(id) => sa.enum_(id),
        SymbolKind::Alias(id) => sa.alias(id),
        _ => unimplemented!(),
    }
}

/// Check a trait type reference (used for trait bounds and impl trait types).
/// Recursively checks type arguments and verifies they satisfy their bounds.
pub(crate) fn check_trait_type_ref(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    trait_ty: crate::TraitType,
) -> Option<crate::TraitType> {
    let type_arguments = match type_refs.type_ref(type_ref_id) {
        TypeRef::Path { type_arguments, .. } => type_arguments,
        _ => return Some(trait_ty),
    };

    let trait_ = sa.trait_(trait_ty.trait_id);
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    // Recursively check positional type arguments
    let mut new_type_params = Vec::with_capacity(trait_ty.type_params.len());
    for (idx, ty) in trait_ty.type_params.iter().enumerate() {
        if idx < type_arguments.len() {
            let arg = &type_arguments[idx];
            let checked_ty =
                check_type_ref_inner(sa, type_refs, ctxt_element, arg.ty, ty.clone(), true);
            new_type_params.push(checked_ty);
        } else {
            new_type_params.push(ty.clone());
        }
    }

    // Recursively check binding type arguments
    let mut new_bindings = Vec::with_capacity(trait_ty.bindings.len());
    for (idx, (alias_id, ty)) in trait_ty.bindings.iter().enumerate() {
        let arg_idx = trait_ty.type_params.len() + idx;
        if arg_idx < type_arguments.len() {
            let arg = &type_arguments[arg_idx];
            let checked_ty =
                check_type_ref_inner(sa, type_refs, ctxt_element, arg.ty, ty.clone(), true);
            new_bindings.push((*alias_id, checked_ty));
        } else {
            new_bindings.push((*alias_id, ty.clone()));
        }
    }

    // Check that type params meet their bounds
    if check_trait_type_param_definition(
        sa,
        ctxt_element,
        trait_,
        &new_type_params,
        &new_bindings,
        file_id,
        span,
        ctxt_element.type_param_definition(),
    ) {
        Some(crate::TraitType {
            trait_id: trait_ty.trait_id,
            type_params: new_type_params.into(),
            bindings: new_bindings,
        })
    } else {
        None
    }
}
