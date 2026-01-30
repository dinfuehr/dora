use std::collections::HashMap;

use crate::access::{sym_accessible_from, trait_accessible_from};
use crate::args;
use crate::error::diagnostics::{
    BOUND_EXPECTED, DUPLICATE_TYPE_BINDING, MISSING_TYPE_BINDING, NO_TYPE_PARAMS_EXPECTED,
    NOT_ACCESSIBLE, TYPE_BINDING_ORDER, UNEXPECTED_TYPE_BINDING, UNKNOWN_TYPE_BINDING,
    WRONG_NUMBER_TYPE_PARAMS,
};
use crate::parsety::ty_for_sym;
use crate::sema::{Element, Sema, TraitDefinitionId};
use crate::sym::SymbolKind;
use crate::{SourceType, SourceTypeArray, TraitType};

use super::{TypeArgument, TypeRef, TypeRefArena, TypeRefId, TypeSymbol, type_ref_span};

pub(crate) fn convert_type_ref(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
) -> SourceType {
    convert_type_ref_inner(sa, type_refs, ctxt_element, type_ref_id)
}

fn convert_type_ref_inner(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
) -> SourceType {
    match type_refs.type_ref(type_ref_id) {
        TypeRef::This => SourceType::This,
        TypeRef::Error => SourceType::Error,
        TypeRef::Ref { .. } => SourceType::Error,
        TypeRef::Tuple { subtypes } => {
            if subtypes.is_empty() {
                return SourceType::Unit;
            }

            let mut new_subtypes = Vec::with_capacity(subtypes.len());

            for subtype in subtypes {
                new_subtypes.push(convert_type_ref_inner(
                    sa,
                    type_refs,
                    ctxt_element,
                    *subtype,
                ));
            }

            SourceType::Tuple(SourceTypeArray::with(new_subtypes))
        }
        TypeRef::Lambda { params, return_ty } => {
            let mut new_params = Vec::with_capacity(params.len());

            for param in params {
                new_params.push(convert_type_ref_inner(sa, type_refs, ctxt_element, *param));
            }

            let new_return_ty = convert_type_ref_inner(sa, type_refs, ctxt_element, *return_ty);
            SourceType::Lambda(SourceTypeArray::with(new_params), Box::new(new_return_ty))
        }
        TypeRef::Path { type_arguments, .. } => {
            let type_symbol = match type_refs.symbol(type_ref_id) {
                Some(sym) => sym,
                None => return SourceType::Error,
            };

            match type_symbol {
                TypeSymbol::Symbol(symbol) => convert_type_ref_symbol(
                    sa,
                    type_refs,
                    ctxt_element,
                    type_ref_id,
                    symbol,
                    type_arguments,
                ),
                TypeSymbol::Assoc(_) => unreachable!(),
                TypeSymbol::GenericAssoc {
                    alias_id,
                    tp_id,
                    trait_ty,
                } => SourceType::GenericAssoc {
                    ty: Box::new(SourceType::TypeParam(tp_id)),
                    trait_ty,
                    assoc_id: alias_id,
                },
            }
        }
        TypeRef::Assoc { .. } => match type_refs.symbol(type_ref_id) {
            Some(TypeSymbol::Assoc(assoc_id)) => {
                ty_for_sym(sa, SymbolKind::Alias(assoc_id), SourceTypeArray::empty())
            }
            Some(_) => unreachable!(),
            None => SourceType::Error,
        },
        TypeRef::QualifiedPath { ty, trait_ty, .. } => convert_type_ref_qualified_path(
            sa,
            type_refs,
            ctxt_element,
            type_ref_id,
            *ty,
            *trait_ty,
        ),
    }
}

fn convert_type_ref_qualified_path(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    ty: TypeRefId,
    trait_ty_ref_id: TypeRefId,
) -> SourceType {
    let assoc_id = match type_refs.symbol(type_ref_id) {
        Some(TypeSymbol::Assoc(assoc_id)) => assoc_id,
        _ => return SourceType::Error,
    };

    let inner_ty = convert_type_ref_inner(sa, type_refs, ctxt_element, ty);

    // For qualified paths, we need to handle the trait specially - don't require bindings
    let trait_ty = match type_refs.symbol(trait_ty_ref_id) {
        Some(TypeSymbol::Symbol(SymbolKind::Trait(trait_id))) => {
            // Get type arguments from the trait_ty type ref
            let type_arguments = match type_refs.type_ref(trait_ty_ref_id) {
                TypeRef::Path { type_arguments, .. } => type_arguments,
                _ => return SourceType::Error,
            };
            convert_type_ref_trait_for_qualified_path(
                sa,
                type_refs,
                ctxt_element,
                trait_ty_ref_id,
                trait_id,
                type_arguments,
            )
        }
        _ => return SourceType::Error,
    };
    let trait_ty = match trait_ty {
        Some(ty) => ty,
        None => return SourceType::Error,
    };

    SourceType::GenericAssoc {
        ty: Box::new(inner_ty),
        trait_ty,
        assoc_id,
    }
}

/// Check a trait type used in a qualified path (e.g., `<Self as Foo>::Bar`).
/// Unlike `convert_type_ref_trait_object`, this doesn't require all type bindings to be specified.
fn convert_type_ref_trait_for_qualified_path(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    trait_id: TraitDefinitionId,
    type_arguments: &[TypeArgument],
) -> Option<TraitType> {
    let trait_ = sa.trait_(trait_id);
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    if !trait_accessible_from(sa, trait_id, ctxt_element.module_id()) {
        sa.report(file_id, span, &NOT_ACCESSIBLE, args!());
    }

    let mut idx = 0;
    let mut trait_type_params = Vec::new();
    let mut bindings: Vec<(crate::sema::AliasDefinitionId, SourceType)> = Vec::new();

    // Process positional type arguments
    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_some() {
            break;
        }

        let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
        trait_type_params.push(ty);
        idx += 1;
    }

    // Process named type bindings (optional for qualified paths)
    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_none() {
            sa.report(file_id, span, &TYPE_BINDING_ORDER, args!());
            return None;
        }

        let name = arg.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            // Check for duplicates
            if bindings.iter().any(|(id, _)| *id == alias_id) {
                sa.report(file_id, span, &DUPLICATE_TYPE_BINDING, args!());
                return None;
            }

            let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
            bindings.push((alias_id, ty));
        } else {
            sa.report(file_id, span, &UNKNOWN_TYPE_BINDING, args!());
            return None;
        }

        idx += 1;
    }

    let type_params = SourceTypeArray::with(trait_type_params);
    Some(TraitType {
        trait_id,
        type_params,
        bindings,
    })
}

fn convert_type_ref_symbol(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    symbol: SymbolKind,
    type_arguments: &[TypeArgument],
) -> SourceType {
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    match symbol {
        SymbolKind::TypeParam(id) => {
            if !type_arguments.is_empty() {
                sa.report(file_id, span, &NO_TYPE_PARAMS_EXPECTED, args!());
                return SourceType::Error;
            }

            SourceType::TypeParam(id)
        }
        SymbolKind::Trait(trait_id) => convert_type_ref_trait_object(
            sa,
            type_refs,
            ctxt_element,
            type_ref_id,
            trait_id,
            type_arguments,
        ),
        SymbolKind::Class(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Alias(..) => {
            if !sym_accessible_from(sa, symbol.clone(), ctxt_element.module_id()) {
                sa.report(file_id, span, &NOT_ACCESSIBLE, args!());
            }

            let mut new_type_params = Vec::with_capacity(type_arguments.len());

            for (idx, arg) in type_arguments.iter().enumerate() {
                if arg.name.is_some() {
                    let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
                    sa.report(file_id, arg_span, &UNEXPECTED_TYPE_BINDING, args!());
                    return SourceType::Error;
                }

                let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
                new_type_params.push(ty);
            }

            let new_type_params = SourceTypeArray::with(new_type_params);
            let callee_element = get_symbol_element(sa, symbol);
            let callee_type_param_definition = callee_element.type_param_definition();

            if callee_type_param_definition.type_param_count() != new_type_params.len() {
                sa.report(
                    file_id,
                    span,
                    &WRONG_NUMBER_TYPE_PARAMS,
                    args!(
                        callee_type_param_definition.type_param_count(),
                        new_type_params.len()
                    ),
                );
                return SourceType::Error;
            }

            ty_for_sym(sa, symbol, new_type_params)
        }
        _ => unreachable!(),
    }
}

fn convert_type_ref_trait_object(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    trait_id: TraitDefinitionId,
    type_arguments: &[TypeArgument],
) -> SourceType {
    let trait_ = sa.trait_(trait_id);
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    if !trait_accessible_from(sa, trait_id, ctxt_element.module_id()) {
        sa.report(file_id, span, &NOT_ACCESSIBLE, args!());
    }

    let mut idx = 0;
    let mut trait_type_params = Vec::new();

    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_some() {
            break;
        }

        let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
        trait_type_params.push(ty);
        idx += 1;
    }

    let mut used_aliases = HashMap::new();

    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_none() {
            let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
            sa.report(file_id, arg_span, &TYPE_BINDING_ORDER, args!());
            return SourceType::Error;
        }

        let name = arg.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            if used_aliases.contains_key(&alias_id) {
                let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
                sa.report(file_id, arg_span, &DUPLICATE_TYPE_BINDING, args!());
                return SourceType::Error;
            }

            let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
            used_aliases.insert(alias_id, ty);
        } else {
            let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
            sa.report(file_id, arg_span, &UNKNOWN_TYPE_BINDING, args!());
            return SourceType::Error;
        }

        idx += 1;
    }

    let mut bindings = Vec::new();

    for alias_id in trait_.aliases() {
        if let Some(ty) = used_aliases.remove(&alias_id) {
            bindings.push(ty);
        } else {
            let name = sa.alias(*alias_id).name;
            let name = sa.interner.str(name).to_string();
            sa.report(file_id, span, &MISSING_TYPE_BINDING, args!(name));
            return SourceType::Error;
        }
    }

    SourceType::TraitObject(trait_id, trait_type_params.into(), bindings.into())
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

/// Convert a type reference to a TraitType (for trait bounds and impl trait types).
/// Unlike `convert_type_ref_trait_object`, this returns `Option<TraitType>` instead of `SourceType`.
pub(crate) fn convert_trait_type_ref(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    allow_bindings: bool,
) -> Option<TraitType> {
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    let type_symbol = match type_refs.symbol(type_ref_id) {
        Some(sym) => sym,
        None => return None,
    };

    let trait_id = match type_symbol {
        TypeSymbol::Symbol(SymbolKind::Trait(trait_id)) => trait_id,
        TypeSymbol::Symbol(_) => {
            // Not a trait - report error
            sa.report(file_id, span, &BOUND_EXPECTED, args!());
            return None;
        }
        _ => return None,
    };

    let type_arguments = match type_refs.type_ref(type_ref_id) {
        TypeRef::Path { type_arguments, .. } => type_arguments,
        _ => return None,
    };

    convert_trait_type_ref_inner(
        sa,
        type_refs,
        ctxt_element,
        type_ref_id,
        trait_id,
        type_arguments,
        allow_bindings,
    )
}

fn convert_trait_type_ref_inner(
    sa: &Sema,
    type_refs: &TypeRefArena,
    ctxt_element: &dyn Element,
    type_ref_id: TypeRefId,
    trait_id: TraitDefinitionId,
    type_arguments: &[TypeArgument],
    allow_bindings: bool,
) -> Option<TraitType> {
    let trait_ = sa.trait_(trait_id);
    let file_id = ctxt_element.file_id();
    let span = type_ref_span(sa, type_refs, file_id, type_ref_id);

    if !trait_accessible_from(sa, trait_id, ctxt_element.module_id()) {
        sa.report(file_id, span, &NOT_ACCESSIBLE, args!());
    }

    let mut idx = 0;
    let mut trait_type_params = Vec::new();
    let mut bindings: Vec<(crate::sema::AliasDefinitionId, SourceType)> = Vec::new();

    // Process positional type arguments
    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_some() {
            break;
        }

        let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
        trait_type_params.push(ty);
        idx += 1;
    }

    // Process named type bindings
    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_none() {
            let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
            sa.report(file_id, arg_span, &TYPE_BINDING_ORDER, args!());
            return None;
        } else if !allow_bindings {
            let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
            sa.report(file_id, arg_span, &UNEXPECTED_TYPE_BINDING, args!());
            return None;
        }

        let name = arg.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            // Check for duplicates
            if bindings.iter().any(|(id, _)| *id == alias_id) {
                let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
                sa.report(file_id, arg_span, &DUPLICATE_TYPE_BINDING, args!());
                return None;
            }

            let ty = convert_type_ref_inner(sa, type_refs, ctxt_element, arg.ty);
            bindings.push((alias_id, ty));
        } else {
            let arg_span = get_type_argument_span(sa, type_refs, file_id, type_ref_id, idx);
            sa.report(file_id, arg_span, &UNKNOWN_TYPE_BINDING, args!());
            return None;
        }

        idx += 1;
    }

    let type_params = SourceTypeArray::with(trait_type_params);
    Some(TraitType {
        trait_id,
        type_params,
        bindings,
    })
}

/// Get the span of a type argument at a specific index from the AST.
fn get_type_argument_span(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: crate::sema::SourceFileId,
    type_ref_id: TypeRefId,
    idx: usize,
) -> dora_parser::Span {
    use dora_parser::ast::{AstType, SyntaxNodeBase};

    let syntax_node_ptr = type_refs
        .syntax_node_ptr(type_ref_id)
        .expect("missing syntax node ptr");
    let ast_ty = sa.syntax::<AstType>(file_id, syntax_node_ptr);

    if let AstType::PathType(path_type) = ast_ty {
        if let Some(arg) = path_type.params().nth(idx) {
            return arg.span();
        }
    }

    panic!("missing type argument span");
}
