use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::access::{const_accessible_from, enum_accessible_from, global_accessible_from};
use crate::args;
use crate::error::diagnostics::{
    CAPTURE_REF_IN_LAMBDA, ENUM_VARIANT_MISSING_ARGUMENTS, EXPECTED_MODULE,
    EXPECTED_SOME_IDENTIFIER, MULTIPLE_CANDIDATES_FOR_ASSOC_TYPE, NO_SUPER_MODULE,
    NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE, PACKAGE_AS_VALUE, SELF_VALUE_TYPE_IN_LAMBDA,
    SUPER_AS_VALUE, THIS_UNAVAILABLE, UNKNOWN_ASSOC, UNKNOWN_ENUM_VARIANT, UNKNOWN_IDENTIFIER,
    UNKNOWN_IDENTIFIER_IN_MODULE, VALUE_EXPECTED,
};
use crate::interner::Name;
use crate::sema::NestedVarId;
use crate::sema::{
    AliasDefinitionId, ExprId, IdentType, PathExpr, PathSegment, PathSegmentKind, TypeParamId,
};
use crate::specialize_type;
use crate::typeck::{TypeCheck, check_type_params};
use crate::{SourceType, SourceTypeArray, SymbolKind, TraitType, ty::error as ty_error};

/// Result of resolving a path. This is separate from SymbolKind because
/// paths can resolve to things that aren't symbols (like `Self` in traits).
#[derive(Debug, Clone)]
pub(crate) enum PathResolution {
    /// A regular symbol from the symbol table
    Symbol(SymbolKind),
    /// `Self` in a trait context (not a symbol, but a contextual reference)
    Self_,
    /// `Self::T` where T is an associated type in a trait context
    SelfAssocType(AliasDefinitionId),
    /// `T::Item` where T is a type param and Item is an associated type
    GenericAssoc {
        tp_id: TypeParamId,
        trait_ty: TraitType,
        assoc_id: AliasDefinitionId,
    },
}

/// Get the span of a path segment by index from the AST.
fn path_segment_span(ck: &TypeCheck, expr_id: ExprId, index: usize) -> Span {
    let e = ck.syntax::<ast::AstPathExpr>(expr_id);
    e.segments()
        .nth(index)
        .expect("path segment index out of bounds")
        .span()
}

pub(super) fn check_expr_path(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    path_expr: &PathExpr,
    expected_ty: SourceType,
) -> SourceType {
    let resolution = match resolve_path(ck, expr_id, path_expr, false) {
        Ok(res) => res,
        Err(()) => return ty_error(),
    };

    match resolution {
        PathResolution::Symbol(sym) => {
            resolve_symbol(ck, expr_id, sym, &path_expr.segments, expected_ty)
        }
        PathResolution::Self_
        | PathResolution::SelfAssocType(_)
        | PathResolution::GenericAssoc { .. } => {
            // `Self`, `Self::T`, or `T::Item` alone is not a valid value expression
            ck.report(ck.expr_span(expr_id), &VALUE_EXPECTED, args![]);
            ty_error()
        }
    }
}

/// Resolves a path to a symbol by traversing modules and enums.
/// If `skip_last` is true, the last segment is not resolved (used for call paths
/// where the last segment is the method name).
pub(crate) fn resolve_path(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    path_expr: &PathExpr,
    skip_last: bool,
) -> Result<PathResolution, ()> {
    let segments = &path_expr.segments;
    let segment_count = segments.len() - if skip_last { 1 } else { 0 };

    let first_segment = &segments[0];

    let mut current_sym = match &first_segment.kind {
        PathSegmentKind::Name(name) => {
            let Some(sym) = ck.symtable.get(*name) else {
                ck.report(
                    ck.expr_span(expr_id),
                    &UNKNOWN_IDENTIFIER,
                    args![ck.sa.name(*name)],
                );
                return Err(());
            };
            sym
        }
        PathSegmentKind::This => {
            if segments.len() == 1 {
                // `self` alone refers to the receiver variable
                if !ck.is_self_available {
                    ck.report(
                        path_segment_span(ck, expr_id, 0),
                        &THIS_UNAVAILABLE,
                        args![],
                    );
                    return Err(());
                }
                let self_var = NestedVarId(0);
                let self_var = SymbolKind::Var(self_var);
                return Ok(PathResolution::Symbol(self_var));
            }
            // `self::...` refers to the current module
            SymbolKind::Module(ck.module_id)
        }
        PathSegmentKind::UpcaseSelf => {
            return resolve_self_path(ck, expr_id, segments, segment_count);
        }
        PathSegmentKind::Package => {
            if segments.len() == 1 {
                ck.report(
                    path_segment_span(ck, expr_id, 0),
                    &PACKAGE_AS_VALUE,
                    args![],
                );
                return Err(());
            }
            let current_module = ck.sa.module(ck.module_id);
            let package = &ck.sa.packages[current_module.package_id()];
            SymbolKind::Module(package.top_level_module_id())
        }
        PathSegmentKind::Super => {
            if segments.len() == 1 {
                ck.report(path_segment_span(ck, expr_id, 0), &SUPER_AS_VALUE, args![]);
                return Err(());
            }
            let current_module = ck.sa.module(ck.module_id);
            let Some(parent_module_id) = current_module.parent_module_id else {
                ck.report(path_segment_span(ck, expr_id, 0), &NO_SUPER_MODULE, args![]);
                return Err(());
            };
            SymbolKind::Module(parent_module_id)
        }
        PathSegmentKind::Error => return Err(()),
    };

    for (idx, segment) in segments.iter().enumerate().skip(1).take(segment_count - 1) {
        let Some(name) = segment.kind.name() else {
            ck.report(
                path_segment_span(ck, expr_id, idx),
                &EXPECTED_MODULE,
                args![],
            );
            return Err(());
        };

        match current_sym {
            SymbolKind::Module(module_id) => {
                if !segments[idx - 1].type_params.is_empty() {
                    ck.report(
                        path_segment_span(ck, expr_id, idx - 1),
                        &NO_TYPE_PARAMS_EXPECTED,
                        args![],
                    );
                }

                let table = ck.sa.module_table(module_id);
                let Some(sym) = table.get(name) else {
                    let module = ck.sa.module(module_id).name(ck.sa);
                    let element_name = ck.sa.interner.str(name).to_string();
                    ck.report(
                        ck.expr_span(expr_id),
                        &UNKNOWN_IDENTIFIER_IN_MODULE,
                        args![module, element_name],
                    );
                    return Err(());
                };
                current_sym = sym;
            }
            SymbolKind::Enum(enum_id) => {
                let enum_ = ck.sa.enum_(enum_id);
                let Some(&variant_idx) = enum_.name_to_value().get(&name) else {
                    let variant_name = ck.sa.interner.str(name).to_string();
                    ck.report(
                        ck.expr_span(expr_id),
                        &UNKNOWN_ENUM_VARIANT,
                        args![variant_name],
                    );
                    return Err(());
                };
                current_sym = SymbolKind::EnumVariant(enum_id, variant_idx);
            }
            SymbolKind::TypeParam(tp_id) => {
                // T::Item where T is a type param and Item is an associated type
                let available = lookup_alias_on_type_param(ck, tp_id, name);

                if available.len() == 1 {
                    let (trait_ty, assoc_id) =
                        available.into_iter().next().expect("element expected");
                    return Ok(PathResolution::GenericAssoc {
                        tp_id,
                        trait_ty,
                        assoc_id,
                    });
                } else if available.len() > 1 {
                    // Multiple candidates - ambiguous
                    ck.report(
                        path_segment_span(ck, expr_id, idx),
                        &MULTIPLE_CANDIDATES_FOR_ASSOC_TYPE,
                        args![],
                    );
                    return Err(());
                } else {
                    // No matching associated type
                    ck.report(path_segment_span(ck, expr_id, idx), &UNKNOWN_ASSOC, args![]);
                    return Err(());
                }
            }
            _ => {
                ck.report(ck.expr_span(expr_id), &EXPECTED_MODULE, args![]);
                return Err(());
            }
        }
    }

    Ok(PathResolution::Symbol(current_sym))
}

fn resolve_self_path(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    segments: &[PathSegment],
    segment_count: usize,
) -> Result<PathResolution, ()> {
    // segment_count is the number of segments to process (excludes method name if skip_last)
    // segment_count == 1: just `Self` (as value) or `Self::method()` (as call)
    // segment_count == 2: `Self::T` (as value) or `Self::T::method()` (as call)
    // segment_count > 2: invalid chained access like `Self::T::Foo::Baz()`

    if segment_count == 1 {
        // Just `Self` - return Self_ and let caller handle it
        return Ok(PathResolution::Self_);
    }

    // For segment_count >= 2, we need a name in the second segment
    let Some(second_name) = segments[1].kind.name() else {
        ck.report(
            path_segment_span(ck, expr_id, 1),
            &EXPECTED_SOME_IDENTIFIER,
            args![],
        );
        return Err(());
    };

    let mut alias_id = None;

    // Check if this is `Self::T` where T is an associated type
    if let Some(trait_id) = ck.parent.trait_id() {
        let trait_ = ck.sa.trait_(trait_id);
        alias_id = trait_.alias_names().get(&second_name).copied();
    }

    // `Self::something` where something is not an associated type
    if alias_id.is_none() {
        ck.report(path_segment_span(ck, expr_id, 1), &UNKNOWN_ASSOC, args![]);
        return Err(());
    }

    // Check for chained access like `Self::T::Foo::...`
    if segment_count > 2 {
        ck.report(path_segment_span(ck, expr_id, 2), &EXPECTED_MODULE, args![]);
        return Err(());
    }

    Ok(PathResolution::SelfAssocType(alias_id.unwrap()))
}

fn resolve_symbol(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sym: SymbolKind,
    path: &[PathSegment],
    expected_ty: SourceType,
) -> SourceType {
    let type_params = extract_type_params(ck, expr_id, &sym, path);

    match sym {
        SymbolKind::Var(var_id) => {
            if !type_params.is_empty() {
                ck.report(ck.expr_span(expr_id), &NO_TYPE_PARAMS_EXPECTED, args![]);
            }

            let var_ty = ck.vars.get_var(var_id).ty.clone();
            let ty = match &var_ty {
                SourceType::Ref(inner) => inner.as_ref().clone(),
                _ => var_ty.clone(),
            };
            ck.body.set_ty(expr_id, ty.clone());

            // Check for invalid captures in lambdas.
            if ck.is_lambda && ck.vars.is_context_var(var_id) && var_ty.is_ref() {
                // self is always var_id 0 when is_self_available is true.
                let is_self = var_id == NestedVarId(0) && ck.is_self_available;

                if is_self {
                    ck.report(ck.expr_span(expr_id), &SELF_VALUE_TYPE_IN_LAMBDA, args![]);
                } else {
                    ck.report(ck.expr_span(expr_id), &CAPTURE_REF_IN_LAMBDA, args![]);
                }
            }

            let ident = ck.maybe_allocate_in_context(var_id);
            ck.body.insert_ident(expr_id, ident);

            ty
        }

        SymbolKind::Global(global_id) => {
            if !type_params.is_empty() {
                ck.report(ck.expr_span(expr_id), &NO_TYPE_PARAMS_EXPECTED, args![]);
            }
            if !global_accessible_from(ck.sa, global_id, ck.module_id) {
                ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args![]);
            }

            let global_var = ck.sa.global(global_id);
            let ty = global_var.ty();
            ck.body.set_ty(expr_id, ty.clone());
            ck.body.insert_ident(expr_id, IdentType::Global(global_id));
            ty
        }

        SymbolKind::Const(const_id) => {
            if !type_params.is_empty() {
                ck.report(ck.expr_span(expr_id), &NO_TYPE_PARAMS_EXPECTED, args![]);
            }
            if !const_accessible_from(ck.sa, const_id, ck.module_id) {
                ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args![]);
            }

            let const_ = ck.sa.const_(const_id);
            ck.body.set_ty(expr_id, const_.ty());
            ck.body.insert_ident(expr_id, IdentType::Const(const_id));
            const_.ty()
        }

        SymbolKind::EnumVariant(enum_id, variant_idx) => check_enum_variant_without_args(
            ck,
            expr_id,
            expected_ty,
            enum_id,
            type_params,
            variant_idx,
        ),

        _ => {
            ck.report(ck.expr_span(expr_id), &VALUE_EXPECTED, args![]);
            ty_error()
        }
    }
}

fn extract_type_params(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sym: &SymbolKind,
    path: &[PathSegment],
) -> SourceTypeArray {
    // For enum variants with multi-segment paths, type params can be on either segment
    if matches!(sym, SymbolKind::EnumVariant(_, _)) && path.len() >= 2 {
        let enum_segment = &path[path.len() - 2];
        let variant_segment = &path[path.len() - 1];

        let enum_params: Vec<SourceType> = enum_segment
            .type_params
            .iter()
            .map(|&ty| ck.read_type(ty))
            .collect();
        let variant_params: Vec<SourceType> = variant_segment
            .type_params
            .iter()
            .map(|&ty| ck.read_type(ty))
            .collect();

        if !enum_params.is_empty() && !variant_params.is_empty() {
            let e = ck.syntax::<ast::AstPathExpr>(expr_id);
            let segments: Vec<_> = e.segments().collect();
            let error_span = segments
                .last()
                .expect("missing segment")
                .type_params_span()
                .expect("missing type params");
            ck.report(
                error_span,
                &crate::error::diagnostics::NO_TYPE_PARAMS_EXPECTED,
                args![],
            );
            SourceTypeArray::with(enum_params)
        } else if !enum_params.is_empty() {
            SourceTypeArray::with(enum_params)
        } else {
            SourceTypeArray::with(variant_params)
        }
    } else {
        // For other symbols, type params come from the last segment
        let last_segment = &path[path.len() - 1];
        let params: Vec<SourceType> = last_segment
            .type_params
            .iter()
            .map(|&ty| ck.read_type(ty))
            .collect();
        SourceTypeArray::with(params)
    }
}

pub(super) fn check_enum_variant_without_args(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
    enum_id: crate::sema::EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args![]);
    }

    let type_params = if expected_ty.enum_id() == Some(enum_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        enum_,
        &type_params,
        ck.file_id,
        || ck.expr_span(expr_id),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let variant_id = enum_.variant_id_at(variant_idx as usize);
    let variant = ck.sa.variant(variant_id);

    if !variant.field_ids().is_empty() {
        ck.report(
            ck.expr_span(expr_id),
            &ENUM_VARIANT_MISSING_ARGUMENTS,
            args![],
        );
    }

    ck.body.insert_ident(
        expr_id,
        IdentType::EnumVariant(enum_id, type_params.clone(), variant_idx),
    );

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.body.set_ty(expr_id, ty.clone());
        ty
    } else {
        ck.body.set_ty(expr_id, ty_error());
        ty_error()
    }
}

fn lookup_alias_on_type_param(
    ck: &TypeCheck,
    tp_id: TypeParamId,
    name: Name,
) -> Vec<(TraitType, AliasDefinitionId)> {
    let mut results = Vec::with_capacity(2);

    for bound in ck.type_param_definition.bounds_for_type_param(tp_id) {
        let trait_id = bound.trait_id;
        let trait_ = ck.sa.trait_(trait_id);

        if let Some(id) = trait_.alias_names().get(&name) {
            results.push((bound, *id));
        }
    }

    results
}
