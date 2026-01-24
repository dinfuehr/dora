use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::access::{const_accessible_from, enum_accessible_from, global_accessible_from};
use crate::args;
use crate::error::diagnostics::{
    ENUM_VARIANT_MISSING_ARGUMENTS, EXPECTED_MODULE, NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE,
    THIS_UNAVAILABLE, UNKNOWN_ENUM_VARIANT, UNKNOWN_IDENTIFIER, UNKNOWN_IDENTIFIER_IN_MODULE,
    VALUE_EXPECTED,
};
use crate::sema::NestedVarId;
use crate::sema::{ExprId, IdentType, PathExpr, PathSegment, PathSegmentKind};
use crate::specialize_type;
use crate::typeck::{TypeCheck, check_type_params};
use crate::{SourceType, SourceTypeArray, SymbolKind, ty::error as ty_error};

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
    sema_expr: &PathExpr,
    expected_ty: SourceType,
) -> SourceType {
    let path = &sema_expr.path;

    let sym = match resolve_path(ck, expr_id, path) {
        Ok(sym) => sym,
        Err(()) => return ty_error(),
    };

    resolve_symbol(ck, expr_id, sym, path, expected_ty)
}

/// Resolves a path to a symbol by traversing modules and enums.
pub(crate) fn resolve_path(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    path: &[PathSegment],
) -> Result<SymbolKind, ()> {
    let first_segment = &path[0];
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
            if path.len() != 1 {
                ck.report(path_segment_span(ck, expr_id, 0), &EXPECTED_MODULE, args![]);
                return Err(());
            }
            if !ck.is_self_available {
                ck.report(
                    path_segment_span(ck, expr_id, 0),
                    &THIS_UNAVAILABLE,
                    args![],
                );
                return Err(());
            }
            return Ok(SymbolKind::Var(NestedVarId(0)));
        }
        PathSegmentKind::UpcaseSelf => {
            unimplemented!()
        }
        PathSegmentKind::Package => unimplemented!(),
        PathSegmentKind::Super => {
            unimplemented!()
        }
        PathSegmentKind::Error => return Err(()),
    };

    for (idx, segment) in path.iter().enumerate().skip(1) {
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
                if !path[idx - 1].type_params.is_empty() {
                    ck.report(ck.expr_span(expr_id), &NO_TYPE_PARAMS_EXPECTED, args![]);
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
            _ => {
                ck.report(ck.expr_span(expr_id), &EXPECTED_MODULE, args![]);
                return Err(());
            }
        }
    }

    Ok(current_sym)
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

            let ty = ck.vars.get_var(var_id).ty.clone();
            ck.body.set_ty(expr_id, ty.clone());

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

#[cfg(test)]
mod tests {
    use crate::error::diagnostics::NO_TYPE_PARAMS_EXPECTED;
    use crate::tests::*;

    #[test]
    fn var_with_type_params() {
        err(
            "fn f() { let x = 1; x[Int32]; }",
            (1, 21),
            8,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn const_with_type_params() {
        err(
            "const X: Int32 = 1; fn f() { X[Int32]; }",
            (1, 30),
            8,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn global_with_type_params() {
        err(
            "let X: Int32 = 1; fn f() { X[Int32]; }",
            (1, 28),
            8,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn nested_module_path() {
        ok("
            mod a { pub mod b { pub mod c { pub let d: Int32 = 1; } } }
            fn f(): Int32 { a::b::c::d }
        ");
    }

    #[test]
    fn nested_module_path_with_type_params_on_first() {
        err(
            "
            mod a { pub mod b { pub mod c { pub let d: Int32 = 1; } } }
            fn f(): Int32 { a[Int32]::b::c::d }
        ",
            (3, 29),
            17,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn nested_module_path_with_type_params_on_middle() {
        err(
            "
            mod a { pub mod b { pub mod c { pub let d: Int32 = 1; } } }
            fn f(): Int32 { a::b[Int32]::c::d }
        ",
            (3, 29),
            17,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn nested_module_path_with_type_params_on_last_module() {
        err(
            "
            mod a { pub mod b { pub mod c { pub let d: Int32 = 1; } } }
            fn f(): Int32 { a::b::c[Int32]::d }
        ",
            (3, 29),
            17,
            crate::ErrorLevel::Error,
            &NO_TYPE_PARAMS_EXPECTED,
            crate::args![],
        );
    }

    #[test]
    fn single_module_with_type_params() {
        // This should report an error, a module cannot be used as a value.
        // Currently this doesn't report an error for the invalid type parameter
        // usage.
        err(
            "mod a {} fn f() { a[Int32]; }",
            (1, 19),
            8,
            crate::ErrorLevel::Error,
            &crate::error::diagnostics::VALUE_EXPECTED,
            crate::args![],
        );
    }
}
