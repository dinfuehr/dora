use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::access::{const_accessible_from, enum_accessible_from, global_accessible_from};
use crate::args;
use crate::error::diagnostics::{
    ENUM_VARIANT_MISSING_ARGUMENTS, EXPECTED_MODULE, INVALID_LEFT_SIDE_OF_SEPARATOR,
    NOT_ACCESSIBLE, UNKNOWN_ENUM_VARIANT, UNKNOWN_IDENTIFIER, UNKNOWN_IDENTIFIER_IN_MODULE,
    VALUE_EXPECTED,
};
use crate::interner::Name;
use crate::sema::{EnumDefinitionId, ExprId, IdentType, ModuleDefinitionId, PathExpr};
use crate::specialize_type;
use crate::typeck::{TypeCheck, check_type_params};
use crate::{SourceType, SourceTypeArray, SymbolKind, ty::error as ty_error};

pub(super) fn check_expr_path(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &PathExpr,
    expected_ty: SourceType,
) -> SourceType {
    let path = &sema_expr.path;

    // Single segment: simple identifier lookup
    if path.len() == 1 {
        let interned_name = path[0].name;
        let sym = ck.symtable.get(interned_name);

        return match sym {
            Some(SymbolKind::Var(var_id)) => {
                let ty = ck.vars.get_var(var_id).ty.clone();
                ck.body.set_ty(expr_id, ty.clone());

                // Variable may have to be context-allocated.
                let ident = ck.maybe_allocate_in_context(var_id);
                ck.body.insert_ident(expr_id, ident);

                ty
            }

            Some(SymbolKind::Global(globalid)) => {
                let global_var = ck.sa.global(globalid);
                let ty = global_var.ty();
                ck.body.set_ty(expr_id, ty.clone());

                ck.body.insert_ident(expr_id, IdentType::Global(globalid));

                ty
            }

            Some(SymbolKind::Const(const_id)) => {
                let const_ = ck.sa.const_(const_id);
                ck.body.set_ty(expr_id, const_.ty());

                ck.body.insert_ident(expr_id, IdentType::Const(const_id));

                const_.ty()
            }

            Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => {
                // Extract type params from the single segment (e.g., None[Char])
                let type_params: Vec<SourceType> = path[0]
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type_id(ty))
                    .collect();

                check_enum_variant_without_args_id(
                    ck,
                    expr_id,
                    ck.expr_span(expr_id),
                    expected_ty,
                    enum_id,
                    SourceTypeArray::with(type_params),
                    variant_idx,
                )
            }

            None => {
                ck.report(
                    ck.expr_span(expr_id),
                    &UNKNOWN_IDENTIFIER,
                    args![ck.path_name(path)],
                );
                ty_error()
            }

            _ => {
                ck.report(ck.expr_span(expr_id), &VALUE_EXPECTED, args![]);
                ty_error()
            }
        };
    }

    // Multi-segment path: resolve through modules/enums
    let first_name = path[0].name;
    let mut sym = ck.symtable.get(first_name);

    // Resolve intermediate segments (all but the last one)
    for segment in &path[1..path.len() - 1] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                sym = symtable.get(segment.name);
            }
            _ => {
                ck.report(ck.expr_span(expr_id), &EXPECTED_MODULE, args![]);
                return ty_error();
            }
        }
    }

    // Handle the last segment
    let last_name = path[path.len() - 1].name;

    match sym {
        Some(SymbolKind::Module(module_id)) => {
            check_expr_path_module_by_name(ck, expr_id, expected_ty, module_id, last_name)
        }

        Some(SymbolKind::Enum(enum_id)) => {
            // Load AST for separator span (still needed for error reporting)
            let e = ck.syntax_by_id::<ast::AstPathExpr>(expr_id);
            let separator_span = e.last_separator().map(|t| t.span()).unwrap_or(e.span());
            let variant_name = ck.sa.interner.str(last_name).to_string();

            // Type params can be on either the enum segment or the variant segment
            // e.g., A[Int32]::V2 or A::V2[Int32]
            let type_params = if path.len() >= 2 {
                let enum_segment = &path[path.len() - 2];
                let variant_segment = &path[path.len() - 1];

                let enum_params: Vec<SourceType> = enum_segment
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type_id(ty))
                    .collect();
                let variant_params: Vec<SourceType> = variant_segment
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type_id(ty))
                    .collect();

                if !enum_params.is_empty() && !variant_params.is_empty() {
                    // Both have type params - error
                    // Get the span from AST for precise error reporting
                    let e = ck.syntax_by_id::<ast::AstPathExpr>(expr_id);
                    let segments: Vec<_> = e.segments().collect();
                    let error_span = segments
                        .last()
                        .and_then(|seg| seg.type_params_span())
                        .unwrap_or_else(|| ck.expr_span(expr_id));
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
                SourceTypeArray::empty()
            };

            check_enum_variant_without_args(
                ck,
                expr_id,
                separator_span,
                expected_ty,
                enum_id,
                type_params,
                variant_name,
            )
        }

        None => {
            ck.report(
                ck.expr_span(expr_id),
                &UNKNOWN_IDENTIFIER,
                args![ck.path_name(path)],
            );
            ty_error()
        }

        _ => {
            // Use first segment's span for error, matching PATH behavior
            let e = ck.syntax_by_id::<ast::AstPathExpr>(expr_id);
            let first_segment_span = e.segments().next().map(|t| t.span()).unwrap_or(e.span());
            ck.report(first_segment_span, &INVALID_LEFT_SIDE_OF_SEPARATOR, args![]);
            ty_error()
        }
    }
}

fn check_enum_variant_without_args(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expr_span: Span,
    _expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    name: String,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(expr_span, &NOT_ACCESSIBLE, args![]);
    }

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        enum_,
        &type_params,
        ck.file_id,
        expr_span,
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let interned_name = ck.sa.interner.intern(&name);

    if let Some(&value) = enum_.name_to_value().get(&interned_name) {
        let variant_id = enum_.variant_id_at(value as usize);
        let variant = ck.sa.variant(variant_id);

        if !variant.field_ids().is_empty() {
            ck.report(expr_span, &ENUM_VARIANT_MISSING_ARGUMENTS, args![]);
        }

        ck.body.insert_ident(
            expr_id,
            IdentType::EnumVariant(enum_id, type_params.clone(), value),
        );
    } else {
        ck.report(expr_span, &UNKNOWN_ENUM_VARIANT, args![name]);
    }

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.body.set_ty(expr_id, ty.clone());
        ty
    } else {
        ck.body.set_ty(expr_id, ty_error());
        ty_error()
    }
}

pub(super) fn check_enum_variant_without_args_id(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expr_span: Span,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(expr_span, &NOT_ACCESSIBLE, args![]);
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
        expr_span,
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let variant_id = enum_.variant_id_at(variant_idx as usize);
    let variant = ck.sa.variant(variant_id);

    if !variant.field_ids().is_empty() {
        ck.report(expr_span, &ENUM_VARIANT_MISSING_ARGUMENTS, args![]);
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

fn check_expr_path_module_by_name(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
    module_id: ModuleDefinitionId,
    element_name: Name,
) -> SourceType {
    let table = ck.sa.module_table(module_id);
    let sym = table.get(element_name);
    // Use the :: separator span for error reporting to match PATH behavior
    let node = ck.syntax_by_id::<ast::AstPathExpr>(expr_id);
    let separator_span = node
        .last_separator()
        .map(|t| t.span())
        .unwrap_or(node.span());

    match sym {
        Some(SymbolKind::Global(global_id)) => {
            if !global_accessible_from(ck.sa, global_id, ck.module_id) {
                ck.report(separator_span, &NOT_ACCESSIBLE, args![]);
            }

            let global_var = ck.sa.global(global_id);
            let ty = global_var.ty();
            ck.body.set_ty(expr_id, ty.clone());

            ck.body.insert_ident(expr_id, IdentType::Global(global_id));

            ty
        }

        Some(SymbolKind::Const(const_id)) => {
            if !const_accessible_from(ck.sa, const_id, ck.module_id) {
                ck.report(separator_span, &NOT_ACCESSIBLE, args![]);
            }

            let const_ = ck.sa.const_(const_id);
            ck.body.set_ty(expr_id, const_.ty());

            ck.body.insert_ident(expr_id, IdentType::Const(const_id));

            const_.ty()
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_variant_without_args_id(
            ck,
            expr_id,
            separator_span,
            expected_ty,
            enum_id,
            SourceTypeArray::empty(),
            variant_idx,
        ),

        None => {
            let module = ck.sa.module(module_id).name(ck.sa);
            let element_name_str = ck.sa.interner.str(element_name).to_string();
            ck.report(
                separator_span,
                &UNKNOWN_IDENTIFIER_IN_MODULE,
                args![module, element_name_str],
            );
            ty_error()
        }

        _ => {
            ck.sa
                .report(ck.file_id, separator_span, &VALUE_EXPECTED, args!());
            ty_error()
        }
    }
}
