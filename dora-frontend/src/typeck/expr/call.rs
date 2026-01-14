use dora_parser::TokenKind;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::{CallExpr, ExprId};
use crate::typeck::{CallArguments, TypeCheck, check_expr, check_expr_opt};
use crate::{SourceType, SourceTypeArray, ty::error as ty_error};

use crate::typeck::call::{
    check_expr_call_expr, check_expr_call_method, check_expr_call_path_name, check_expr_call_sym,
};

pub(crate) fn check_expr_call(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    expr: ast::AstCallExpr,
    _sema_expr: &CallExpr,
    expected_ty: SourceType,
) -> SourceType {
    let call_expr: ast::AstExpr = expr.clone().into();
    let callee = expr.callee();

    let arguments = create_call_arguments(ck, &expr);

    match callee.syntax_kind() {
        TokenKind::PATH_EXPR => {
            let expr_ident = callee.clone().as_path_expr();
            let segments: Vec<_> = expr_ident.segments().collect();

            // Get type params from the last segment if any
            let type_params = if let Some(last_segment) = segments.last() {
                let params: Vec<SourceType> = last_segment
                    .type_params()
                    .filter_map(|arg| arg.ty())
                    .map(|ty| ck.read_type(ty))
                    .collect();
                SourceTypeArray::with(params)
            } else {
                SourceTypeArray::empty()
            };

            if segments.len() == 1 {
                // Single segment: simple identifier lookup
                let sym = segments[0]
                    .name()
                    .map(|n| ck.symtable.get_string(ck.sa, n.text()))
                    .flatten();

                check_expr_call_sym(
                    ck,
                    call_expr.clone(),
                    expr,
                    expected_ty,
                    callee,
                    sym,
                    type_params,
                    arguments,
                )
            } else {
                // Multi-segment path
                check_expr_call_path_name(ck, expr, expected_ty, expr_ident, type_params, arguments)
            }
        }

        TokenKind::FIELD_EXPR => {
            let callee_span = callee.span();
            let expr_field = callee.as_field_expr();
            let object_type = check_expr(ck, expr_field.lhs(), SourceType::Any);

            let Some(name_token) = expr_field.name() else {
                ck.body.set_ty(expr.id(), ty_error());
                return ty_error();
            };

            let method_name = name_token.text().to_string();
            check_expr_call_method(
                ck,
                call_expr.clone(),
                expr.callee(),
                callee_span,
                object_type,
                method_name,
                SourceTypeArray::empty(),
                arguments,
            )
        }

        _ => {
            let expr_type = check_expr(ck, callee, SourceType::Any);
            check_expr_call_expr(ck, call_expr, expr_type, arguments)
        }
    }
}

pub(crate) fn create_call_arguments(ck: &mut TypeCheck, node: &ast::AstCallExpr) -> CallArguments {
    let mut arguments = CallArguments {
        arguments: Vec::new(),
        span: node.span(),
    };

    for arg in node.arg_list().items() {
        let ty = check_expr_opt(ck, arg.expr(), SourceType::Any);
        ck.body.set_ty(arg.id(), ty);

        arguments.arguments.push(arg);
    }

    arguments
}
