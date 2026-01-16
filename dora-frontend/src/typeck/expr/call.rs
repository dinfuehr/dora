use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::{CallExpr, Expr, ExprId};
use crate::typeck::{CallArguments, TypeCheck, check_expr_id, check_expr_opt};
use crate::{SourceType, SourceTypeArray};

use crate::typeck::call::{check_expr_call_expr, check_expr_call_path_name, check_expr_call_sym};

pub(crate) fn check_expr_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &CallExpr,
    expected_ty: SourceType,
) -> SourceType {
    let expr = ck.syntax_by_id::<ast::AstCallExpr>(expr_id);
    let call_expr: ast::AstExpr = expr.clone().into();

    let arguments = create_call_arguments(ck, expr_id);

    let callee_expr = ck.expr(sema_expr.callee);

    match callee_expr {
        Expr::Name(name_expr) => {
            let callee = expr.callee();
            let callee_path = callee.clone().as_path_expr();
            let segments: Vec<_> = callee_path.segments().collect();

            // Get type params from the last segment
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

            if name_expr.path.len() == 1 {
                // Single segment: simple identifier lookup
                let sym = ck.symtable.get(name_expr.path[0]);

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
                check_expr_call_path_name(
                    ck,
                    expr,
                    expected_ty,
                    callee_path,
                    type_params,
                    arguments,
                )
            }
        }

        _ => {
            let expr_type = check_expr_id(ck, sema_expr.callee, SourceType::Any);
            check_expr_call_expr(ck, call_expr, expr_type, arguments)
        }
    }
}

pub(crate) fn create_call_arguments(ck: &mut TypeCheck, expr_id: ExprId) -> CallArguments {
    let node = ck.syntax_by_id::<ast::AstCallExpr>(expr_id);

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
