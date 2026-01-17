use crate::sema::{CallExpr, Expr, ExprId};
use crate::typeck::{TypeCheck, check_expr};
use crate::{SourceType, SourceTypeArray};

use crate::typeck::call::{check_expr_call_expr, check_expr_call_path_name, check_expr_call_sym};

pub(crate) fn check_expr_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &CallExpr,
    expected_ty: SourceType,
) -> SourceType {
    check_call_arguments(ck, sema_expr);
    let call_expr_id = expr_id;

    let callee_expr = ck.expr(sema_expr.callee);

    match callee_expr {
        Expr::Path(name_expr) => {
            // Get type params from the last segment
            let type_params = if let Some(last_segment) = name_expr.path.last() {
                let params: Vec<SourceType> = last_segment
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type_id(ty))
                    .collect();
                SourceTypeArray::with(params)
            } else {
                SourceTypeArray::empty()
            };

            if name_expr.path.len() == 1 {
                // Single segment: simple identifier lookup
                let sym = ck.symtable.get(name_expr.path[0].name);

                check_expr_call_sym(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    sym,
                    type_params,
                    call_expr_id,
                )
            } else {
                // Multi-segment path
                check_expr_call_path_name(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    type_params,
                    call_expr_id,
                )
            }
        }

        _ => {
            let expr_type = check_expr(ck, sema_expr.callee, SourceType::Any);
            check_expr_call_expr(ck, expr_id, expr_type, call_expr_id)
        }
    }
}

pub(crate) fn check_call_arguments(ck: &mut TypeCheck, sema_expr: &CallExpr) {
    for sema_arg in &sema_expr.args {
        let ty = check_expr(ck, sema_arg.expr, SourceType::Any);
        ck.body.set_ty(sema_arg.expr, ty);
    }
}
