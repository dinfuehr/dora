use crate::SourceType;
use crate::args;
use crate::error::diagnostics::INVALID_RETURN;
use crate::sema::{ExprId, ReturnExpr};
use crate::typeck::{TypeCheck, check_expr};

pub(crate) fn check_expr_return(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &ReturnExpr,
    _expected_ty: SourceType,
) -> SourceType {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = sema_expr
            .expr
            .map(|expr| check_expr(ck, expr, expected_ty.clone()))
            .unwrap_or(SourceType::Unit);

        ck.check_fct_return_type(
            expected_ty.clone(),
            ck.expr_span(expr_id),
            expr_type.clone(),
        );

        if expected_ty.is_ref() && expr_type.is_ref() {
            if let Some(expr_id) = sema_expr.expr {
                ck.check_ref_return_value(expr_id);
            }
        }
    } else {
        ck.sa
            .report(ck.file_id, ck.expr_span(expr_id), &INVALID_RETURN, args!());

        if let Some(expr) = sema_expr.expr {
            check_expr(ck, expr, SourceType::Any);
        }
    }

    SourceType::Unit
}
