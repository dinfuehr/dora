use super::if_::check_expr_condition;
use crate::SourceType;
use crate::args;
use crate::error::diagnostics::WHILE_COND_TYPE;
use crate::sema::{ExprId, WhileExpr};
use crate::typeck::{TypeCheck, check_expr_id};

pub(crate) fn check_expr_while(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expr: &WhileExpr,
    _expected_ty: SourceType,
) -> SourceType {
    ck.enter_block_scope();

    let cond_ty = check_expr_condition(ck, expr.cond);

    if !cond_ty.is_error() && !cond_ty.is_bool() {
        let cond_ty = ck.ty_name(&cond_ty);
        ck.report(ck.expr_span(expr.cond), &WHILE_COND_TYPE, args!(cond_ty));
    }

    check_loop_body(ck, expr.block);
    ck.leave_block_scope(expr_id);
    SourceType::Unit
}

pub(super) fn check_loop_body(ck: &mut TypeCheck, block: ExprId) {
    let old_in_loop = ck.in_loop;
    ck.in_loop = true;
    check_expr_id(ck, block, SourceType::Any);
    ck.in_loop = old_in_loop;
}
