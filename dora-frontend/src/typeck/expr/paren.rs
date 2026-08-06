use crate::SourceType;
use crate::sema::ExprId;
use crate::typeck::TypeCheck;
use crate::typeck::expr::{ExprContext, check_expr_with_context};

pub(super) fn check_expr_paren(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    subexpr_id: ExprId,
    expected_ty: SourceType,
    context: ExprContext,
) -> SourceType {
    let ty = check_expr_with_context(ck, subexpr_id, expected_ty, context);
    ck.body.set_ty(expr_id, ty.clone());

    ty
}
