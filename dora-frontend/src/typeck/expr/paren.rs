use crate::SourceType;
use crate::sema::ExprId;
use crate::typeck::{TypeCheck, check_expr};

pub(super) fn check_expr_paren(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    subexpr_id: ExprId,
    _expected_ty: SourceType,
) -> SourceType {
    let ty = check_expr(ck, subexpr_id, SourceType::Any);
    ck.body.set_ty(expr_id, ty.clone());

    ty
}
