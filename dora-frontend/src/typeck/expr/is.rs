use crate::SourceType;
use crate::sema::{ExprId, IsExpr};
use crate::typeck::{TypeCheck, check_expr, check_pattern};

pub(super) fn check_expr_is(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    expr: &IsExpr,
    expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();
    let ty = check_expr_is_raw(ck, expr, expected_ty);
    ck.symtable.pop_level();
    ty
}

pub(crate) fn check_expr_is_raw(
    ck: &mut TypeCheck,
    expr: &IsExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let value_type = check_expr(ck, expr.value, SourceType::Any);
    ck.body.set_ty(expr.value, value_type.clone());
    check_pattern(ck, expr.pattern, value_type.clone());
    SourceType::Bool
}
