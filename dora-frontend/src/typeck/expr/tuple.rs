use crate::SourceType;
use crate::sema::{ExprId, TupleExpr, create_tuple};
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr_id;

pub(super) fn check_expr_tuple(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &TupleExpr,
    _expected_ty: SourceType,
) -> SourceType {
    if sema_expr.values.is_empty() {
        ck.body.set_ty(expr_id, SourceType::Unit);
        return SourceType::Unit;
    }

    let mut subtypes = Vec::new();

    for &value_id in &sema_expr.values {
        let subtype = check_expr_id(ck, value_id, SourceType::Any);
        subtypes.push(subtype);
    }

    let ty = create_tuple(ck.sa, subtypes);
    ck.body.set_ty(expr_id, ty.clone());

    ty
}
