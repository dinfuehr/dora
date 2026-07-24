use crate::SourceType;
use crate::sema::{ExprId, TupleExpr, create_tuple};
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;

pub(super) fn check_expr_tuple(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &TupleExpr,
    expected_ty: SourceType,
) -> SourceType {
    if sema_expr.values.is_empty() {
        ck.body.set_ty(expr_id, SourceType::Unit);
        return SourceType::Unit;
    }

    let expected_subtypes = expected_ty.tuple_subtypes();
    let mut subtypes = Vec::new();

    for (idx, &value_id) in sema_expr.values.iter().enumerate() {
        let expected_subtype = expected_subtypes
            .as_ref()
            .and_then(|subtypes| subtypes.types().get(idx).cloned())
            .unwrap_or(SourceType::Any);
        let subtype = check_expr(ck, value_id, expected_subtype);
        subtypes.push(subtype);
    }

    let ty = create_tuple(ck.sa, subtypes);
    ck.body.set_ty(expr_id, ty.clone());

    ty
}
