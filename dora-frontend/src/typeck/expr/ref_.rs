use crate::SourceType;
use crate::sema::{ExprId, RefExpr};
use crate::typeck::TypeCheck;

pub(super) fn check_expr_ref(
    _ck: &mut TypeCheck,
    _expr_id: ExprId,
    _sema_expr: &RefExpr,
    _expected_ty: SourceType,
) -> SourceType {
    unimplemented!()
}
