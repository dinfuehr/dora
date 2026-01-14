use crate::args;
use crate::error::diagnostics::THIS_UNAVAILABLE;
use crate::sema::{ExprId, NestedVarId};
use crate::typeck::TypeCheck;
use crate::{SourceType, ty::error as ty_error};

pub(super) fn check_expr_this(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.is_self_available {
        ck.report_id(expr_id, &THIS_UNAVAILABLE, args!());
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    }

    assert!(ck.is_self_available);
    let var_id = NestedVarId(0);
    let ident = ck.maybe_allocate_in_context(var_id);
    ck.body.insert_ident(expr_id, ident);

    let var = ck.vars.get_var(var_id);
    ck.body.set_ty(expr_id, var.ty.clone());
    var.ty.clone()
}
