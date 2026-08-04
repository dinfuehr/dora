use crate::SourceType;
use crate::args;
use crate::error::diagnostics::REF_REQUIRES_ADDRESSABLE;
use crate::sema::{CallType, Expr, ExprId, IdentType, Intrinsic, RefExpr};
use crate::ty::error as ty_error;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::typeck) enum RefTarget {
    Local,
    Global,
    ClassField,
    ArrayElement,
    RefReturningCall,
    Invalid,
}

impl RefTarget {
    pub(in crate::typeck) fn is_returnable(self) -> bool {
        match self {
            RefTarget::Global
            | RefTarget::ClassField
            | RefTarget::ArrayElement
            | RefTarget::RefReturningCall => true,
            RefTarget::Local | RefTarget::Invalid => false,
        }
    }
}

pub(super) fn check_expr_ref(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &RefExpr,
    expected_ty: SourceType,
) -> SourceType {
    let inner_ty = check_expr(ck, sema_expr.expr, expected_ty);

    if inner_ty.is_error() {
        return ty_error();
    }

    let ref_ty = match compute_ref_target(ck, sema_expr.expr) {
        RefTarget::Local | RefTarget::Global | RefTarget::ClassField | RefTarget::ArrayElement => {
            SourceType::Ref(Box::new(inner_ty))
        }
        // A call that returns a reference already provides the reference to forward.
        RefTarget::RefReturningCall => inner_ty,
        RefTarget::Invalid => {
            ck.report(ck.expr_span(expr_id), &REF_REQUIRES_ADDRESSABLE, args![]);
            return ty_error();
        }
    };

    ck.body.set_ty(expr_id, ref_ty.clone());

    ref_ty
}

pub(in crate::typeck) fn compute_ref_target(ck: &TypeCheck, expr_id: ExprId) -> RefTarget {
    match ck.expr(expr_id) {
        Expr::Path(..) => match ck.body.get_ident(expr_id) {
            Some(IdentType::Var(..) | IdentType::Context(..)) => RefTarget::Local,
            Some(IdentType::Global(..)) => RefTarget::Global,
            _ => RefTarget::Invalid,
        },
        Expr::Field(field_expr) => match ck.body.get_ident(expr_id) {
            Some(IdentType::ClassField(..)) => RefTarget::ClassField,
            Some(IdentType::StructField(..) | IdentType::TupleField(..)) => {
                compute_ref_target(ck, field_expr.lhs)
            }
            _ => RefTarget::Invalid,
        },
        Expr::Call(..) | Expr::MethodCall(..) => {
            if ck.body.ty(expr_id).is_ref() {
                RefTarget::RefReturningCall
            } else if is_array_get(ck, expr_id) {
                RefTarget::ArrayElement
            } else {
                RefTarget::Invalid
            }
        }
        Expr::Paren(inner_expr_id) => compute_ref_target(ck, *inner_expr_id),
        _ => RefTarget::Invalid,
    }
}

pub(crate) fn is_array_get(ck: &TypeCheck, expr_id: ExprId) -> bool {
    let Some(call_type) = ck.body.get_call_type(expr_id) else {
        return false;
    };

    let CallType::Index(_, fct_id, _) = call_type.as_ref() else {
        return false;
    };

    ck.sa.fct(*fct_id).intrinsic.get().copied() == Some(Intrinsic::ArrayGet)
}
