use crate::SourceType;
use crate::args;
use crate::error::diagnostics::{REF_MUT_REQUIRES_MUTABLE, REF_REQUIRES_ADDRESSABLE};
use crate::sema::{CallType, Expr, ExprId, IdentType, Intrinsic, RefExpr};
use crate::ty::error as ty_error;
use crate::typeck::TypeCheck;
use crate::typeck::expr::{ExprContext, check_expr_with_context};

pub(in crate::typeck) enum RefTarget {
    Local { writable: bool },
    Global { writable: bool },
    ClassField { writable: bool },
    ArrayElement { writable: bool },
    RefReturningCall { writable: bool },
    Invalid,
}

impl RefTarget {
    pub(in crate::typeck) fn is_returnable(&self) -> bool {
        match self {
            RefTarget::Global { .. }
            | RefTarget::ClassField { .. }
            | RefTarget::ArrayElement { .. }
            | RefTarget::RefReturningCall { .. } => true,
            RefTarget::Local { .. } | RefTarget::Invalid => false,
        }
    }

    fn is_writable(&self) -> bool {
        match self {
            RefTarget::Local { writable }
            | RefTarget::Global { writable }
            | RefTarget::ClassField { writable }
            | RefTarget::ArrayElement { writable }
            | RefTarget::RefReturningCall { writable } => *writable,
            RefTarget::Invalid => true,
        }
    }

    fn restrict_writability(&mut self, writable: bool) {
        match self {
            RefTarget::Local {
                writable: target_writable,
            }
            | RefTarget::Global {
                writable: target_writable,
            }
            | RefTarget::ClassField {
                writable: target_writable,
            }
            | RefTarget::ArrayElement {
                writable: target_writable,
            }
            | RefTarget::RefReturningCall {
                writable: target_writable,
            } => *target_writable &= writable,
            RefTarget::Invalid => {}
        }
    }
}

pub(super) fn check_expr_ref(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &RefExpr,
    expected_ty: SourceType,
) -> SourceType {
    let inner_ty = check_expr_with_context(ck, sema_expr.expr, expected_ty, ExprContext::Place);

    if inner_ty.is_error() {
        return ty_error();
    }

    let ref_target = compute_ref_target(ck, sema_expr.expr);
    if sema_expr.is_mut && !ref_target.is_writable() {
        ck.report(ck.expr_span(expr_id), &REF_MUT_REQUIRES_MUTABLE, args![]);
    }

    let ref_ty = match ref_target {
        RefTarget::Local { .. }
        | RefTarget::Global { .. }
        | RefTarget::ClassField { .. }
        | RefTarget::ArrayElement { .. } => SourceType::Ref {
            ty: Box::new(inner_ty),
            is_mut: sema_expr.is_mut,
        },
        // A call that returns a reference already provides the reference to forward.
        RefTarget::RefReturningCall { .. } => {
            let SourceType::Ref { ty, .. } = inner_ty else {
                unreachable!()
            };

            SourceType::Ref {
                ty,
                is_mut: sema_expr.is_mut,
            }
        }
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
            Some(IdentType::Var(var_id)) => {
                let var = ck.vars.get_var(ck.vars.nested_var_id(var_id));
                let writable = if var.ty.is_ref() {
                    var.ty.is_mut_ref()
                } else {
                    var.mutable
                };
                RefTarget::Local { writable }
            }
            Some(IdentType::Context { writable, .. }) => RefTarget::Local { writable },
            Some(IdentType::Global(global_id)) => RefTarget::Global {
                writable: ck.sa.global(global_id).mutable,
            },
            _ => RefTarget::Invalid,
        },
        Expr::Field(field_expr) => match ck.body.get_ident(expr_id) {
            Some(IdentType::ClassField(object_ty, field_index)) => {
                let SourceType::Class(class_id, _) = object_ty else {
                    unreachable!()
                };
                let field_id = ck.sa.class(class_id).field_id(field_index);
                RefTarget::ClassField {
                    writable: ck.sa.field(field_id).mutable,
                }
            }
            Some(IdentType::StructField(object_ty, field_index)) => {
                let SourceType::Struct(struct_id, _) = object_ty else {
                    unreachable!()
                };
                let field_id = ck.sa.struct_(struct_id).field_id(field_index);
                let mut ref_target = compute_ref_target(ck, field_expr.lhs);
                ref_target.restrict_writability(ck.sa.field(field_id).mutable);
                ref_target
            }
            Some(IdentType::TupleField(..)) => compute_ref_target(ck, field_expr.lhs),
            _ => RefTarget::Invalid,
        },
        Expr::Call(..) | Expr::MethodCall(..) => {
            let ty = ck.body.ty(expr_id);
            if ty.is_ref() {
                RefTarget::RefReturningCall {
                    writable: ty.is_mut_ref(),
                }
            } else if is_array_get(ck, expr_id) {
                RefTarget::ArrayElement { writable: true }
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
