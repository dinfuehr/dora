use crate::SourceType;
use crate::args;
use crate::error::diagnostics::{IF_COND_TYPE, MATCH_BRANCH_TYPES_INCOMPATIBLE};
use crate::sema::{ExprId, MatchArmExpr, MatchExpr};
use crate::ty;
use crate::typeck::{TypeCheck, check_expr, check_pattern};

pub(crate) fn check_expr_match(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &MatchExpr,
    expected_ty: SourceType,
) -> SourceType {
    let expr_type = if let Some(match_expr_id) = sema_expr.expr {
        let ty = check_expr(ck, match_expr_id, SourceType::Any);
        ck.body.set_ty(match_expr_id, ty.clone());
        ty
    } else {
        SourceType::Error
    };

    let mut result_type = ty::error();

    for arm in &sema_expr.arms {
        ck.symtable.push_level();
        check_expr_match_arm(
            ck,
            arm,
            expr_type.clone(),
            expected_ty.clone(),
            &mut result_type,
        );
        ck.symtable.pop_level();
    }

    ck.body.set_ty(expr_id, result_type.clone());

    result_type
}

fn check_expr_match_arm(
    ck: &mut TypeCheck,
    arm: &MatchArmExpr,
    expr_ty: SourceType,
    expected_ty: SourceType,
    result_type: &mut SourceType,
) {
    check_pattern(ck, arm.pattern, expr_ty);

    if let Some(cond_id) = arm.cond {
        let cond_ty = check_expr(ck, cond_id, SourceType::Bool);

        if !cond_ty.is_bool() && !cond_ty.is_error() {
            let cond_ty = ck.ty_name(&cond_ty);
            ck.report(ck.expr_span(cond_id), &IF_COND_TYPE, args!(cond_ty));
        }
    }

    let arm_ty = check_expr(ck, arm.value, expected_ty.clone());

    if result_type.is_error() {
        *result_type = arm_ty;
    } else if arm_ty.is_error() {
        // Ignore this arm.
    } else if !result_type.allows(ck.sa, arm_ty.clone()) {
        let result_type_name = ck.ty_name(&result_type);
        let arm_ty_name = ck.ty_name(&arm_ty);
        ck.report(
            ck.expr_span(arm.value),
            &MATCH_BRANCH_TYPES_INCOMPATIBLE,
            args!(result_type_name, arm_ty_name),
        );
    }
}
