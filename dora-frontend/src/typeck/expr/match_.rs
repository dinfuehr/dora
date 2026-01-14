use dora_parser::ast::{self, SyntaxNodeBase};

use crate::SourceType;
use crate::args;
use crate::error::diagnostics::{IF_COND_TYPE, MATCH_BRANCH_TYPES_INCOMPATIBLE};
use crate::sema::{ExprId, MatchExpr};
use crate::ty;
use crate::typeck::{TypeCheck, check_expr, check_expr_opt, check_pattern};

pub(crate) fn check_expr_match(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstMatchExpr,
    _sema_expr: &MatchExpr,
    expected_ty: SourceType,
) -> SourceType {
    let expr_type = check_expr_opt(ck, node.expr(), SourceType::Any);
    if let Some(ref ast_expr) = node.expr() {
        ck.body.set_ty(ast_expr.id(), expr_type.clone());
    }
    let mut result_type = ty::error();

    for arm in node.arms() {
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

    ck.body.set_ty(node.id(), result_type.clone());

    result_type
}

fn check_expr_match_arm(
    ck: &mut TypeCheck,
    arm: ast::AstMatchArm,
    expr_ty: SourceType,
    expected_ty: SourceType,
    result_type: &mut SourceType,
) {
    check_pattern(ck, arm.pattern(), expr_ty);

    if let Some(cond) = arm.cond() {
        let cond_ty = check_expr(ck, cond.clone(), SourceType::Bool);

        if !cond_ty.is_bool() && !cond_ty.is_error() {
            let cond_ty = ck.ty_name(&cond_ty);
            ck.report(cond.span(), &IF_COND_TYPE, args!(cond_ty));
        }
    }

    let arm_ty = check_expr(ck, arm.value(), expected_ty.clone());

    if result_type.is_error() {
        *result_type = arm_ty;
    } else if arm_ty.is_error() {
        // Ignore this arm.
    } else if !result_type.allows(ck.sa, arm_ty.clone()) {
        let result_type_name = ck.ty_name(&result_type);
        let arm_ty_name = ck.ty_name(&arm_ty);
        ck.report(
            arm.value().span(),
            &MATCH_BRANCH_TYPES_INCOMPATIBLE,
            args!(result_type_name, arm_ty_name),
        );
    }
}
