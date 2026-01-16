use dora_parser::ast;

use super::bin::check_expr_bin_and;
use super::is::check_expr_is_raw;
use crate::SourceType;
use crate::args;
use crate::error::diagnostics::{IF_BRANCH_TYPES_INCOMPATIBLE, IF_COND_TYPE};
use crate::expr_always_returns;
use crate::sema::{Expr, ExprId, IfExpr};
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr_id;

pub(crate) fn check_expr_if(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &IfExpr,
    expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    let ty = check_expr_condition(ck, sema_expr.cond);

    if !ty.is_bool() && !ty.is_error() {
        let expr_type = ck.ty_name(&ty);
        ck.report(
            ck.expr_span(sema_expr.cond),
            &IF_COND_TYPE,
            args!(expr_type),
        );
    }

    let then_type = check_expr_id(ck, sema_expr.then_expr, expected_ty.clone());

    ck.symtable.pop_level();

    let merged_type = if let Some(else_expr) = sema_expr.else_expr {
        let else_type = check_expr_id(ck, else_expr, expected_ty);

        let ast_file = ck.sa.file(ck.file_id).ast();
        let then_ast = ck.syntax_by_id::<ast::AstExpr>(sema_expr.then_expr);
        let else_ast = ck.syntax_by_id::<ast::AstExpr>(else_expr);

        if expr_always_returns(ast_file, then_ast) {
            else_type
        } else if expr_always_returns(ast_file, else_ast) {
            then_type
        } else if then_type.is_error() {
            else_type
        } else if else_type.is_error() {
            then_type
        } else if !then_type.allows(ck.sa, else_type.clone()) {
            let then_type_name = ck.ty_name(&then_type);
            let else_type_name = ck.ty_name(&else_type);
            ck.report(
                ck.expr_span(expr_id),
                &IF_BRANCH_TYPES_INCOMPATIBLE,
                args!(then_type_name, else_type_name),
            );
            then_type
        } else {
            then_type
        }
    } else {
        SourceType::Unit
    };

    ck.body.set_ty(expr_id, merged_type.clone());

    merged_type
}

pub(super) fn check_expr_condition(ck: &mut TypeCheck, cond: ExprId) -> SourceType {
    match ck.expr(cond) {
        Expr::Bin(bin_expr) if bin_expr.op == ast::BinOp::And => check_expr_bin_and(ck, cond),
        Expr::Is(is_expr) => check_expr_is_raw(ck, cond, is_expr, SourceType::Bool),
        _ => check_expr_id(ck, cond, SourceType::Bool),
    }
}
