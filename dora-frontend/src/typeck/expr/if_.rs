use dora_parser::ast::{self, SyntaxNodeBase};

use super::bin::check_expr_bin_and;
use super::is::check_expr_is_raw;
use crate::SourceType;
use crate::args;
use crate::error::diagnostics::{IF_BRANCH_TYPES_INCOMPATIBLE, IF_COND_TYPE};
use crate::expr_always_returns;
use crate::sema::{Expr, ExprId, IfExpr};
use crate::typeck::expr::check_expr_id;
use crate::typeck::{TypeCheck, check_expr};

pub(crate) fn check_expr_if(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstIfExpr,
    _sema_expr: &IfExpr,
    expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    let ty = check_expr_condition(ck, _sema_expr.cond);

    if !ty.is_bool() && !ty.is_error() {
        let expr_type = ck.ty_name(&ty);
        ck.report(node.cond().span(), &IF_COND_TYPE, args!(expr_type));
    }

    let then_block = node.then_block();
    let then_type = check_expr(ck, then_block, expected_ty.clone());

    ck.symtable.pop_level();

    let merged_type = if let Some(else_block) = node.else_block() {
        let else_type = check_expr(ck, else_block.clone(), expected_ty);

        let ast_file = ck.sa.file(ck.file_id).ast();

        if expr_always_returns(ast_file, node.then_block()) {
            else_type
        } else if expr_always_returns(ast_file, else_block) {
            then_type
        } else if then_type.is_error() {
            else_type
        } else if else_type.is_error() {
            then_type
        } else if !then_type.allows(ck.sa, else_type.clone()) {
            let then_type_name = ck.ty_name(&then_type);
            let else_type_name = ck.ty_name(&else_type);
            ck.report(
                node.span(),
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

    ck.body.set_ty(node.id(), merged_type.clone());

    merged_type
}

pub(super) fn check_expr_condition(ck: &mut TypeCheck, cond: ExprId) -> SourceType {
    match ck.expr(cond) {
        Expr::Bin(bin_expr) if bin_expr.op == ast::BinOp::And => {
            check_expr_bin_and(ck, cond, SourceType::Bool)
        }
        Expr::Is(is_expr) => check_expr_is_raw(ck, cond, is_expr, SourceType::Bool),
        _ => check_expr_id(ck, cond, SourceType::Bool),
    }
}
