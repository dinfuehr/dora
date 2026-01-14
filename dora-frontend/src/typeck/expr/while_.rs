use dora_parser::ast::{self, SyntaxNodeBase};

use super::if_::check_expr_condition;
use crate::SourceType;
use crate::args;
use crate::error::diagnostics::WHILE_COND_TYPE;
use crate::sema::{ExprId, WhileExpr};
use crate::typeck::{TypeCheck, check_expr};

pub(crate) fn check_expr_while(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstWhileExpr,
    expr: &WhileExpr,
    _expected_ty: SourceType,
) -> SourceType {
    ck.enter_block_scope();

    let cond_ty = check_expr_condition(ck, expr.cond);

    if !cond_ty.is_error() && !cond_ty.is_bool() {
        let cond_ty = ck.ty_name(&cond_ty);
        ck.report(node.span(), &WHILE_COND_TYPE, args!(cond_ty));
    }

    let block_expr = node.block();
    check_loop_body(ck, block_expr);
    ck.leave_block_scope(node.id());
    SourceType::Unit
}

pub(super) fn check_loop_body(ck: &mut TypeCheck, block: ast::AstBlockExpr) {
    let old_in_loop = ck.in_loop;
    ck.in_loop = true;
    check_expr(ck, block.into(), SourceType::Any);
    ck.in_loop = old_in_loop;
}
