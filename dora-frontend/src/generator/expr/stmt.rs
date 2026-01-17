use super::gen_expr;
use crate::generator::pattern::{destruct_pattern_or_fail, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, LetStmt};

pub(in crate::generator) fn gen_stmt_expr(g: &mut AstBytecodeGen, expr_id: ExprId) {
    let reg = gen_expr(g, expr_id, DataDest::Alloc);
    g.free_if_temp(reg);
}

pub(in crate::generator) fn gen_stmt_let(g: &mut AstBytecodeGen, stmt: &LetStmt) {
    // Convert PatternId to AstPattern for now
    let ast_pattern = g.ast_pattern_for_id(stmt.pattern);
    setup_pattern_vars(g, ast_pattern.clone());

    if let Some(expr_id) = stmt.expr {
        let ty = g.ty(expr_id);
        let value = gen_expr(g, expr_id, DataDest::Alloc);
        destruct_pattern_or_fail(g, ast_pattern, value, ty);
        g.free_if_temp(value);
    }
}
