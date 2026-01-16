use dora_parser::ast::{self, SyntaxNodeBase};

use super::gen_expr;
use crate::generator::pattern::{destruct_pattern_or_fail, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest};

pub(in crate::generator) fn gen_stmt_expr(g: &mut AstBytecodeGen, stmt: ast::AstExprStmt) {
    let reg = gen_expr(g, stmt.expr(), DataDest::Alloc);
    g.free_if_temp(reg);
}

pub(in crate::generator) fn gen_stmt_let(g: &mut AstBytecodeGen, stmt: ast::AstLet) {
    setup_pattern_vars(g, stmt.pattern());

    if let Some(expr) = stmt.expr() {
        let ty = g.ty(expr.id());
        let value = gen_expr(g, expr, DataDest::Alloc);
        destruct_pattern_or_fail(g, stmt.pattern(), value, ty);
        g.free_if_temp(value);
    }
}
