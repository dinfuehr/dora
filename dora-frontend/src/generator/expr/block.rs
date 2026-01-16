use dora_bytecode::Register;
use dora_parser::ast;

use super::gen_expr;
use crate::generator::{AstBytecodeGen, DataDest};

pub(super) fn gen_expr_block(
    g: &mut AstBytecodeGen,
    block: ast::AstBlockExpr,
    dest: DataDest,
) -> Register {
    g.push_scope();

    for stmt in block.stmts_without_tail() {
        g.visit_stmt(stmt);
    }

    let result = if let Some(stmt) = block.tail() {
        let expr_stmt = stmt.as_expr_stmt();
        gen_expr(g, expr_stmt.expr(), dest)
    } else {
        g.ensure_unit_register()
    };

    g.pop_scope();

    result
}
