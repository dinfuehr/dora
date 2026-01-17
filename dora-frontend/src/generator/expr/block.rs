use dora_bytecode::Register;

use super::gen_expr;
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{BlockExpr, ExprId};

pub(super) fn gen_expr_block(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: &BlockExpr,
    dest: DataDest,
) -> Register {
    g.push_scope();

    for stmt_id in &e.stmts {
        g.visit_stmt(*stmt_id);
    }

    let result = if let Some(expr_id) = e.expr {
        gen_expr(g, expr_id, dest)
    } else {
        g.ensure_unit_register()
    };

    g.pop_scope();

    result
}
