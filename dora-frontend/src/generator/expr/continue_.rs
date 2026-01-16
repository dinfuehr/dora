use dora_bytecode::Register;
use dora_parser::ast;

use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::ExprId;

pub(super) fn gen_expr_continue(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    _node: ast::AstContinueExpr,
    _dest: DataDest,
) -> Register {
    let cond = g.loops.last().unwrap().cond;
    g.builder.emit_jump_loop(cond);
    g.ensure_unit_register()
}
