use dora_bytecode::Register;
use dora_parser::ast;

use crate::generator::{AstBytecodeGen, DataDest};

pub(super) fn gen_expr_continue(
    g: &mut AstBytecodeGen,
    _node: ast::AstContinueExpr,
    _dest: DataDest,
) -> Register {
    let cond = g.loops.last().unwrap().cond;
    g.builder.emit_jump_loop(cond);
    g.ensure_unit_register()
}
