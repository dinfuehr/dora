use dora_bytecode::Register;
use dora_parser::ast::{self, SyntaxNodeBase};

use super::gen_expr;
use super::if_::gen_expr_condition;
use crate::generator::{AstBytecodeGen, DataDest, LoopLabels};

pub(super) fn gen_expr_while(
    g: &mut AstBytecodeGen,
    node: ast::AstWhileExpr,
    _dest: DataDest,
) -> Register {
    let node_id = node.id();
    let cond_lbl = g.builder.define_label();
    let end_lbl = g.builder.create_label();
    g.builder.emit_loop_start();
    g.enter_block_context(node_id);

    gen_expr_condition(g, node.cond(), end_lbl);

    g.loops.push(LoopLabels::new(cond_lbl, end_lbl));
    let block_reg = gen_expr(g, node.block().into(), DataDest::Alloc);
    g.free_if_temp(block_reg);
    g.loops.pop().unwrap();
    g.builder.emit_jump_loop(cond_lbl);
    g.builder.bind_label(end_lbl);
    g.leave_block_context(node_id);
    g.ensure_unit_register()
}
