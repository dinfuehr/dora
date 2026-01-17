use dora_bytecode::Register;

use super::gen_expr;
use super::if_::gen_expr_condition_id;
use crate::generator::{AstBytecodeGen, DataDest, LoopLabels};
use crate::sema::{ExprId, WhileExpr};

pub(super) fn gen_expr_while(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &WhileExpr,
    _dest: DataDest,
) -> Register {
    let cond_lbl = g.builder.define_label();
    let end_lbl = g.builder.create_label();
    g.builder.emit_loop_start();
    g.enter_block_context(expr_id);

    gen_expr_condition_id(g, e.cond, end_lbl);

    g.loops.push(LoopLabels::new(cond_lbl, end_lbl));
    let block_reg = gen_expr(g, e.block, DataDest::Alloc);
    g.free_if_temp(block_reg);
    g.loops.pop().unwrap();
    g.builder.emit_jump_loop(cond_lbl);
    g.builder.bind_label(end_lbl);
    g.leave_block_context(expr_id);
    g.ensure_unit_register()
}
