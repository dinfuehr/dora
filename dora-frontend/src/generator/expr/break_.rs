use dora_bytecode::Register;
use dora_parser::ast;

use crate::generator::{AstBytecodeGen, DataDest};

pub(super) fn gen_expr_break(
    g: &mut AstBytecodeGen,
    _node: ast::AstBreakExpr,
    _dest: DataDest,
) -> Register {
    let end = g.loops.last().unwrap().end;
    g.builder.emit_jump(end);
    g.ensure_unit_register()
}
