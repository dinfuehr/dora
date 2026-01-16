use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{ensure_register, gen_expr};
use crate::generator::pattern::destruct_pattern;
use crate::generator::{AstBytecodeGen, DataDest};

pub(super) fn gen_expr_is(
    g: &mut AstBytecodeGen,
    node: ast::AstIsExpr,
    dest: DataDest,
) -> Register {
    let ty = g.ty(node.value().id());
    let value_reg = gen_expr(g, node.value(), DataDest::Alloc);

    g.push_scope();
    let mismatch_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();
    destruct_pattern(g, node.pattern(), value_reg, ty, Some(mismatch_lbl));
    let dest = ensure_register(g, dest, BytecodeType::Bool);
    g.builder.emit_const_true(dest);
    g.builder.emit_jump(merge_lbl);
    g.builder.bind_label(mismatch_lbl);
    g.builder.emit_const_false(dest);
    g.builder.bind_label(merge_lbl);
    g.pop_scope();

    g.free_if_temp(value_reg);

    dest
}
