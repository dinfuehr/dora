use dora_bytecode::Register;
use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

use super::{ensure_register, gen_expr};
use crate::expr_always_returns;
use crate::flatten_and;
use crate::generator::pattern::{destruct_pattern, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest, Label};

pub(super) fn gen_expr_if(
    g: &mut AstBytecodeGen,
    expr: ast::AstIfExpr,
    dest: DataDest,
) -> Register {
    let expr_id = expr.id();
    let ty = g.ty(expr_id);

    let dest = ensure_register(g, dest, g.emitter.convert_ty_reg(ty));
    let else_lbl = g.builder.create_label();

    g.push_scope();

    gen_expr_condition(g, expr.cond(), else_lbl);

    gen_expr(g, expr.then_block(), DataDest::Reg(dest));

    g.pop_scope();

    if let Some(else_block) = expr.else_block() {
        let end_lbl = g.builder.create_label();

        if !expr_always_returns(&g.sa.file(g.file_id).ast(), expr.then_block()) {
            g.builder.emit_jump(end_lbl);
        }

        g.builder.bind_label(else_lbl);
        gen_expr(g, else_block, DataDest::Reg(dest));
        g.builder.bind_label(end_lbl);
    } else {
        g.builder.bind_label(else_lbl);
    }

    dest
}

pub(super) fn gen_expr_condition(g: &mut AstBytecodeGen, expr: AstExpr, false_lbl: Label) {
    if let Some(bin_expr) = expr.clone().to_bin_expr() {
        if bin_expr.op() == ast::BinOp::And {
            emit_and_for_condition(g, bin_expr, false_lbl);
            return;
        }
    }

    if let Some(is_expr) = expr.clone().to_is_expr() {
        emit_is(g, is_expr, false_lbl);
    } else {
        let cond_reg = gen_expr(g, expr, DataDest::Alloc);
        g.builder.emit_jump_if_false(cond_reg, false_lbl);
        g.free_if_temp(cond_reg);
    }
}

fn emit_and_for_condition(g: &mut AstBytecodeGen, expr: ast::AstBinExpr, false_lbl: Label) {
    let conditions = flatten_and(expr);

    for cond in conditions {
        if cond.is_is_expr() {
            let is_expr = cond.as_is_expr();
            emit_is(g, is_expr, false_lbl);
        } else {
            let dest = gen_expr(g, cond, DataDest::Alloc);
            g.builder.emit_jump_if_false(dest, false_lbl);
            g.free_if_temp(dest);
        }
    }
}

pub(super) fn emit_is(g: &mut AstBytecodeGen, expr: ast::AstIsExpr, false_lbl: Label) {
    let value = gen_expr(g, expr.value(), DataDest::Alloc);
    let ty = g.ty(expr.value().id());
    setup_pattern_vars(g, expr.pattern());
    destruct_pattern(g, expr.pattern(), value, ty, Some(false_lbl));
    g.free_if_temp(value);
}
