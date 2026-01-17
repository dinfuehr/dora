use dora_bytecode::Register;
use dora_parser::ast::{self, AstExpr};

use super::{ensure_register, gen_expr};
use crate::expr_always_returns;
use crate::generator::pattern::{destruct_pattern, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest, Label};
use crate::sema::{Expr, ExprId, IfExpr};

pub(super) fn gen_expr_if(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &IfExpr,
    dest: DataDest,
) -> Register {
    let ty = g.ty(expr_id);

    let dest = ensure_register(g, dest, g.emitter.convert_ty_reg(ty));
    let else_lbl = g.builder.create_label();

    g.push_scope();

    gen_expr_condition_id(g, e.cond, else_lbl);

    gen_expr(g, e.then_expr, DataDest::Reg(dest));

    g.pop_scope();

    if let Some(else_expr_id) = e.else_expr {
        let end_lbl = g.builder.create_label();

        // Check if return is needed using AST for now
        let ptr = g.analysis.exprs().syntax_node_ptr(e.then_expr);
        let then_ast = g.sa.syntax::<AstExpr>(g.file_id, ptr);
        if !expr_always_returns(&g.sa.file(g.file_id).ast(), then_ast) {
            g.builder.emit_jump(end_lbl);
        }

        g.builder.bind_label(else_lbl);
        gen_expr(g, else_expr_id, DataDest::Reg(dest));
        g.builder.bind_label(end_lbl);
    } else {
        g.builder.bind_label(else_lbl);
    }

    dest
}

/// Generate condition check using ExprId from HIR
pub(super) fn gen_expr_condition_id(g: &mut AstBytecodeGen, expr_id: ExprId, false_lbl: Label) {
    let expr = g.analysis.expr(expr_id);

    // Check if it's a binary AND expression
    if let Expr::Bin(bin_expr) = expr {
        if bin_expr.op == ast::BinOp::And {
            emit_and_for_condition_id(g, expr_id, bin_expr.lhs, bin_expr.rhs, false_lbl);
            return;
        }
    }

    // Check if it's an is expression
    if let Expr::Is(is_expr) = expr {
        emit_is_id(g, expr_id, is_expr.value, is_expr.pattern, false_lbl);
    } else {
        let cond_reg = gen_expr(g, expr_id, DataDest::Alloc);
        g.builder.emit_jump_if_false(cond_reg, false_lbl);
        g.free_if_temp(cond_reg);
    }
}

/// Helper for HIR-based AND condition checking
fn emit_and_for_condition_id(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    lhs: ExprId,
    rhs: ExprId,
    false_lbl: Label,
) {
    // Process left-hand side
    gen_expr_condition_id(g, lhs, false_lbl);
    // Process right-hand side
    gen_expr_condition_id(g, rhs, false_lbl);
}

/// Helper for HIR-based is expression checking
fn emit_is_id(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    value_id: ExprId,
    pattern_id: crate::sema::PatternId,
    false_lbl: Label,
) {
    let value = gen_expr(g, value_id, DataDest::Alloc);
    let ty = g.ty(value_id);
    setup_pattern_vars(g, pattern_id);
    destruct_pattern(g, pattern_id, value, ty, Some(false_lbl));
    g.free_if_temp(value);
}

/// Emit is expression checking (used by bin.rs)
pub(super) fn emit_is(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    is_expr: &crate::sema::IsExpr,
    false_lbl: Label,
) {
    emit_is_id(g, expr_id, is_expr.value, is_expr.pattern, false_lbl);
}
