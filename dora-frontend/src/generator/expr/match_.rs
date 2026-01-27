use dora_bytecode::Register;
use dora_parser::Span;

use super::{ensure_register, gen_expr};
use crate::generator::pattern::{destruct_pattern, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, MatchExpr};
use crate::ty::SourceTypeArray;

pub(super) fn gen_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MatchExpr,
    dest: DataDest,
) -> Register {
    let result_ty = g.ty(expr_id);
    let expr_id_inner = e.expr.expect("missing match expr");
    let expr_ty = g.ty(expr_id_inner);

    let result_bc_ty = g.emitter.convert_ty_reg(g.sa, result_ty);
    let dest = ensure_register(g, dest, result_bc_ty);

    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, expr_id_inner, DataDest::Alloc);

    let num_arms = e.arms.len();

    let mut arm_labels = Vec::with_capacity(num_arms);

    for _ in 0..num_arms {
        arm_labels.push(g.builder.create_label());
    }

    arm_labels.push(fallthrough_lbl);

    for (idx, arm) in e.arms.iter().enumerate() {
        let arm_lbl = arm_labels[idx];
        g.builder.bind_label(arm_lbl);

        let next_arm_lbl = arm_labels[idx + 1];

        g.push_scope();

        setup_pattern_vars(g, arm.pattern);
        destruct_pattern(
            g,
            arm.pattern,
            expr_reg,
            expr_ty.clone(),
            Some(next_arm_lbl),
        );

        if let Some(cond_id) = arm.cond {
            let cond_reg = gen_expr(g, cond_id, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, next_arm_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr(g, arm.value, DataDest::Reg(dest));

        g.builder.emit_jump(merge_lbl);
        g.pop_scope();
    }

    g.builder.bind_label(fallthrough_lbl);
    let span = g.span_for_expr(expr_id);
    gen_unreachable(g, span);

    g.builder.bind_label(merge_lbl);
    g.free_if_temp(expr_reg);

    dest
}

fn gen_unreachable(g: &mut AstBytecodeGen, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty_reg(g.sa, return_type.clone());
    let dest = g.alloc_temp(register_bty);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_id = g
        .emitter
        .convert_function_id(g.sa, g.sa.known.functions.unreachable());
    let fct_idx = g.builder.add_const_fct_types(fct_id, fct_type_params);
    g.builder.emit_invoke_direct(dest, fct_idx, g.loc(span));
    g.builder.emit_ret(dest);
    g.free_temp(dest);
}
