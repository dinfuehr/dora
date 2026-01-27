use dora_bytecode::{BytecodeType, Register};

use super::{emit_invoke_direct, gen_expr};
use crate::generator::pattern::{destruct_pattern_or_fail, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest, LoopLabels};
use crate::sema::{ExprId, ForExpr};
use crate::ty::SourceTypeArray;

pub(super) fn gen_expr_for(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &ForExpr,
    _dest: DataDest,
) -> Register {
    g.push_scope();
    let for_type_info = g.analysis.get_for_type_info(expr_id).expect("missing for");

    // Get location from the iterable expression for error reporting
    let iter_loc = g.loc_for_expr(e.expr);

    // Emit: <obj> = <expr> (for <var> in <expr> { ... })
    let object_reg = gen_expr(g, e.expr, DataDest::Alloc);

    let iterator_reg = if let Some((iter_fct_id, iter_type_params)) = for_type_info.iter {
        // Emit: <iterator> = <obj>.iter();
        let iterator_reg = g.alloc_var(BytecodeType::Ptr);
        g.builder.emit_push_register(object_reg);
        let fct_idx = g.builder.add_const_fct_types(
            g.emitter.convert_function_id(iter_fct_id),
            g.convert_tya(&iter_type_params),
        );
        g.builder
            .emit_invoke_direct(iterator_reg, fct_idx, iter_loc);
        iterator_reg
    } else {
        // Object is already the iterator - just use it
        object_reg
    };

    let lbl_cond = g.builder.define_label();
    g.builder.emit_loop_start();

    g.enter_block_context(expr_id);

    let iterator_type = for_type_info.iterator_type.clone();
    let iterator_type_params = g.convert_tya(&iterator_type.type_params());

    g.builder.emit_push_register(iterator_reg);

    let lbl_end = g.builder.create_label();

    let value_ty = for_type_info.value_type.clone();
    let option_type_params = SourceTypeArray::single(value_ty.clone());

    // Emit: <next-temp> = <iterator>.next()
    let next_result_ty = g.emitter.convert_ty_reg(for_type_info.next_type.clone());
    let next_result_reg = g.alloc_temp(next_result_ty);

    let next_fct_id = g
        .emitter
        .convert_function_id(for_type_info.next.expect("missing fct id"));
    let fct_idx = g
        .builder
        .add_const_fct_types(next_fct_id, iterator_type_params);

    g.builder.emit_push_register(iterator_reg);
    emit_invoke_direct(
        g,
        for_type_info.next_type.clone(),
        next_result_reg,
        fct_idx,
        iter_loc,
    );

    // Emit: if <next-result>.isNone() then goto lbl_end
    let cond_reg = g.alloc_temp(BytecodeType::Bool);
    let is_none_fct_id = g
        .emitter
        .convert_function_id(g.sa.known.functions.option_is_none());
    let fct_idx = g
        .builder
        .add_const_fct_types(is_none_fct_id, g.convert_tya(&option_type_params));
    g.builder.emit_push_register(next_result_reg);
    g.builder.emit_invoke_direct(cond_reg, fct_idx, iter_loc);
    g.builder.emit_jump_if_true(cond_reg, lbl_end);
    g.free_temp(cond_reg);

    // Emit: <value-reg> = <next-result>.unwrap()
    if value_ty.is_unit() {
        g.free_temp(next_result_reg);
    } else {
        let value_ty = g.emitter.convert_ty_reg(value_ty);
        let value_reg = g.alloc_var(value_ty);
        let unwrap_fct_id = g
            .emitter
            .convert_function_id(g.sa.known.functions.option_unwrap());
        let fct_idx = g
            .builder
            .add_const_fct_types(unwrap_fct_id, g.convert_tya(&option_type_params));
        g.builder.emit_push_register(next_result_reg);
        g.builder.emit_invoke_direct(value_reg, fct_idx, iter_loc);
        g.free_temp(next_result_reg);

        setup_pattern_vars(g, e.pattern);
        destruct_pattern_or_fail(g, e.pattern, value_reg, for_type_info.value_type);
    }

    g.loops.push(LoopLabels::new(lbl_cond, lbl_end));
    let block_reg = gen_expr(g, e.block, DataDest::Alloc);
    g.free_if_temp(block_reg);
    g.loops.pop().unwrap();

    g.builder.emit_jump_loop(lbl_cond);
    g.builder.bind_label(lbl_end);

    g.leave_block_context(expr_id);
    g.pop_scope();

    g.free_if_temp(object_reg);
    g.ensure_unit_register()
}
