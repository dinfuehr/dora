use dora_bytecode::Register;

use super::ensure_register;
use crate::generator::{
    AstBytecodeGen, DataDest, last_context_register, load_outer_context_object,
};
use crate::sema::{ExprId, LambdaExpr, OuterContextIdx};

pub(super) fn gen_expr_lambda(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &LambdaExpr,
    dest: DataDest,
) -> Register {
    let ty = g.ty(expr_id);
    let ty = g.emitter.convert_ty(g.sa, ty);
    let dest = ensure_register(g, dest, ty);

    let lambda_fct_id = g
        .analysis
        .get_lambda(expr_id)
        .expect("missing lambda id")
        .fct_id();

    let lambda_fct = g.sa.fct(lambda_fct_id);
    let lambda_analysis = lambda_fct.analysis();

    let mut arguments: Vec<Register> = Vec::new();
    let mut outer_context_reg: Option<Register> = None;

    if lambda_analysis.needs_context_slot_in_lambda_object() {
        if let Some(context_register) = last_context_register(g) {
            arguments.push(context_register.clone());
        } else {
            // This lambda doesn't have a context object on its own, simply
            // pass down the parent context (the context in the lambda object).
            assert!(g.is_lambda);
            assert!(g.analysis.needs_context_slot_in_lambda_object());
            let outer_contexts = g.analysis.outer_contexts();
            let context_id = outer_contexts
                .iter()
                .rposition(|context| context.has_class_id())
                .expect("missing outer context");
            drop(outer_contexts);
            outer_context_reg = Some(load_outer_context_object(
                g,
                OuterContextIdx(context_id),
                g.loc_for_expr(expr_id),
            ));
            arguments.push(outer_context_reg.expect("missing reg"));
        }
    }

    let bc_fct_id = g.emitter.convert_function_id(g.sa, lambda_fct_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let idx = g.builder.add_const_fct_types(bc_fct_id, bc_type_params);
    g.builder
        .emit_new_lambda(dest, idx, &arguments, g.loc_for_expr(expr_id));

    if let Some(outer_context_reg) = outer_context_reg {
        g.free_if_temp(outer_context_reg);
    }

    dest
}
