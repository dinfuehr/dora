use dora_bytecode::{BytecodeType, BytecodeTypeArray, Register};

use super::ensure_register;
use crate::generator::{AstBytecodeGen, DataDest, SELF_VAR_ID, last_context_register, var_reg};
use crate::sema::{ExprId, LambdaExpr};

pub(super) fn gen_expr_lambda(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &LambdaExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Ptr);

    let lambda_fct_id = g
        .analysis
        .get_lambda(expr_id)
        .expect("missing lambda id")
        .fct_id();

    let lambda_fct = g.sa.fct(lambda_fct_id);
    let lambda_analysis = lambda_fct.analysis();

    let mut outer_context_reg: Option<Register> = None;

    if lambda_analysis.needs_context_slot_in_lambda_object() {
        if let Some(context_register) = last_context_register(g) {
            g.builder.emit_push_register(context_register.clone());
        } else {
            // This lambda doesn't have a context object on its own, simply
            // pass down the parent context (the context in the lambda object).
            assert!(g.is_lambda);
            assert!(g.analysis.needs_context_slot_in_lambda_object());
            outer_context_reg = Some(g.alloc_temp(BytecodeType::Ptr));
            let lambda_cls_id = g.sa.known.classes.lambda();
            let idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(lambda_cls_id),
                BytecodeTypeArray::empty(),
                0,
            );
            g.builder.emit_load_field(
                outer_context_reg.expect("missing reg"),
                var_reg(g, SELF_VAR_ID),
                idx,
                g.loc_for_expr(expr_id),
            );
            g.builder
                .emit_push_register(outer_context_reg.expect("missing reg"));
        }
    }

    let bc_fct_id = g.emitter.convert_function_id(lambda_fct_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let idx = g.builder.add_const_fct_types(bc_fct_id, bc_type_params);
    g.builder
        .emit_new_lambda(dest, idx, g.loc_for_expr(expr_id));

    if let Some(outer_context_reg) = outer_context_reg {
        g.free_if_temp(outer_context_reg);
    }

    dest
}
