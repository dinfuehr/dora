use dora_bytecode::{BytecodeType, Register};

use super::ensure_register;
use crate::generator::{
    AstBytecodeGen, DataDest, enclosing_context_class, last_context_register,
    load_outer_context_object,
};
use crate::sema::{Element, ExprId, LambdaExpr};
use crate::ty::SourceType;

pub(super) fn gen_expr_lambda(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &LambdaExpr,
    dest: DataDest,
) -> Register {
    let source_ty = g.ty(expr_id);
    let trait_object_ty = g.emitter.convert_ty(g.sa, source_ty);
    let dest = ensure_register(g, dest, trait_object_ty.clone());

    let lambda_id = g
        .analysis
        .get_lambda_id(expr_id)
        .expect("missing lambda id");
    let lambda_fct_id = g.sa.lambda_fct_id(lambda_id);

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
            let function_context_id = g.analysis.function_context_id();
            let context_id = enclosing_context_class(g.sa, function_context_id);
            outer_context_reg = Some(load_outer_context_object(
                g,
                context_id,
                g.loc_for_expr(expr_id),
            ));
            arguments.push(outer_context_reg.expect("missing reg"));
        }
    }

    let env_struct_id = g.sa.lambda_env_struct_id(lambda_id);
    let env_struct = g.sa.struct_(env_struct_id);
    let env_type_params = g.identity_type_params();
    assert_eq!(
        env_struct.type_param_definition(g.sa).type_param_count(),
        env_type_params.len()
    );
    let env_ty = g
        .emitter
        .convert_ty(g.sa, SourceType::Struct(env_struct_id, env_type_params));
    let BytecodeType::Struct(bc_env_struct_id, bc_type_params) = env_ty.clone() else {
        unreachable!()
    };
    let env_reg = g.alloc_temp(env_ty.clone());
    let env_idx = g.builder.add_const_struct(bc_env_struct_id, bc_type_params);
    g.builder
        .emit_new_struct(env_reg, env_idx, &arguments, g.loc_for_expr(expr_id));

    let trait_idx = g.builder.add_const_trait(trait_object_ty, env_ty);
    g.builder
        .emit_new_trait_object(dest, trait_idx, env_reg, g.loc_for_expr(expr_id));
    g.free_temp(env_reg);

    if let Some(outer_context_reg) = outer_context_reg {
        g.free_if_temp(outer_context_reg);
    }

    dest
}
