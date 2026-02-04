use std::collections::HashMap;

use dora_bytecode::{BytecodeFunction, BytecodeType, Register};

use crate::expr_block_always_returns;
use crate::program_emitter::Emitter;
use crate::sema::{AnalysisData, FctDefinition, FctDefinitionId, Sema, VarLocation};

use super::expr::gen_expr;
use super::pattern::{destruct_pattern_or_fail, setup_pattern_vars};
use super::{
    AstBytecodeGen, BytecodeBuilder, DataDest, SELF_VAR_ID, set_var_reg, store_in_context,
};

pub fn generate_fct_id(sa: &Sema, emitter: &mut Emitter, id: FctDefinitionId) -> BytecodeFunction {
    let fct = sa.fct(id);
    let analysis = fct.analysis();

    generate_fct(sa, emitter, &fct, analysis)
}

pub fn generate_fct(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    src: &AnalysisData,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        emitter,
        type_params_len: fct.type_param_definition.type_param_count(),
        is_lambda: fct.is_lambda(),
        return_type: fct.return_type(),
        file_id: fct.file_id,
        span: fct.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };
    generate_fct_impl(ast_bytecode_generator)
}

fn generate_fct_impl(mut g: AstBytecodeGen) -> BytecodeFunction {
    g.push_scope();
    create_params(&mut g);
    g.enter_function_context();
    store_params_in_context(&mut g);
    emit_function_body(&mut g);
    g.leave_function_context();
    g.pop_scope();
    g.builder.generate()
}

fn create_params(g: &mut AstBytecodeGen) {
    let mut params = Vec::new();

    if g.analysis.has_self() {
        let vars = g.analysis.vars();
        let var_self = vars.get_self();
        // The self type already includes Ref wrapper for mutating methods on value types.
        let var_ty = var_self.ty.clone();

        let bty = g.emitter.convert_ty(g.sa, var_ty.clone());
        params.push(bty);

        // For register allocation, use convert_ty_reg which converts classes to Ptr.
        let bty_reg = g.emitter.convert_ty_reg(g.sa, var_ty);
        let reg = g.alloc_var(bty_reg);
        set_var_reg(g, SELF_VAR_ID, reg);
    }

    for &param_id in g.analysis.param_pattern_ids() {
        let ty = g.ty(param_id);
        let bty = g.emitter.convert_ty(g.sa, ty.clone());
        params.push(bty);

        let bty: BytecodeType = g.emitter.convert_ty_reg(g.sa, ty);
        g.alloc_var(bty);
    }

    g.builder.set_params(params);
}

fn store_params_in_context(g: &mut AstBytecodeGen) {
    let next_register_idx = if g.analysis.has_self() {
        let vars = g.analysis.vars();
        let var_self = vars.get_self();
        let reg = Register(0);

        match var_self.location {
            VarLocation::Context(scope_id, field_id) => {
                store_in_context(g, reg, scope_id, field_id, g.loc(g.span));
            }

            VarLocation::Stack => {
                // Nothing to do.
            }
        }

        1
    } else {
        0
    };

    let param_pattern_ids = g.analysis.param_pattern_ids();

    for (param_idx, &pattern_id) in param_pattern_ids.iter().enumerate() {
        let reg = Register(next_register_idx + param_idx);
        let pattern = g.analysis.pattern(pattern_id);

        if pattern.to_ident().is_some() {
            let var_id = g.analysis.get_var_id(pattern_id).unwrap();
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);

            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    store_in_context(g, reg, scope_id, field_id, g.loc(g.span));
                }

                VarLocation::Stack => {
                    set_var_reg(g, var_id, reg);
                }
            }
        } else {
            // Get type from AST param since that's where it's stored
            let ty = g.analysis.ty(pattern_id);
            setup_pattern_vars(g, pattern_id);
            destruct_pattern_or_fail(g, pattern_id, reg, ty);
        }
    }
}

fn emit_function_body(g: &mut AstBytecodeGen) {
    let bty_return_type = g.emitter.convert_ty(g.sa, g.return_type.clone());
    g.builder.set_return_type(bty_return_type);

    let mut needs_return = true;

    let root_expr_id = g.analysis.root_expr_id();
    let block = g.analysis.expr(root_expr_id).as_block();

    for stmt_id in &block.stmts {
        g.visit_stmt(*stmt_id);
    }

    if let Some(tail_expr_id) = block.expr {
        let reg = gen_expr(g, tail_expr_id, DataDest::Alloc);

        if !expr_block_always_returns(g.analysis, block) {
            g.builder.emit_ret(reg);
        }

        needs_return = false;
        g.free_if_temp(reg);
    }

    if needs_return && g.return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder.emit_ret(dest);
    }
}
