use std::collections::HashMap;

use dora_bytecode::{BytecodeFunction, BytecodeType, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

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
    generate_fct_impl(ast_bytecode_generator, fct.ast(sa))
}

fn generate_fct_impl(mut g: AstBytecodeGen, ast: ast::AstCallable) -> BytecodeFunction {
    g.push_scope();
    create_params(&mut g, ast.clone());
    g.enter_function_context();
    store_params_in_context(&mut g, ast.clone());
    emit_function_body(&mut g, ast);
    g.leave_function_context();
    g.pop_scope();
    g.builder.generate()
}

fn create_params(g: &mut AstBytecodeGen, ast: ast::AstCallable) {
    let mut params = Vec::new();

    if g.analysis.has_self() {
        let var_self = g.analysis.vars.get_self();
        let var_ty = var_self.ty.clone();

        let bty = g.emitter.convert_ty(var_ty.clone());
        params.push(bty);

        g.allocate_register_for_var(SELF_VAR_ID);
    }

    for param in ast.params() {
        let param_id = param.id();
        let ty = g.ty(param_id);
        let bty = g.emitter.convert_ty(ty.clone());
        params.push(bty);

        let bty: BytecodeType = g.emitter.convert_ty_reg(ty);
        g.alloc_var(bty);
    }

    g.builder.set_params(params);
}

fn store_params_in_context(g: &mut AstBytecodeGen, ast: ast::AstCallable) {
    let next_register_idx = if g.analysis.has_self() {
        let var_self = g.analysis.vars.get_self();
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

    for (param_idx, param) in ast.params().enumerate() {
        let param_id = param.id();
        let reg = Register(next_register_idx + param_idx);

        if param.pattern().is_none() {
            continue;
        }

        let pattern = param.pattern().unwrap();

        if pattern.is_ident_pattern() {
            let var_id = *g.analysis.map_vars.get(pattern.id()).unwrap();
            let var = g.analysis.vars.get_var(var_id);

            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    store_in_context(g, reg, scope_id, field_id, g.loc(g.span));
                }

                VarLocation::Stack => {
                    set_var_reg(g, var_id, reg);
                }
            }
        } else {
            let ty = g.analysis.ty(param_id);
            setup_pattern_vars(g, pattern.clone());
            destruct_pattern_or_fail(g, pattern, reg, ty);
        }
    }
}

fn emit_function_body(g: &mut AstBytecodeGen, ast: ast::AstCallable) {
    let bty_return_type = g.emitter.convert_ty(g.return_type.clone());
    g.builder.set_return_type(bty_return_type);

    let mut needs_return = true;

    let block = ast.block().expect("missing block");

    for stmt in block.stmts_without_tail() {
        g.visit_stmt(stmt);
    }

    if let Some(stmt) = block.tail() {
        let expr_stmt = stmt.as_expr_stmt();
        let reg = gen_expr(g, expr_stmt.expr(), DataDest::Alloc);

        if !expr_block_always_returns(&g.sa.file(g.file_id).ast(), block) {
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
