use std::collections::HashMap;

use crate::program_emitter::Emitter;
use crate::sema::{AnalysisData, ExprId, GlobalDefinition, Sema};
use dora_bytecode::BytecodeBody;

use super::expr::gen_expr;
use super::{AstBytecodeGen, BytecodeBuilder, DataDest};

pub fn generate_global_initializer(
    sa: &Sema,
    emitter: &mut Emitter,
    global: &GlobalDefinition,
    src: &AnalysisData,
) -> BytecodeBody {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        emitter,
        frontend_type_params_len: 0,
        type_params_len: 0,
        type_param_definition_id: global.type_param_definition_id,
        is_lambda: false,
        return_type: global.ty(),
        file_id: global.file_id,
        span: global.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };

    let expr_id = src.root_expr_id();
    generate_global_initializer_impl(ast_bytecode_generator, expr_id)
}

fn generate_global_initializer_impl(mut g: AstBytecodeGen, expr_id: ExprId) -> BytecodeBody {
    g.push_scope();
    g.enter_function_context();
    emit_global_initializer(&mut g, expr_id);
    g.leave_function_context();
    g.pop_scope();
    g.builder.generate()
}

fn emit_global_initializer(g: &mut AstBytecodeGen, expr_id: ExprId) {
    let result = gen_expr(g, expr_id, DataDest::Alloc);
    g.builder.emit_ret(result);
    g.free_if_temp(result);
}
