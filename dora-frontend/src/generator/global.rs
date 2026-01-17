use std::collections::HashMap;

use crate::program_emitter::Emitter;
use crate::sema::{AnalysisData, ExprId, GlobalDefinition, Sema};
use dora_bytecode::BytecodeFunction;
use dora_parser::ast::SyntaxNodeBase;

use super::expr::gen_expr;
use super::{AstBytecodeGen, BytecodeBuilder, DataDest};

pub fn generate_global_initializer(
    sa: &Sema,
    emitter: &mut Emitter,
    global: &GlobalDefinition,
    src: &AnalysisData,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        emitter,
        type_params_len: 0,
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

    let initial_expr = global.ast(sa).initial_value().expect("missing initializer");
    let expr_id = src.exprs().to_expr_id(initial_expr.id());
    generate_global_initializer_impl(ast_bytecode_generator, expr_id)
}

fn generate_global_initializer_impl(mut g: AstBytecodeGen, expr_id: ExprId) -> BytecodeFunction {
    g.push_scope();
    g.builder.set_params(Vec::new());
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
