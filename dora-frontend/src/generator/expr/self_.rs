use dora_bytecode::Register;
use dora_parser::ast::{self, SyntaxNodeBase};

use super::path::gen_expr_path_context;
use crate::generator::{AstBytecodeGen, DataDest, SELF_VAR_ID, emit_mov, var_reg};
use crate::sema::IdentType;

pub(super) fn gen_expr_self(
    g: &mut AstBytecodeGen,
    expr: ast::AstThisExpr,
    dest: DataDest,
) -> Register {
    let expr_id = expr.id();
    if g.is_lambda {
        let ident = g.analysis.get_ident(expr_id).expect("missing ident");
        let (level, context_idx) = match ident {
            IdentType::Context(level, context_idx) => (level, context_idx),
            _ => unreachable!(),
        };
        gen_expr_path_context(g, level, context_idx, dest, g.loc(expr.span()))
    } else {
        let var_reg = var_reg(g, SELF_VAR_ID);

        if dest.is_alloc() {
            return var_reg;
        }

        let dest = dest.reg();

        emit_mov(g, dest, var_reg);

        dest
    }
}
