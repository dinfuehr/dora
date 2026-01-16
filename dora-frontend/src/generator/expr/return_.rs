use dora_bytecode::Register;
use dora_parser::ast;

use super::gen_expr;
use crate::generator::{AstBytecodeGen, DataDest};

pub(super) fn gen_expr_return(
    g: &mut AstBytecodeGen,
    ret: ast::AstReturnExpr,
    _dest: DataDest,
) -> Register {
    let result_reg = if let Some(expr) = ret.expr() {
        gen_expr(g, expr, DataDest::Alloc)
    } else {
        g.ensure_unit_register()
    };

    g.builder.emit_ret(result_reg);
    g.free_if_temp(result_reg);

    g.ensure_unit_register()
}
