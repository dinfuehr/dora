use dora_bytecode::Register;

use super::gen_expr;
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, ReturnExpr};

pub(super) fn gen_expr_return(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: &ReturnExpr,
    _dest: DataDest,
) -> Register {
    let result_reg = if let Some(expr_id) = e.expr {
        gen_expr(g, expr_id, DataDest::Alloc)
    } else {
        g.ensure_unit_register()
    };

    g.builder.emit_ret(result_reg);
    g.free_if_temp(result_reg);

    g.ensure_unit_register()
}
