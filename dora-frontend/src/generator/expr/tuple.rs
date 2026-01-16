use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, TupleExpr};

pub(super) fn gen_expr_tuple(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &TupleExpr,
    node: ast::AstTupleExpr,
    dest: DataDest,
) -> Register {
    if node.values().count() == 0 {
        return g.ensure_unit_register();
    }

    let ty = g.ty(expr_id);

    let result_ty: BytecodeType = g.emitter.convert_ty_reg(ty.clone());
    let result = ensure_register(g, dest, result_ty);

    let mut values = Vec::with_capacity(node.values().count());

    for value in node.values() {
        let value_id = value.id();
        let value_ty = g.ty(value_id);
        let reg = gen_expr(g, value, DataDest::Alloc);

        if !value_ty.is_unit() {
            values.push(reg);
        }
    }

    for &value in &values {
        g.builder.emit_push_register(value);
    }

    let subtypes = ty.tuple_subtypes().expect("tuple expected");
    let idx = g.builder.add_const_tuple(g.convert_tya(&subtypes));
    g.builder.emit_new_tuple(result, idx, g.loc(node.span()));

    for arg_reg in values {
        g.free_if_temp(arg_reg);
    }

    result
}
