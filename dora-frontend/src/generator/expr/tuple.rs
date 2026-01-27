use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, TupleExpr};

pub(super) fn gen_expr_tuple(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &TupleExpr,
    dest: DataDest,
) -> Register {
    if e.values.is_empty() {
        return g.ensure_unit_register();
    }

    let ty = g.ty(expr_id);

    let result_ty: BytecodeType = g.emitter.convert_ty_reg(ty.clone());
    let result = ensure_register(g, dest, result_ty);

    let mut values = Vec::with_capacity(e.values.len());

    for &value_id in &e.values {
        let value_ty = g.ty(value_id);
        let reg = gen_expr(g, value_id, DataDest::Alloc);

        if !value_ty.is_unit() {
            values.push(reg);
        }
    }

    for &value in &values {
        g.builder.emit_push_register(value);
    }

    let subtypes = ty.tuple_subtypes().expect("tuple expected");
    let bc_subtypes = g.convert_tya(&subtypes);
    let idx = g.builder.add_const_tuple(bc_subtypes);
    g.builder
        .emit_new_tuple(result, idx, g.loc_for_expr(expr_id));

    for arg_reg in values {
        g.free_if_temp(arg_reg);
    }

    result
}
