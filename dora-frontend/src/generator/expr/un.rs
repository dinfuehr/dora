use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast;

use super::call::emit_intrinsic_un_id;
use super::lit::gen_expr_lit_int;
use super::{
    add_const_pool_entry_for_call, emit_invoke_direct, emit_invoke_generic_direct, ensure_register,
    gen_expr, specialize_type_for_call,
};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{Expr, ExprId, UnExpr};
use crate::ty::SourceType;

pub(super) fn gen_expr_un(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &UnExpr,
    dest: DataDest,
) -> Register {
    // Check if this is negation of an integer literal
    let opnd_expr = g.analysis.expr(e.expr);
    if e.op == ast::UnOp::Neg {
        if let Expr::LitInt(_) = opnd_expr {
            return gen_expr_lit_int(g, e.expr, dest);
        }
    }

    if let Some(intrinsic) = g.get_intrinsic(expr_id) {
        let loc = g.loc_for_expr(expr_id);
        emit_intrinsic_un_id(g, e.expr, intrinsic, loc, dest)
    } else {
        gen_expr_un_method(g, expr_id, e, dest)
    }
}

fn gen_expr_un_method(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &UnExpr,
    dest: DataDest,
) -> Register {
    let opnd = gen_expr(g, e.expr, DataDest::Alloc);

    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, call_type.as_ref());

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type.as_ref(), callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(g.sa, function_return_type.clone());
    let dest = ensure_register(g, dest, function_return_type_bc);

    g.builder.emit_push_register(opnd);

    let loc = g.loc_for_expr(expr_id);
    if call_type.is_generic_method() {
        emit_invoke_generic_direct(g, function_return_type, dest, callee_idx, loc);
    } else {
        emit_invoke_direct(g, function_return_type, dest, callee_idx, loc);
    }

    g.free_if_temp(opnd);

    dest
}
