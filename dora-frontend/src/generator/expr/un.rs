use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{
    add_const_pool_entry_for_call, emit_intrinsic_un, emit_invoke_direct,
    emit_invoke_generic_direct, ensure_register, gen_expr, gen_expr_lit_int,
    specialize_type_for_call,
};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::ty::SourceType;

pub(super) fn gen_expr_un(
    g: &mut AstBytecodeGen,
    node: ast::AstUnExpr,
    dest: DataDest,
) -> Register {
    let node_id = node.id();
    let opnd = node.opnd();
    if node.op() == ast::UnOp::Neg && opnd.is_lit_int_expr() {
        gen_expr_lit_int(g, opnd.as_lit_int_expr(), dest, true)
    } else if let Some(intrinsic) = g.get_intrinsic(node_id) {
        emit_intrinsic_un(g, opnd, intrinsic, g.loc(node.span()), dest)
    } else {
        gen_expr_un_method(g, node, dest)
    }
}

fn gen_expr_un_method(g: &mut AstBytecodeGen, node: ast::AstUnExpr, dest: DataDest) -> Register {
    let node_id = node.id();
    let opnd = gen_expr(g, node.opnd(), DataDest::Alloc);

    let call_type = g.analysis.get_call_type(node_id).expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, call_type.as_ref());

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type.as_ref(), callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(function_return_type.clone());
    let dest = ensure_register(g, dest, function_return_type_bc);

    g.builder.emit_push_register(opnd);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            g.loc(node.span()),
        );
    } else {
        emit_invoke_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            g.loc(node.span()),
        );
    }

    g.free_if_temp(opnd);

    dest
}
