use dora_bytecode::Register;
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{AsExpr, ExprId};

pub(super) fn gen_expr_as(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    _e: &AsExpr,
    expr: ast::AstAsExpr,
    dest: DataDest,
) -> Register {
    let object_type = g.ty(expr.object().unwrap().id());
    let check_type = g.ty(expr.data_type().unwrap().id());
    assert!(check_type.is_trait_object());

    let check_type = g.emitter.convert_ty(check_type);

    let object = gen_expr(g, expr.object().unwrap(), DataDest::Alloc);
    let idx = g
        .builder
        .add_const_trait(check_type.clone(), g.emitter.convert_ty(object_type));
    let dest = ensure_register(g, dest, check_type);
    g.builder
        .emit_new_trait_object(dest, idx, object, g.loc(expr.span()));
    g.free_if_temp(object);
    dest
}
