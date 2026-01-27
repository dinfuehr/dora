use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{AsExpr, ExprId};
use dora_bytecode::Register;

pub(super) fn gen_expr_as(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AsExpr,
    dest: DataDest,
) -> Register {
    let object_id = e.object.expect("as expression without object");
    let object_type = g.ty(object_id);

    let check_type = g.ty(e.ty);
    assert!(check_type.is_trait_object());

    let check_type = g.emitter.convert_ty(g.sa, check_type);

    let object = gen_expr(g, object_id, DataDest::Alloc);
    let idx = g
        .builder
        .add_const_trait(check_type.clone(), g.emitter.convert_ty(g.sa, object_type));
    let dest = ensure_register(g, dest, check_type);
    g.builder
        .emit_new_trait_object(dest, idx, object, g.loc_for_expr(expr_id));
    g.free_if_temp(object);
    dest
}
