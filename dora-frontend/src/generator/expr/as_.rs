use dora_bytecode::Register;
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{AsExpr, ExprId};

pub(super) fn gen_expr_as(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AsExpr,
    dest: DataDest,
) -> Register {
    let object_id = e.object.expect("as expression without object");
    let object_type = g.ty(object_id);

    // Get type from TypeRefId by going through SyntaxNodeId → AST → GreenId
    let type_syntax_id = g.sa.type_refs().syntax_node_id(e.ty);
    let type_ast =
        g.sa.file(g.file_id)
            .ast()
            .syntax_by_id::<ast::AstType>(type_syntax_id);
    let check_type = g.ty(type_ast.id());
    assert!(check_type.is_trait_object());

    let check_type = g.emitter.convert_ty(check_type);

    let object = gen_expr(g, object_id, DataDest::Alloc);
    let idx = g
        .builder
        .add_const_trait(check_type.clone(), g.emitter.convert_ty(object_type));
    let dest = ensure_register(g, dest, check_type);
    g.builder
        .emit_new_trait_object(dest, idx, object, g.loc_for_expr(expr_id));
    g.free_if_temp(object);
    dest
}
