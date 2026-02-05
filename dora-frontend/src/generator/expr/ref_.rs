use dora_bytecode::{BytecodeType, Register};

use super::ensure_register;
use crate::generator::{AstBytecodeGen, DataDest, var_reg};
use crate::sema::{ExprId, IdentType, RefExpr, VarLocation};

pub(super) fn gen_expr_ref(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: &RefExpr,
    dest: DataDest,
) -> Register {
    // Get the ident type for the inner expression (should be a Var based on type checking)
    let ident_type = g
        .analysis
        .get_ident(e.expr)
        .expect("missing ident for ref expression");

    let IdentType::Var(var_id) = ident_type else {
        unreachable!("ref expression should only be on variables");
    };

    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    // Get the type of the reference
    let inner_ty = g.emitter.convert_ty_reg(g.sa, var.ty.clone());
    let ref_ty = BytecodeType::Ref(Box::new(inner_ty));
    let dest_reg = ensure_register(g, dest, ref_ty);

    let VarLocation::Stack = var.location else {
        unimplemented!("ref on context variable");
    };

    let src_reg = var_reg(g, var_id);
    g.builder.emit_get_ref(dest_reg, src_reg);

    dest_reg
}
