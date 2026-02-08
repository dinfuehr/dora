use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest, var_reg};
use crate::sema::{Expr, ExprId, FieldExpr, IdentType, RefExpr, VarLocation};

pub(super) fn gen_expr_ref(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &RefExpr,
    dest: DataDest,
) -> Register {
    let inner_expr = g.analysis.expr(e.expr);

    match inner_expr {
        Expr::Path(..) => gen_expr_ref_var(g, expr_id, e, dest),
        Expr::Field(field_expr) => gen_expr_ref_field(g, expr_id, e, field_expr, dest),
        _ => unreachable!("ref expression should only be on variables or fields"),
    }
}

fn gen_expr_ref_var(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: &RefExpr,
    dest: DataDest,
) -> Register {
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

fn gen_expr_ref_field(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: &RefExpr,
    field_expr: &FieldExpr,
    dest: DataDest,
) -> Register {
    let field_ty = g.ty(e.expr);
    let inner_ty = g.emitter.convert_ty_reg(g.sa, field_ty);
    let ref_ty = BytecodeType::Ref(Box::new(inner_ty));
    let dest_reg = ensure_register(g, dest, ref_ty);

    let ident_type = g
        .analysis
        .get_ident(e.expr)
        .expect("missing ident for ref field expression");

    let field_idx = match ident_type {
        IdentType::Field(cls_ty, field_id) => {
            let (cls_id, type_params) = cls_ty.to_class().expect("class expected");
            let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
            let bc_type_params = g.convert_tya(&type_params);
            g.builder
                .add_const_field_types(bc_cls_id, bc_type_params, field_id.0 as u32)
        }

        IdentType::StructField(struct_ty, field_id) => {
            let (struct_id, type_params) = struct_ty.to_struct().expect("struct expected");
            let bc_struct_id = g.emitter.convert_struct_id(g.sa, struct_id);
            let bc_type_params = g.convert_tya(&type_params);
            g.builder
                .add_const_struct_field(bc_struct_id, bc_type_params, field_id.0 as u32)
        }

        IdentType::TupleField(tuple_ty, idx) => g
            .builder
            .add_const_tuple_element(g.emitter.convert_ty(g.sa, tuple_ty.clone()), idx),

        _ => unreachable!("unexpected ident type for ref field expression"),
    };

    let obj = gen_expr(g, field_expr.lhs, DataDest::Alloc);
    let location = g.loc_for_expr(e.expr);
    g.builder
        .emit_get_field_ref(dest_reg, obj, field_idx, location);
    g.free_if_temp(obj);

    dest_reg
}
