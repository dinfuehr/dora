use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest, var_reg};
use crate::sema::{Expr, ExprId, FieldExpr, IdentType, RefExpr, VarLocation};
use crate::ty::SourceType;

/// Produces a ref for loading a field chain rooted in a variable of type `ref T`.
pub(super) fn gen_expr_as_ref(g: &mut AstBytecodeGen, expr_id: ExprId, ty: SourceType) -> Register {
    match g.analysis.expr(expr_id) {
        Expr::Path(..) => gen_expr_as_ref_path(g, expr_id),
        Expr::Field(field_expr) => gen_expr_as_ref_field(g, expr_id, field_expr, ty),
        Expr::Paren(inner_expr) => gen_expr_as_ref(g, *inner_expr, ty),
        _ => unreachable!("expected field chain rooted in a ref variable"),
    }
}

fn gen_expr_as_ref_path(g: &mut AstBytecodeGen, expr_id: ExprId) -> Register {
    let ident_type = g
        .analysis
        .get_ident(expr_id)
        .expect("missing ident for ref-rooted field expression");
    let IdentType::Var(var_id) = ident_type else {
        unreachable!("expected ref variable")
    };

    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);
    assert!(var.ty.is_ref());
    let VarLocation::Stack = var.location else {
        unreachable!("captured ref field access isn't supported")
    };

    var_reg(g, var_id)
}

fn gen_expr_as_ref_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    field_expr: &FieldExpr,
    field_ty: SourceType,
) -> Register {
    let inner_ty = g.emitter.convert_ty(g.sa, field_ty);
    let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
    let field_idx = add_field_const_pool_entry(g, expr_id);
    let object_ty = g.ty(field_expr.lhs);

    let obj = if object_ty.is_ref() || object_ty.is_struct() || object_ty.is_tuple() {
        gen_expr_as_ref(g, field_expr.lhs, object_ty)
    } else {
        gen_expr(g, field_expr.lhs, DataDest::Alloc)
    };

    g.builder
        .emit_get_field_ref(reference, obj, field_idx, g.loc_for_expr(expr_id));
    g.free_if_temp(obj);

    reference
}

/// Returns whether a path or field chain is rooted in a variable whose type is already `ref T`.
/// Fields of such expressions are accessed through `GetFieldRef` to avoid copying the aggregate.
pub(super) fn is_rooted_in_ref(g: &AstBytecodeGen, expr_id: ExprId) -> bool {
    match g.analysis.expr(expr_id) {
        Expr::Path(..) => {
            let Some(IdentType::Var(var_id)) = g.analysis.get_ident(expr_id) else {
                return false;
            };

            g.analysis.vars().get_var(var_id).ty.is_ref()
        }
        Expr::Field(field_expr) => is_rooted_in_ref(g, field_expr.lhs),
        Expr::Paren(inner_expr) => is_rooted_in_ref(g, *inner_expr),
        _ => false,
    }
}

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
    let inner_ty = g.emitter.convert_ty(g.sa, var.ty.clone());
    let ref_ty = BytecodeType::Ref(Box::new(inner_ty));
    let dest_reg = ensure_register(g, dest, ref_ty);

    let VarLocation::Stack = var.location else {
        unimplemented!("ref on context variable");
    };

    let src_reg = var_reg(g, var_id);
    g.builder.emit_get_register_ref(dest_reg, src_reg);

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
    let inner_ty = g.emitter.convert_ty(g.sa, field_ty);
    let ref_ty = BytecodeType::Ref(Box::new(inner_ty));
    let dest_reg = ensure_register(g, dest, ref_ty);
    let field_idx = add_field_const_pool_entry(g, e.expr);
    let obj = gen_expr(g, field_expr.lhs, DataDest::Alloc);
    let location = g.loc_for_expr(e.expr);
    g.builder
        .emit_get_field_ref(dest_reg, obj, field_idx, location);
    g.free_if_temp(obj);

    dest_reg
}

fn add_field_const_pool_entry(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
) -> dora_bytecode::ConstPoolIdx {
    let ident_type = g
        .analysis
        .get_ident(expr_id)
        .expect("missing ident for ref field expression");

    match ident_type {
        IdentType::ClassField(cls_ty, field_id) => {
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
    }
}
