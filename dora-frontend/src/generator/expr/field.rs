use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, FieldExpr, IdentType, StructDefinitionId};
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &FieldExpr,
    dest: DataDest,
) -> Register {
    let object_ty = g.ty(e.lhs);

    // Auto-dereference Ref types for field access.
    let object_ty = match object_ty {
        SourceType::Ref(inner) => (*inner).clone(),
        ty => ty,
    };

    if object_ty.is_tuple() {
        return gen_expr_field_tuple(g, expr_id, e, dest);
    }

    if let Some((struct_id, type_params)) = object_ty.to_struct() {
        return gen_expr_field_struct(g, expr_id, e, struct_id, type_params, dest);
    }

    let (cls_ty, field_id) = {
        let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");

        match ident_type {
            IdentType::Field(ty, field) => (ty.clone(), field),
            _ => unreachable!(),
        }
    };

    let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&type_params);
    let field_idx = g
        .builder
        .add_const_field_types(bc_cls_id, bc_type_params, field_id.0 as u32);

    let field_ty = g.ty(expr_id);

    let field_bc_ty: BytecodeType = g.emitter.convert_ty_reg(g.sa, field_ty);
    let dest = ensure_register(g, dest, field_bc_ty);
    let obj = gen_expr(g, e.lhs, DataDest::Alloc);

    g.builder
        .emit_load_field(dest, obj, field_idx, g.loc_for_expr(expr_id));
    g.free_if_temp(obj);

    dest
}

fn gen_expr_field_struct(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &FieldExpr,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
    dest: DataDest,
) -> Register {
    let struct_obj = gen_expr(g, e.lhs, DataDest::Alloc);

    let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");

    let field_idx = match ident_type {
        IdentType::StructField(_, field_idx) => field_idx,
        _ => unreachable!(),
    };

    let fty = g.ty(expr_id);

    if fty.is_unit() {
        g.free_if_temp(struct_obj);
        return g.ensure_unit_register();
    }

    let ty: BytecodeType = g.emitter.convert_ty_reg(g.sa, fty);
    let dest = ensure_register(g, dest, ty);
    let bc_struct_id = g.emitter.convert_struct_id(g.sa, struct_id);
    let bc_type_params = g.convert_tya(&type_params);
    let const_idx =
        g.builder
            .add_const_struct_field(bc_struct_id, bc_type_params, field_idx.0 as u32);
    g.builder
        .emit_load_field(dest, struct_obj, const_idx, g.loc_for_expr(expr_id));

    g.free_if_temp(struct_obj);

    dest
}

fn gen_expr_field_tuple(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &FieldExpr,
    dest: DataDest,
) -> Register {
    let tuple = gen_expr(g, e.lhs, DataDest::Alloc);

    let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");
    let IdentType::TupleField(tuple_ty, idx) = ident_type else {
        unreachable!()
    };

    let subtypes: SourceTypeArray = tuple_ty.tuple_subtypes().expect("tuple expected");
    let ty = subtypes[idx as usize].clone();

    let ty: BytecodeType = g.emitter.convert_ty_reg(g.sa, ty);
    let dest = ensure_register(g, dest, ty);
    let field_idx = g
        .builder
        .add_const_tuple_element(g.emitter.convert_ty(g.sa, tuple_ty), idx);
    g.builder
        .emit_load_field(dest, tuple, field_idx, g.loc_for_expr(expr_id));

    g.free_if_temp(tuple);

    dest
}
