use dora_bytecode::{BytecodeType, Location, Register};

use super::ensure_register;
use crate::generator::{
    AstBytecodeGen, DataDest, field_id_from_context_idx, load_outer_context_object, var_reg,
};
use crate::sema::{
    ConstDefinitionId, ContextFieldId, ContextId, EnumDefinitionId, ExprId, GlobalDefinitionId,
    IdentType, ScopeId, VarId, VarLocation,
};
use crate::ty::SourceTypeArray;

pub(super) fn gen_expr_path(g: &mut AstBytecodeGen, expr_id: ExprId, dest: DataDest) -> Register {
    let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");
    let location = g.loc_for_expr(expr_id);

    match ident_type {
        IdentType::Var(var_id) => gen_expr_path_var(g, expr_id, var_id, dest, location),
        IdentType::Context(level, field_id) => {
            gen_expr_path_context(g, level, field_id, dest, location)
        }
        IdentType::Global(gid) => gen_expr_path_global(g, gid, dest, location),
        IdentType::Const(cid) => gen_expr_path_const(g, cid, dest),
        IdentType::EnumVariant(enum_id, type_params, variant_idx) => {
            emit_new_enum(g, enum_id, type_params, variant_idx, location, dest)
        }

        IdentType::ClassField(..) => unreachable!(),
        IdentType::Struct(..) => unreachable!(),
        IdentType::StructField(..) => unreachable!(),
        IdentType::TupleField(..) => unreachable!(),

        IdentType::Fct(..) => unreachable!(),
        IdentType::Class(..) => unreachable!(),
    }
}

pub(super) fn gen_expr_path_context(
    g: &mut AstBytecodeGen,
    context_id: ContextId,
    field_id: ContextFieldId,
    dest: DataDest,
    location: Location,
) -> Register {
    assert!(g.is_lambda);
    let outer_context_reg = load_outer_context_object(g, context_id, location);
    let outer_context = g.sa.context(context_id);
    let outer_cls_id = outer_context.class_id();
    let has_parent_slot = outer_context.has_parent_slot();

    let outer_cls = g.sa.class(outer_cls_id);
    let field_index = field_id_from_context_idx(field_id, has_parent_slot);
    let field_id = outer_cls.field_id(field_index);
    let field = g.sa.field(field_id);

    let ty: BytecodeType = g.emitter.convert_ty(g.sa, field.ty());
    let value_reg = ensure_register(g, dest, ty);

    let bc_cls_id = g.emitter.convert_class_id(g.sa, outer_cls_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let idx = g
        .builder
        .add_const_field_types(bc_cls_id, bc_type_params, field_index.0 as u32);
    g.builder
        .emit_load_field(value_reg, outer_context_reg, idx, location);

    g.free_temp(outer_context_reg);

    value_reg
}

fn gen_expr_path_const(
    g: &mut AstBytecodeGen,
    const_id: ConstDefinitionId,
    dest: DataDest,
) -> Register {
    let const_ = g.sa.const_(const_id);
    let ty = const_.ty();

    let bytecode_ty = g.emitter.convert_ty(g.sa, ty.clone());
    let dest = ensure_register(g, dest, bytecode_ty);

    let const_id = g.emitter.convert_const_id(g.sa, const_id);
    g.builder.emit_load_const(dest, const_id);

    dest
}

fn gen_expr_path_global(
    g: &mut AstBytecodeGen,
    gid: GlobalDefinitionId,
    dest: DataDest,
    location: Location,
) -> Register {
    let global_var = g.sa.global(gid);

    let ty: BytecodeType = g.emitter.convert_ty(g.sa, global_var.ty());
    let dest = ensure_register(g, dest, ty);

    g.builder
        .emit_load_global(dest, g.emitter.convert_global_id(g.sa, gid), location);

    dest
}

fn gen_expr_path_var(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    var_id: VarId,
    dest: DataDest,
    location: Location,
) -> Register {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);
    let expr_ty = g.ty(expr_id);
    let auto_deref = var.ty.is_ref() && !expr_ty.is_ref();

    match var.location {
        VarLocation::Context(scope_id, field_idx) => {
            assert!(!auto_deref);
            let ty = g.emitter.convert_ty(g.sa, var.ty.clone());
            let dest_reg = ensure_register(g, dest, ty);
            load_from_context(g, dest_reg, scope_id, field_idx, location);
            dest_reg
        }

        VarLocation::Stack => {
            let var_reg = var_reg(g, var_id);

            if auto_deref {
                let value_ty = g.emitter.convert_ty(g.sa, expr_ty);
                let dest_reg = ensure_register(g, dest, value_ty);
                g.builder.emit_load_ref(dest_reg, var_reg);
                return dest_reg;
            }

            if dest.is_alloc() {
                return var_reg;
            }

            let dest = dest.reg();
            g.builder.emit_mov(dest, var_reg);
            dest
        }
    }
}

pub(super) fn emit_new_enum(
    g: &mut AstBytecodeGen,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
    location: Location,
    dest: DataDest,
) -> Register {
    let type_params = g.convert_tya(&type_params);
    let enum_id = g.emitter.convert_enum_id(g.sa, enum_id);
    let bty = BytecodeType::Enum(enum_id, type_params.clone());
    let dest = ensure_register(g, dest, bty);
    let idx = g
        .builder
        .add_const_enum_variant(enum_id, type_params, variant_idx);
    g.builder.emit_new_enum(dest, idx, &[], location);
    dest
}

pub(super) fn load_from_context(
    g: &mut AstBytecodeGen,
    dest: Register,
    scope_id: ScopeId,
    field_id: ContextFieldId,
    location: Location,
) {
    let entered_context = &g.entered_contexts[scope_id.0];
    let context_register = entered_context.register.expect("missing register");
    let context = g.sa.context(entered_context.context_id);
    let cls_id = context.class_id();
    let field_id = field_id_from_context_idx(field_id, context.has_parent_slot());
    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let field_idx = g
        .builder
        .add_const_field_types(bc_cls_id, bc_type_params, field_id.0 as u32);
    g.builder
        .emit_load_field(dest, context_register, field_idx, location);
}
