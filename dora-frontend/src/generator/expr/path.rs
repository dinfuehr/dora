use dora_bytecode::{BytecodeType, BytecodeTypeArray, Location, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{emit_new_enum, ensure_register, load_from_context};
use crate::generator::{AstBytecodeGen, DataDest, SELF_VAR_ID, field_id_from_context_idx, var_reg};
use crate::sema::{
    ConstDefinitionId, ContextFieldId, GlobalDefinitionId, IdentType, OuterContextIdx, VarId,
    VarLocation,
};

pub(super) fn gen_expr_path(
    g: &mut AstBytecodeGen,
    ident: ast::AstPathExpr,
    dest: DataDest,
) -> Register {
    let ast_id = ident.id();
    let ident_type = g.analysis.get_ident(ast_id).expect("missing ident");

    match ident_type {
        IdentType::Var(var_id) => gen_expr_path_var(g, var_id, dest, g.loc(ident.span())),
        IdentType::Context(level, field_id) => {
            gen_expr_path_context(g, level, field_id, dest, g.loc(ident.span()))
        }
        IdentType::Global(gid) => gen_expr_path_global(g, gid, dest, g.loc(ident.span())),
        IdentType::Const(cid) => gen_expr_path_const(g, cid, dest),
        IdentType::EnumVariant(enum_id, type_params, variant_idx) => emit_new_enum(
            g,
            enum_id,
            type_params,
            variant_idx,
            g.loc(ident.span()),
            dest,
        ),

        IdentType::Field(..) => unreachable!(),
        IdentType::Struct(..) => unreachable!(),
        IdentType::StructField(..) => unreachable!(),

        IdentType::Fct(..) => unreachable!(),
        IdentType::Class(..) => unreachable!(),
    }
}

pub(super) fn gen_expr_path_context(
    g: &mut AstBytecodeGen,
    context_id: OuterContextIdx,
    field_id: ContextFieldId,
    dest: DataDest,
    location: Location,
) -> Register {
    assert!(g.is_lambda);
    let self_reg = var_reg(g, SELF_VAR_ID);

    // Load context field of lambda object (in self register).
    let outer_context_reg = g.alloc_temp(BytecodeType::Ptr);
    let lambda_cls_id = g.sa.known.classes.lambda();
    let idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(lambda_cls_id),
        BytecodeTypeArray::empty(),
        0,
    );
    g.builder
        .emit_load_field(outer_context_reg, self_reg, idx, location);

    let outer_contexts = g.analysis.outer_contexts();
    assert!(context_id.0 < outer_contexts.len());

    for outer_context_class in outer_contexts.iter().skip(context_id.0 + 1).rev() {
        if outer_context_class.has_class_id() {
            let outer_cls_id = outer_context_class.class_id();
            let idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(outer_cls_id),
                g.convert_tya(&g.identity_type_params()),
                0,
            );
            assert!(outer_context_class.has_parent_slot());
            g.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
        }
    }

    let outer_context_info = outer_contexts[context_id.0].clone();
    let outer_cls_id = outer_context_info.class_id();

    let outer_cls = g.sa.class(outer_cls_id);
    let field_index = field_id_from_context_idx(field_id, outer_context_info.has_parent_slot());
    let field_id = outer_cls.field_id(field_index);
    let field = g.sa.field(field_id);

    let ty: BytecodeType = g.emitter.convert_ty_reg(field.ty());
    let value_reg = ensure_register(g, dest, ty);

    let idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(outer_cls_id),
        g.convert_tya(&g.identity_type_params()),
        field_index.0 as u32,
    );
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

    let bytecode_ty = g.emitter.convert_ty_reg(ty.clone());
    let dest = ensure_register(g, dest, bytecode_ty);

    let const_id = g.emitter.convert_const_id(const_id);
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

    let ty: BytecodeType = g.emitter.convert_ty_reg(global_var.ty());
    let dest = ensure_register(g, dest, ty);

    g.builder
        .emit_load_global(dest, g.emitter.convert_global_id(gid), location);

    dest
}

fn gen_expr_path_var(
    g: &mut AstBytecodeGen,
    var_id: VarId,
    dest: DataDest,
    location: Location,
) -> Register {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    match var.location {
        VarLocation::Context(scope_id, field_idx) => {
            let ty = g.emitter.convert_ty_reg(var.ty.clone());
            let dest_reg = ensure_register(g, dest, ty);
            load_from_context(g, dest_reg, scope_id, field_idx, location);
            dest_reg
        }

        VarLocation::Stack => {
            let var_reg = var_reg(g, var_id);

            if dest.is_alloc() {
                return var_reg;
            }

            let dest = dest.reg();
            g.builder.emit_mov(dest, var_reg);
            dest
        }
    }
}
