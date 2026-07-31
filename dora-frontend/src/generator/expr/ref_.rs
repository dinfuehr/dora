use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{
    AstBytecodeGen, DataDest, field_id_from_context_idx, load_outer_context_object, var_reg,
};
use crate::sema::{
    ContextFieldId, ContextId, Expr, ExprId, FieldExpr, IdentType, RefExpr, ScopeId, VarLocation,
};
use crate::ty::SourceType;

pub(super) struct GeneratedRef {
    pub reference: Register,
    pub backing: Option<Register>,
}

impl AstBytecodeGen<'_> {
    pub(super) fn free_generated_ref(&mut self, generated_ref: GeneratedRef) {
        self.free_if_temp(generated_ref.reference);

        if let Some(backing) = generated_ref.backing {
            self.free_if_temp(backing);
        }
    }
}

/// Produces a ref to an expression for mutating method receivers and ref-backed field access.
/// Addressable paths and fields refer directly to their storage; other expressions use a
/// temporary value that is kept alive in `GeneratedRef::backing`.
pub(super) fn gen_expr_as_ref(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    ty: SourceType,
) -> GeneratedRef {
    match g.analysis.expr(expr_id) {
        Expr::Path(..) => gen_expr_as_ref_path(g, expr_id),
        Expr::Field(field_expr) => gen_expr_as_ref_field(g, expr_id, field_expr, ty),
        Expr::Paren(inner_expr) => gen_expr_as_ref(g, *inner_expr, ty),
        // `(ref value).field` already has a reference to use as the chain root.
        Expr::Ref(_) => GeneratedRef {
            reference: gen_expr(g, expr_id, DataDest::Alloc),
            backing: None,
        },
        _ => {
            let value = gen_expr(g, expr_id, DataDest::Alloc);
            let inner_ty = g.emitter.convert_ty(g.sa, ty);
            let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
            g.builder.emit_get_register_ref(reference, value);

            GeneratedRef {
                reference,
                backing: Some(value),
            }
        }
    }
}

enum FieldRefBase {
    Value(ExprId),
    Ref(ExprId),
}

pub(super) struct FieldRefPath {
    base: FieldRefBase,
    /// Field expressions ordered from the base towards the final field.
    fields: Vec<ExprId>,
}

/// Returns the aggregate field segment that should be loaded through references. A single field
/// on an ordinary value returns `None` and remains a `LoadField`.
pub(super) fn field_ref_path(g: &AstBytecodeGen, expr_id: ExprId) -> Option<FieldRefPath> {
    let mut current_expr = expr_id;
    let mut fields = Vec::new();

    // Parentheses intentionally end this field segment and are generated as part of its base.
    let (base_expr, base_needs_ref) = loop {
        match g.analysis.expr(current_expr) {
            Expr::Field(field_expr) => {
                fields.push(current_expr);

                let object_ty = g.ty(field_expr.lhs);
                if !object_ty.is_ref() && !object_ty.is_struct() && !object_ty.is_tuple() {
                    break (field_expr.lhs, false);
                }

                current_expr = field_expr.lhs;
            }
            _ => break (current_expr, true),
        }
    };

    let (base_is_ref, base_is_addressable) = ref_base_info(g, base_expr);
    if fields.len() == 1 && !base_is_ref {
        return None;
    }

    let base = if base_needs_ref {
        if !base_is_addressable {
            return None;
        }

        FieldRefBase::Ref(base_expr)
    } else {
        FieldRefBase::Value(base_expr)
    };

    fields.reverse();
    Some(FieldRefPath { base, fields })
}

/// Returns whether the base is already a `ref` and whether it can safely be addressed.
fn ref_base_info(g: &AstBytecodeGen, expr_id: ExprId) -> (bool, bool) {
    match g.analysis.expr(expr_id) {
        Expr::Path(..) => match g.analysis.get_ident(expr_id) {
            Some(IdentType::Var(var_id)) => {
                let vars = g.analysis.vars();
                let var = vars.get_var(var_id);

                (var.ty.is_ref(), matches!(var.location, VarLocation::Stack))
            }
            Some(IdentType::Global(_)) => (false, true),
            _ => (false, false),
        },
        Expr::Ref(_) => (true, true),
        // The parenthesis split the field path, but do not change the base's storage properties.
        Expr::Paren(inner_expr) => ref_base_info(g, *inner_expr),
        _ => (false, false),
    }
}

pub(super) fn gen_expr_field_via_ref(
    g: &mut AstBytecodeGen,
    path: FieldRefPath,
    dest: DataDest,
) -> Register {
    let expr_id = *path.fields.last().expect("field path is empty");
    let field_ty = g.ty(expr_id);

    if field_ty.is_unit() {
        return g.ensure_unit_register();
    }

    let bytecode_ty = g.emitter.convert_ty(g.sa, field_ty.clone());
    let dest = ensure_register(g, dest, bytecode_ty);

    let (mut obj, backing) = match path.base {
        FieldRefBase::Ref(base) => {
            let base_ty = g.ty(base);
            let generated_ref = gen_expr_as_ref(g, base, base_ty);
            (generated_ref.reference, generated_ref.backing)
        }
        FieldRefBase::Value(base) => (gen_expr(g, base, DataDest::Alloc), None),
    };

    for &field_expr_id in &path.fields {
        let field_ty = g.ty(field_expr_id);
        let inner_ty = g.emitter.convert_ty(g.sa, field_ty);
        let next = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
        let field_idx = add_field_const_pool_entry(g, field_expr_id);
        g.builder
            .emit_get_field_ref(next, obj, field_idx, g.loc_for_expr(field_expr_id));
        g.free_if_temp(obj);
        obj = next;
    }

    g.builder.emit_load_ref(dest, obj);
    g.free_generated_ref(GeneratedRef {
        reference: obj,
        backing,
    });

    dest
}

fn gen_expr_as_ref_path(g: &mut AstBytecodeGen, expr_id: ExprId) -> GeneratedRef {
    let ident_type = g
        .analysis
        .get_ident(expr_id)
        .expect("missing ident for mutating receiver");

    match ident_type {
        IdentType::Var(var_id) => {
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);
            let VarLocation::Stack = var.location else {
                unreachable!("captured ref assignment isn't supported")
            };

            if var.ty.is_ref() {
                return GeneratedRef {
                    reference: var_reg(g, var_id),
                    backing: None,
                };
            }

            let inner_ty = g.emitter.convert_ty(g.sa, var.ty.clone());
            let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
            g.builder
                .emit_get_register_ref(reference, var_reg(g, var_id));

            GeneratedRef {
                reference,
                backing: None,
            }
        }
        IdentType::Global(global_id) => {
            let global = g.sa.global(global_id);
            let inner_ty = g.emitter.convert_ty(g.sa, global.ty());
            let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
            let global_id = g.emitter.convert_global_id(g.sa, global_id);
            g.builder
                .emit_get_global_ref(reference, global_id, g.loc_for_expr(expr_id));

            GeneratedRef {
                reference,
                backing: None,
            }
        }
        _ => unreachable!("unexpected mutating receiver"),
    }
}

fn gen_expr_as_ref_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    field_expr: &FieldExpr,
    field_ty: SourceType,
) -> GeneratedRef {
    let inner_ty = g.emitter.convert_ty(g.sa, field_ty);
    let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
    let field_idx = add_field_const_pool_entry(g, expr_id);
    let object_ty = g.ty(field_expr.lhs);

    let (obj, backing) = if object_ty.is_ref() || object_ty.is_struct() || object_ty.is_tuple() {
        let object = gen_expr_as_ref(g, field_expr.lhs, object_ty);
        (object.reference, object.backing)
    } else {
        (gen_expr(g, field_expr.lhs, DataDest::Alloc), None)
    };

    g.builder
        .emit_get_field_ref(reference, obj, field_idx, g.loc_for_expr(expr_id));
    g.free_if_temp(obj);

    GeneratedRef { reference, backing }
}

pub(super) fn gen_expr_ref(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &RefExpr,
    dest: DataDest,
) -> Register {
    let inner_expr = g.analysis.expr(e.expr);

    match inner_expr {
        Expr::Path(..) => gen_expr_ref_path(g, expr_id, e, dest),
        Expr::Field(field_expr) => gen_expr_ref_field(g, expr_id, e, field_expr, dest),
        _ => unreachable!("ref expression should only be on variables or fields"),
    }
}

fn gen_expr_ref_path(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &RefExpr,
    dest: DataDest,
) -> Register {
    let ident_type = g
        .analysis
        .get_ident(e.expr)
        .expect("missing ident for ref expression");

    let inner_ty = g.emitter.convert_ty(g.sa, g.ty(e.expr));
    let ref_ty = BytecodeType::Ref(Box::new(inner_ty));
    let dest_reg = ensure_register(g, dest, ref_ty);

    match ident_type {
        IdentType::Var(var_id) => {
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);
            match var.location {
                VarLocation::Stack => {
                    let src_reg = var_reg(g, var_id);
                    g.builder.emit_get_register_ref(dest_reg, src_reg);
                }
                VarLocation::Context(scope_id, field_id) => {
                    gen_expr_ref_context_var(g, dest_reg, scope_id, field_id, expr_id);
                }
            }
        }
        IdentType::Context(context_id, field_id) => {
            gen_expr_ref_outer_context_var(g, dest_reg, context_id, field_id, expr_id);
        }
        IdentType::Global(global_id) => {
            let global_id = g.emitter.convert_global_id(g.sa, global_id);
            g.builder
                .emit_get_global_ref(dest_reg, global_id, g.loc_for_expr(expr_id));
        }
        _ => unreachable!("ref expression should only be on variables"),
    }

    dest_reg
}

fn gen_expr_ref_context_var(
    g: &mut AstBytecodeGen,
    dest: Register,
    scope_id: ScopeId,
    field_id: ContextFieldId,
    expr_id: ExprId,
) {
    let entered_context = &g.entered_contexts[scope_id.0];
    let context_id = entered_context.context_id;
    let context_reg = entered_context.register.expect("missing context register");
    emit_get_context_field_ref(g, dest, context_reg, context_id, field_id, expr_id);
}

fn gen_expr_ref_outer_context_var(
    g: &mut AstBytecodeGen,
    dest: Register,
    context_id: ContextId,
    field_id: ContextFieldId,
    expr_id: ExprId,
) {
    let location = g.loc_for_expr(expr_id);
    let context_reg = load_outer_context_object(g, context_id, location);
    emit_get_context_field_ref(g, dest, context_reg, context_id, field_id, expr_id);
    g.free_temp(context_reg);
}

fn emit_get_context_field_ref(
    g: &mut AstBytecodeGen,
    dest: Register,
    context_reg: Register,
    context_id: ContextId,
    field_id: ContextFieldId,
    expr_id: ExprId,
) {
    let context = g.sa.context(context_id);
    let field_index = field_id_from_context_idx(field_id, context.has_parent_slot());
    let class_id = g.emitter.convert_class_id(g.sa, context.class_id());
    let type_params = g.context_type_params(context_id);
    let field_idx = g
        .builder
        .add_const_field_types(class_id, type_params, field_index.0 as u32);
    g.builder
        .emit_get_field_ref(dest, context_reg, field_idx, g.loc_for_expr(expr_id));
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
