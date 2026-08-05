use dora_bytecode::{BytecodeType, Register};

use super::{ensure_register, gen_expr};
use crate::generator::expr::method_call::gen_expr_method_call_field_object;
use crate::generator::{
    AstBytecodeGen, DataDest, field_id_from_context_idx, load_outer_context_object, var_reg,
};
use crate::sema::{
    CallExpr, CallType, ContextFieldId, ContextId, Expr, ExprId, FieldExpr, IdentType, Intrinsic,
    MethodCallExpr, RefExpr, ScopeId, VarId, VarLocation,
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
        Expr::Path(..) => gen_expr_as_ref_path(g, expr_id, ty),
        Expr::Field(field_expr) => {
            gen_expr_as_ref_field(g, expr_id, field_expr, ty, DataDest::Alloc)
        }
        Expr::Call(call_expr) if is_array_get(g, expr_id) => GeneratedRef {
            reference: gen_expr_ref_array_element(g, expr_id, call_expr, DataDest::Alloc),
            backing: None,
        },
        Expr::MethodCall(method_call) if is_array_get(g, expr_id) => GeneratedRef {
            reference: gen_expr_ref_array_field_element(g, expr_id, method_call, DataDest::Alloc),
            backing: None,
        },
        Expr::Paren(inner_expr) => gen_expr_as_ref(g, *inner_expr, ty),
        // An explicit `ref` expression already provides a reference to use.
        Expr::Ref(_) => GeneratedRef {
            reference: gen_expr(g, expr_id, DataDest::Alloc),
            backing: None,
        },
        _ => gen_temporary_expr_as_ref(g, expr_id, ty),
    }
}

fn gen_temporary_expr_as_ref(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    ty: SourceType,
) -> GeneratedRef {
    let value = gen_expr(g, expr_id, DataDest::Alloc);
    let inner_ty = g.emitter.convert_ty(g.sa, ty);
    let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
    g.builder.emit_get_register_ref(reference, value);

    GeneratedRef {
        reference,
        backing: Some(value),
    }
}

enum FieldRefBase {
    Value(ExprId),
    Ref(ExprId),
}

impl FieldRefBase {
    fn expr_id(&self) -> ExprId {
        match self {
            FieldRefBase::Value(expr_id) | FieldRefBase::Ref(expr_id) => *expr_id,
        }
    }
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
    let base = loop {
        match g.analysis.expr(current_expr) {
            Expr::Field(field_expr) => {
                fields.push(current_expr);

                let object_ty = g.ty(field_expr.lhs);
                if !object_ty.is_struct() && !object_ty.is_tuple() {
                    assert!(object_ty.is_class());
                    break FieldRefBase::Value(field_expr.lhs);
                }

                current_expr = field_expr.lhs;
            }
            _ => break FieldRefBase::Ref(current_expr),
        }
    };
    assert!(!fields.is_empty(), "field reference path is empty");

    // For a single field access, bail out to emit the regular `LoadField`. The exception is a local
    // `ref` variable: that path would first dereference it with `LoadRef`, copying the entire
    // aggregate. `GetFieldRef` followed by `LoadRef` instead copies only the requested field.
    if fields.len() == 1 && !is_ref_var(g, base.expr_id()) {
        return None;
    }

    fields.reverse();
    Some(FieldRefPath { base, fields })
}

/// Returns whether the expression is a ref-typed local variable, possibly parenthesized.
fn is_ref_var(g: &AstBytecodeGen, expr_id: ExprId) -> bool {
    match g.analysis.expr(expr_id) {
        Expr::Path(..) => match g.analysis.get_ident(expr_id) {
            Some(IdentType::Var(var_id)) => {
                let vars = g.analysis.vars();
                let var = vars.get_var(var_id);

                var.ty.is_ref()
            }
            _ => false,
        },
        // The parenthesis split the field path, but do not change the base's storage properties.
        Expr::Paren(inner_expr) => is_ref_var(g, *inner_expr),
        _ => false,
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

fn gen_expr_as_ref_path(g: &mut AstBytecodeGen, expr_id: ExprId, ty: SourceType) -> GeneratedRef {
    let ident_type = g
        .analysis
        .get_ident(expr_id)
        .expect("missing ident for mutating receiver");

    match ident_type {
        IdentType::Var(var_id) => {
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);

            if var.ty.is_ref() {
                let VarLocation::Stack = var.location else {
                    unreachable!("captured ref values aren't supported")
                };

                return GeneratedRef {
                    reference: var_reg(g, var_id),
                    backing: None,
                };
            }

            let inner_ty = g.emitter.convert_ty(g.sa, var.ty.clone());
            let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));

            match var.location {
                VarLocation::Stack => g
                    .builder
                    .emit_get_register_ref(reference, var_reg(g, var_id)),
                VarLocation::Context(scope_id, field_id) => {
                    gen_expr_ref_context_var(g, reference, scope_id, field_id, expr_id);
                }
            }

            GeneratedRef {
                reference,
                backing: None,
            }
        }
        IdentType::Context {
            context_id,
            field_id,
            ..
        } => {
            let inner_ty = g.emitter.convert_ty(g.sa, ty);
            let reference = g.alloc_temp(BytecodeType::Ref(Box::new(inner_ty)));
            gen_expr_ref_outer_context_var(g, reference, context_id, field_id, expr_id);

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
        _ => gen_temporary_expr_as_ref(g, expr_id, ty),
    }
}

fn gen_expr_as_ref_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    field_expr: &FieldExpr,
    field_ty: SourceType,
    dest: DataDest,
) -> GeneratedRef {
    let inner_ty = g.emitter.convert_ty(g.sa, field_ty);
    let reference = ensure_register(g, dest, BytecodeType::Ref(Box::new(inner_ty)));
    let field_idx = add_field_const_pool_entry(g, expr_id);
    let object_ty = g.ty(field_expr.lhs);

    let (obj, backing) = if object_ty.is_class() {
        // Class bases are references already, so they can be generated directly.
        (gen_expr(g, field_expr.lhs, DataDest::Alloc), None)
    } else if let Some(var_id) = direct_stack_value_var(g, field_expr.lhs) {
        // Stack-local aggregates can use their existing value or reference register directly.
        (var_reg(g, var_id), None)
    } else {
        debug_assert!(object_ty.is_ref() || object_ty.is_struct() || object_ty.is_tuple());
        // Value aggregates need to remain backed by their original storage. For example,
        // `ref holder.pairs(index).value` must take a ref to the array element before taking its
        // `value` field; loading the element normally would create a temporary copy.
        let object = gen_expr_as_ref(g, field_expr.lhs, object_ty);
        (object.reference, object.backing)
    };

    g.builder
        .emit_get_field_ref(reference, obj, field_idx, g.loc_for_expr(expr_id));
    g.free_if_temp(obj);

    GeneratedRef { reference, backing }
}

fn direct_stack_value_var(g: &AstBytecodeGen, expr_id: ExprId) -> Option<VarId> {
    match g.analysis.expr(expr_id) {
        Expr::Path(_) => {
            let Some(IdentType::Var(var_id)) = g.analysis.get_ident(expr_id) else {
                return None;
            };

            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);

            if var.location.is_stack() {
                Some(var_id)
            } else {
                None
            }
        }
        Expr::Paren(inner_expr) => direct_stack_value_var(g, *inner_expr),
        _ => None,
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
        Expr::Path(..) => gen_expr_ref_path(g, expr_id, e, dest),
        Expr::Field(field_expr) => gen_expr_ref_field(g, e, field_expr, dest),
        Expr::Call(call_expr) => {
            if g.ty(e.expr).is_ref() {
                gen_expr(g, e.expr, dest)
            } else {
                gen_expr_ref_array_element(g, e.expr, call_expr, dest)
            }
        }
        Expr::MethodCall(method_call) => {
            if g.ty(e.expr).is_ref() {
                gen_expr(g, e.expr, dest)
            } else {
                gen_expr_ref_array_field_element(g, e.expr, method_call, dest)
            }
        }
        Expr::Paren(inner_expr) => gen_expr_ref(
            g,
            expr_id,
            &RefExpr {
                expr: *inner_expr,
                is_mut: e.is_mut,
            },
            dest,
        ),
        _ => unreachable!(
            "ref expression should only be on variables, fields, array elements, or ref-returning calls"
        ),
    }
}

fn gen_expr_ref_array_field_element(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    method_call: &MethodCallExpr,
    dest: DataDest,
) -> Register {
    let info = g.get_intrinsic(expr_id).expect("missing array intrinsic");
    assert_eq!(info.intrinsic, Intrinsic::ArrayGet);
    assert_eq!(method_call.args.len(), 1);

    let inner_ty = g.emitter.convert_ty(g.sa, g.ty(expr_id));
    let dest_reg = ensure_register(g, dest, BytecodeType::Ref(Box::new(inner_ty)));
    let array_reg = gen_expr_method_call_field_object(g, expr_id, method_call);
    let index_reg = gen_expr(g, method_call.args[0].expr, DataDest::Alloc);

    g.builder
        .emit_get_array_ref(dest_reg, array_reg, index_reg, g.loc_for_expr(expr_id));

    g.free_if_temp(array_reg);
    g.free_if_temp(index_reg);

    dest_reg
}

fn gen_expr_ref_array_element(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    call_expr: &CallExpr,
    dest: DataDest,
) -> Register {
    let info = g.get_intrinsic(expr_id).expect("missing array intrinsic");
    assert_eq!(info.intrinsic, Intrinsic::ArrayGet);
    assert_eq!(call_expr.args.len(), 1);

    let inner_ty = g.emitter.convert_ty(g.sa, g.ty(expr_id));
    let dest_reg = ensure_register(g, dest, BytecodeType::Ref(Box::new(inner_ty)));
    let array_reg = gen_expr(g, call_expr.callee, DataDest::Alloc);
    let index_reg = gen_expr(g, call_expr.args[0].expr, DataDest::Alloc);

    g.builder
        .emit_get_array_ref(dest_reg, array_reg, index_reg, g.loc_for_expr(expr_id));

    g.free_if_temp(array_reg);
    g.free_if_temp(index_reg);

    dest_reg
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

    match ident_type {
        IdentType::Var(var_id) => {
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);
            match var.location {
                VarLocation::Stack => {
                    let src_reg = var_reg(g, var_id);
                    if var.ty.is_ref() {
                        if dest.is_alloc() {
                            src_reg
                        } else {
                            let dest_reg = dest.reg();
                            g.builder.emit_mov(dest_reg, src_reg);
                            dest_reg
                        }
                    } else {
                        let dest_reg = ensure_register(g, dest, ref_ty);
                        g.builder.emit_get_register_ref(dest_reg, src_reg);
                        dest_reg
                    }
                }
                VarLocation::Context(scope_id, field_id) => {
                    let dest_reg = ensure_register(g, dest, ref_ty);
                    gen_expr_ref_context_var(g, dest_reg, scope_id, field_id, expr_id);
                    dest_reg
                }
            }
        }
        IdentType::Context {
            context_id,
            field_id,
            ..
        } => {
            let dest_reg = ensure_register(g, dest, ref_ty);
            gen_expr_ref_outer_context_var(g, dest_reg, context_id, field_id, expr_id);
            dest_reg
        }
        IdentType::Global(global_id) => {
            let dest_reg = ensure_register(g, dest, ref_ty);
            let global_id = g.emitter.convert_global_id(g.sa, global_id);
            g.builder
                .emit_get_global_ref(dest_reg, global_id, g.loc_for_expr(expr_id));
            dest_reg
        }
        _ => unreachable!("ref expression should only be on variables"),
    }
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
    e: &RefExpr,
    field_expr: &FieldExpr,
    dest: DataDest,
) -> Register {
    let field_ty = g.ty(e.expr);
    let generated_ref = gen_expr_as_ref_field(g, e.expr, field_expr, field_ty, dest);

    if let Some(backing) = generated_ref.backing {
        g.free_if_temp(backing);
    }

    generated_ref.reference
}

fn is_array_get(g: &AstBytecodeGen, expr_id: ExprId) -> bool {
    let Some(call_type) = g.analysis.get_call_type(expr_id) else {
        return false;
    };

    // Index syntax like `array(index)` uses `CallType::Index`, while an explicit
    // method call like `array.get(index)` uses `CallType::Method`, which must not
    // be handled as an array access here.
    if !matches!(call_type.as_ref(), CallType::Index(..)) {
        return false;
    }

    g.get_intrinsic(expr_id)
        .map(|info| info.intrinsic == Intrinsic::ArrayGet)
        .unwrap_or(false)
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
