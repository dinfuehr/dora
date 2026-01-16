use dora_bytecode::{BytecodeType, BytecodeTypeArray, Location, Register};
use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

use super::bin::gen_intrinsic_bin;
use super::path::load_from_context;
use super::{
    add_const_pool_entry_for_call, emit_invoke_direct, emit_invoke_generic_direct, gen_expr,
    specialize_type_for_call,
};
use crate::generator::{
    AstBytecodeGen, DataDest, SELF_VAR_ID, field_id_from_context_idx, store_in_context, var_reg,
};
use crate::sema::{
    ContextFieldId, GlobalDefinitionId, IdentType, Intrinsic, OuterContextIdx, VarId, VarLocation,
};
use crate::ty::SourceType;

pub(super) fn gen_expr_assign(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    _dest: DataDest,
) -> Register {
    let lhs_expr = expr.lhs();

    if lhs_expr.is_path_expr() {
        let value_reg = gen_expr(g, expr.rhs(), DataDest::Alloc);
        let ident_type = g.analysis.get_ident(lhs_expr.id()).expect("missing ident");
        match ident_type {
            IdentType::Var(var_id) => {
                gen_expr_assign_var(g, expr.clone(), var_id, value_reg);
            }
            IdentType::Context(level, field_id) => {
                gen_expr_assign_context(g, expr.clone(), level, field_id, value_reg);
            }
            IdentType::Global(gid) => {
                gen_expr_assign_global(g, expr.clone(), gid, value_reg);
            }
            _ => unreachable!(),
        }
        g.free_if_temp(value_reg);
    } else {
        match lhs_expr {
            AstExpr::FieldExpr(field) => {
                gen_expr_assign_dot(g, expr.clone(), field);
            }
            AstExpr::CallExpr(call) => {
                gen_expr_assign_call(g, expr.clone(), call);
            }
            AstExpr::MethodCallExpr(method_call) => {
                gen_expr_assign_method_call(g, expr.clone(), method_call);
            }
            _ => unreachable!(),
        };
    }

    g.ensure_unit_register()
}

fn gen_expr_assign_call(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    call_expr: ast::AstCallExpr,
) {
    let object = call_expr.callee();
    let argument_list = call_expr.arg_list();

    let arg0 = argument_list.items().next().expect("argument expected");
    let index = arg0.expr().unwrap();
    let value = expr.rhs();

    let obj_reg = gen_expr(g, object.clone(), DataDest::Alloc);
    let idx_reg = gen_expr(g, index, DataDest::Alloc);
    let val_reg = gen_expr(g, value, DataDest::Alloc);

    let array_assignment = g
        .analysis
        .get_array_assignment(expr.id())
        .expect("missing assignment data");

    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let ty = g
            .emitter
            .convert_ty_reg(array_assignment.item_ty.expect("missing item type"));
        let current = g.alloc_temp(ty);

        let call_type = array_assignment.index_get.expect("missing index_get");
        let fct_id = call_type.fct_id().unwrap();
        let fct = g.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArrayGet);
            g.builder
                .emit_load_array(current, obj_reg, idx_reg, location);
        } else {
            let obj_ty = g.ty(object.id());

            g.builder.emit_push_register(obj_reg);
            g.builder.emit_push_register(idx_reg);

            let type_params = obj_ty.type_params();

            let callee_idx = g.builder.add_const_fct_types(
                g.emitter.convert_function_id(fct_id),
                g.convert_tya(&type_params),
            );
            g.builder.emit_invoke_direct(current, callee_idx, location);
        }

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, val_reg, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, val_reg, location);
        }

        current
    } else {
        val_reg
    };

    let call_type = array_assignment.index_set.expect("missing index_set");
    let fct_id = call_type.fct_id().unwrap();
    let fct = g.sa.fct(fct_id);

    if let Some(intrinsic) = fct.intrinsic.get() {
        assert_eq!(*intrinsic, Intrinsic::ArraySet);
        g.builder
            .emit_store_array(assign_value, obj_reg, idx_reg, location);
    } else {
        let obj_ty = g.ty(object.id());

        g.builder.emit_push_register(obj_reg);
        g.builder.emit_push_register(idx_reg);
        g.builder.emit_push_register(assign_value);

        let type_params = obj_ty.type_params();

        let callee_idx = g.builder.add_const_fct_types(
            g.emitter.convert_function_id(fct_id),
            g.convert_tya(&type_params),
        );
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_direct(dest, callee_idx, location);
    }

    g.free_if_temp(obj_reg);
    g.free_if_temp(idx_reg);
    g.free_if_temp(val_reg);
    g.free_if_temp(assign_value);
}

fn gen_expr_assign_method_call(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    method_call: ast::AstMethodCallExpr,
) {
    // Get the field type from the method call expression
    // The type stored on method_call is the field type (e.g., Vec[T] for self.vec)
    let field_ty = g.ty(method_call.id());

    let object = method_call.object();
    let argument_list = method_call.arg_list();

    let arg0 = argument_list.items().next().expect("argument expected");
    let index = arg0.expr().unwrap();
    let value = expr.rhs();

    // First generate code to load the field value (the array/vec)
    let field_reg = gen_expr_field_access(g, &method_call, object.clone());
    let idx_reg = gen_expr(g, index, DataDest::Alloc);
    let val_reg = gen_expr(g, value, DataDest::Alloc);

    let array_assignment = g
        .analysis
        .get_array_assignment(expr.id())
        .expect("missing assignment data");

    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let ty = g
            .emitter
            .convert_ty_reg(array_assignment.item_ty.expect("missing item type"));
        let current = g.alloc_temp(ty);

        let call_type = array_assignment.index_get.expect("missing index_get");
        let fct_id = call_type.fct_id().unwrap();
        let fct = g.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArrayGet);
            g.builder
                .emit_load_array(current, field_reg, idx_reg, location);
        } else {
            g.builder.emit_push_register(field_reg);
            g.builder.emit_push_register(idx_reg);

            let type_params = field_ty.type_params();

            let callee_idx = g.builder.add_const_fct_types(
                g.emitter.convert_function_id(fct_id),
                g.convert_tya(&type_params),
            );
            g.builder.emit_invoke_direct(current, callee_idx, location);
        }

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, val_reg, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, val_reg, location);
        }

        current
    } else {
        val_reg
    };

    let call_type = array_assignment.index_set.expect("missing index_set");
    let fct_id = call_type.fct_id().unwrap();
    let fct = g.sa.fct(fct_id);

    if let Some(intrinsic) = fct.intrinsic.get() {
        assert_eq!(*intrinsic, Intrinsic::ArraySet);
        g.builder
            .emit_store_array(assign_value, field_reg, idx_reg, location);
    } else {
        g.builder.emit_push_register(field_reg);
        g.builder.emit_push_register(idx_reg);
        g.builder.emit_push_register(assign_value);

        let type_params = field_ty.type_params();

        let callee_idx = g.builder.add_const_fct_types(
            g.emitter.convert_function_id(fct_id),
            g.convert_tya(&type_params),
        );
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_direct(dest, callee_idx, location);
    }

    g.free_if_temp(field_reg);
    g.free_if_temp(idx_reg);
    g.free_if_temp(val_reg);
    g.free_if_temp(assign_value);
}

fn gen_expr_field_access(
    g: &mut AstBytecodeGen,
    method_call: &ast::AstMethodCallExpr,
    object: ast::AstExpr,
) -> Register {
    let ident_type = g
        .analysis
        .get_ident(method_call.id())
        .expect("missing ident");

    match ident_type {
        IdentType::Field(cls_ty, field_index) => {
            let (cls_id, type_params) = cls_ty.to_class().expect("class expected");
            let field_idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(cls_id),
                g.convert_tya(&type_params),
                field_index.0 as u32,
            );

            let obj_reg = gen_expr(g, object.clone(), DataDest::Alloc);
            let field_ty = g.ty(method_call.id());
            let field_reg = g.alloc_temp(g.emitter.convert_ty_reg(field_ty));
            g.builder
                .emit_load_field(field_reg, obj_reg, field_idx, g.loc(object.span()));
            g.free_if_temp(obj_reg);
            field_reg
        }
        IdentType::StructField(struct_ty, field_index) => {
            let (struct_id, type_params) = struct_ty.to_struct().expect("struct expected");
            let field_idx = g.builder.add_const_struct_field(
                g.emitter.convert_struct_id(struct_id),
                g.convert_tya(&type_params),
                field_index.0 as u32,
            );

            let obj_reg = gen_expr(g, object.clone(), DataDest::Alloc);
            let field_ty = g.ty(method_call.id());
            let field_reg = g.alloc_temp(g.emitter.convert_ty_reg(field_ty));
            g.builder
                .emit_load_struct_field(field_reg, obj_reg, field_idx);
            g.free_if_temp(obj_reg);
            field_reg
        }
        _ => unreachable!(),
    }
}

fn gen_expr_assign_dot(g: &mut AstBytecodeGen, expr: ast::AstAssignExpr, field: ast::AstFieldExpr) {
    let (cls_ty, field_index) = {
        let ident_type = g.analysis.get_ident(field.id()).expect("missing ident");
        match ident_type {
            IdentType::Field(class, field) => (class, field),
            _ => unreachable!(),
        }
    };

    let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

    let field_idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&type_params),
        field_index.0 as u32,
    );

    let obj = gen_expr(g, field.lhs(), DataDest::Alloc);
    let value = gen_expr(g, expr.rhs(), DataDest::Alloc);

    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let cls = g.sa.class(cls_id);
        let field_id = cls.field_id(field_index);
        let ty = g.sa.field(field_id).ty();
        let ty = g.emitter.convert_ty_reg(ty);
        let current = g.alloc_temp(ty);
        g.builder.emit_load_field(current, obj, field_idx, location);

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder
        .emit_store_field(assign_value, obj, field_idx, location);

    if expr.op() != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }

    g.free_if_temp(obj);
    g.free_if_temp(value);
}

fn gen_expr_assign_context(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    outer_context_id: OuterContextIdx,
    context_field_id: ContextFieldId,
    value: Register,
) {
    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let current = load_from_outer_context(g, outer_context_id, context_field_id, location);

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, value, location);
        }

        current
    } else {
        value
    };

    store_in_outer_context(
        g,
        outer_context_id,
        context_field_id,
        assign_value,
        location,
    );

    if expr.op() != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }
}

fn gen_expr_assign_var(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    var_id: VarId,
    value: Register,
) {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let current = match var.location {
            VarLocation::Context(scope_id, field_id) => {
                let ty = g.emitter.convert_ty_reg(var.ty.clone());
                let dest_reg = g.alloc_temp(ty);
                load_from_context(g, dest_reg, scope_id, field_id, g.loc(expr.span()));
                dest_reg
            }

            VarLocation::Stack => var_reg(g, var_id),
        };

        let location = g.loc(expr.span());

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, value, location);
        }

        current
    } else {
        value
    };

    match var.location {
        VarLocation::Context(scope_id, field_id) => {
            store_in_context(g, assign_value, scope_id, field_id, g.loc(expr.span()));
        }

        VarLocation::Stack => {
            let var_reg = var_reg(g, var_id);
            g.builder.emit_mov(var_reg, assign_value);
        }
    }

    if expr.op() != ast::AssignOp::Assign {
        g.free_if_temp(assign_value);
    }
}

fn gen_expr_assign_global(
    g: &mut AstBytecodeGen,
    expr: ast::AstAssignExpr,
    gid: GlobalDefinitionId,
    value: Register,
) {
    let bc_gid = g.emitter.convert_global_id(gid);
    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::AssignOp::Assign {
        let global = g.sa.global(gid);
        let ty = g.emitter.convert_ty_reg(global.ty());
        let current = g.alloc_temp(ty);
        g.builder.emit_load_global(current, bc_gid, location);

        if let Some(info) = g.get_intrinsic(expr.id()) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr.clone().into(), current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder.emit_store_global(assign_value, bc_gid);

    if expr.op() != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }
}

fn gen_method_bin(
    g: &mut AstBytecodeGen,
    expr: ast::AstExpr,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    let call_type = g
        .analysis
        .get_call_type(expr.id())
        .expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, &call_type, callee.return_type());

    g.builder.emit_push_register(lhs_reg);
    g.builder.emit_push_register(rhs_reg);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(g, function_return_type, dest, callee_idx, location);
    } else {
        emit_invoke_direct(g, function_return_type, dest, callee_idx, location);
    }
}

pub(super) fn store_in_outer_context(
    g: &mut AstBytecodeGen,
    level: OuterContextIdx,
    context_idx: ContextFieldId,
    value: Register,
    location: Location,
) {
    let self_reg = var_reg(g, SELF_VAR_ID);

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
    assert!(level.0 < outer_contexts.len());

    for outer_context_class in outer_contexts.iter().skip(level.0 + 1).rev() {
        if outer_context_class.has_class_id() {
            let outer_cls_id = outer_context_class.class_id();

            let idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(outer_cls_id),
                g.convert_tya(&g.identity_type_params()),
                0,
            );
            g.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
        }
    }

    let outer_context_info = outer_contexts[level.0].clone();
    let outer_cls_id = outer_context_info.class_id();
    let field_index = field_id_from_context_idx(context_idx, outer_context_info.has_parent_slot());
    let idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(outer_cls_id),
        g.convert_tya(&g.identity_type_params()),
        field_index.0 as u32,
    );
    g.builder
        .emit_store_field(value, outer_context_reg, idx, location);

    g.free_temp(outer_context_reg);
}

pub(super) fn load_from_outer_context(
    g: &mut AstBytecodeGen,
    context_id: OuterContextIdx,
    field_id: ContextFieldId,
    location: Location,
) -> Register {
    assert!(g.is_lambda);
    let self_reg = var_reg(g, SELF_VAR_ID);

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
    let dest = g.alloc_temp(ty);

    let idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(outer_cls_id),
        g.convert_tya(&g.identity_type_params()),
        field_index.0 as u32,
    );
    g.builder
        .emit_load_field(dest, outer_context_reg, idx, location);

    g.free_temp(outer_context_reg);

    dest
}
