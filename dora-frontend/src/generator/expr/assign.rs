use dora_bytecode::{BytecodeType, BytecodeTypeArray, ConstPoolIdx, Location, Register};
use dora_parser::ast;

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
    AssignExpr, CallExpr, ContextFieldId, Expr, ExprId, FieldExpr, GlobalDefinitionId, IdentType,
    Intrinsic, MethodCallExpr, OuterContextIdx, VarId, VarLocation,
};
use crate::specialize::specialize_type;
use crate::ty::SourceType;

/// Represents a chain of value-type (struct/tuple) field accesses.
/// For an expression like `a.b.c.d` where all are struct fields,
/// `base_expr` would be `a` and `field_chain` would be `[a.b, a.b.c, a.b.c.d]`
/// (the field expressions in order from outermost to innermost).
struct ValueTypeFieldChain {
    /// The base expression to evaluate first (e.g., `a` or `obj.class_field`)
    base_expr: ExprId,
    /// Field expressions for value-type field accesses, in order
    field_chain: Vec<ExprId>,
}

/// Collects a chain of field accesses starting from a field expression.
/// Includes both value-type (struct/tuple) fields and the first class field
/// encountered. Stops at the object expression before the class field.
fn collect_value_type_field_chain(
    g: &AstBytecodeGen,
    field_expr_id: ExprId,
) -> ValueTypeFieldChain {
    let mut chain = Vec::new();
    let mut current_expr = field_expr_id;

    loop {
        let expr = g.analysis.expr(current_expr);
        match expr {
            Expr::Paren(inner_expr) => {
                // Look through parentheses
                current_expr = *inner_expr;
            }
            Expr::Field(field_expr) => {
                // Check if this field access is a value type (struct or tuple)
                if let Some(ident_type) = g.analysis.get_ident(current_expr) {
                    match ident_type {
                        IdentType::StructField(..) | IdentType::TupleField(..) => {
                            // This is a value-type field access, add to chain and continue
                            chain.push(current_expr);
                            current_expr = field_expr.lhs;
                        }
                        IdentType::Field(..) => {
                            // Class field - include in chain but stop after this
                            // The base becomes the object expression (field_expr.lhs)
                            chain.push(current_expr);
                            current_expr = field_expr.lhs;
                            break;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }
            _ => {
                // Not a field expression, stop
                break;
            }
        }
    }

    // Reverse the chain so it's in order from outermost to innermost
    chain.reverse();

    ValueTypeFieldChain {
        base_expr: current_expr,
        field_chain: chain,
    }
}

/// Emits GetFieldAddress instructions for a chain of value-type field accesses.
/// Returns the final address register.
fn emit_field_chain_addresses(
    g: &mut AstBytecodeGen,
    base_reg: Register,
    field_chain: &[ExprId],
    location: Location,
) -> Register {
    assert!(!field_chain.is_empty());

    let mut current_addr = base_reg;

    for (i, &field_expr_id) in field_chain.iter().enumerate() {
        let ident_type = g.analysis.get_ident(field_expr_id).expect("missing ident");
        let field_idx = add_field_const_pool_entry(g, &ident_type);

        let new_addr = g.alloc_temp(BytecodeType::Address);
        g.builder
            .emit_get_field_address(new_addr, current_addr, field_idx, location);

        if i > 0 {
            g.free_temp(current_addr);
        }
        current_addr = new_addr;
    }

    current_addr
}

/// Adds a const pool entry for a field access (class, struct, or tuple).
fn add_field_const_pool_entry(g: &mut AstBytecodeGen, ident_type: &IdentType) -> ConstPoolIdx {
    match ident_type {
        IdentType::Field(cls_ty, field_index) => {
            let (cls_id, type_params) = cls_ty.to_class().expect("class expected");
            let bc_class_id = g.emitter.convert_class_id(g.sa, cls_id);
            let bc_type_params = g.convert_tya(&type_params);
            g.builder
                .add_const_field_types(bc_class_id, bc_type_params, field_index.0 as u32)
        }
        IdentType::StructField(struct_ty, field_index) => {
            let (struct_id, type_params) = struct_ty.to_struct().expect("struct expected");
            let bc_struct_id = g.emitter.convert_struct_id(g.sa, struct_id);
            let bc_type_params = g.convert_tya(&type_params);
            g.builder
                .add_const_struct_field(bc_struct_id, bc_type_params, field_index.0 as u32)
        }
        IdentType::TupleField(tuple_ty, element_idx) => {
            let bc_tuple_ty = g.emitter.convert_ty(g.sa, tuple_ty.clone());
            g.builder.add_const_tuple_element(bc_tuple_ty, *element_idx)
        }
        _ => unreachable!(),
    }
}

pub(super) fn gen_expr_assign(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    _dest: DataDest,
) -> Register {
    let lhs_expr = g.analysis.expr(e.lhs);

    match lhs_expr {
        Expr::Path(_) | Expr::Paren(_) => {
            // Path-like expression (variable, global, context variable)
            let value_reg = gen_expr(g, e.rhs, DataDest::Alloc);
            let ident_type = g.analysis.get_ident(e.lhs).expect("missing ident");
            match ident_type {
                IdentType::Var(var_id) => {
                    gen_expr_assign_var(g, expr_id, e, var_id, value_reg);
                }
                IdentType::Context(level, field_id) => {
                    gen_expr_assign_context(g, expr_id, e, level, field_id, value_reg);
                }
                IdentType::Global(gid) => {
                    gen_expr_assign_global(g, expr_id, e, gid, value_reg);
                }
                _ => unreachable!(),
            }
            g.free_if_temp(value_reg);
        }
        Expr::Field(field) => {
            gen_expr_assign_dot(g, expr_id, e, field);
        }
        Expr::Call(call) => {
            gen_expr_assign_call(g, expr_id, e, call);
        }
        Expr::MethodCall(method_call) => {
            gen_expr_assign_method_call(g, expr_id, e, e.lhs, method_call);
        }
        _ => unreachable!(),
    }

    g.ensure_unit_register()
}

fn gen_expr_assign_call(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    call_expr: &CallExpr,
) {
    let object_id = call_expr.callee;
    let index_id = call_expr.args[0].expr;

    let obj_reg = gen_expr(g, object_id, DataDest::Alloc);
    let idx_reg = gen_expr(g, index_id, DataDest::Alloc);
    let val_reg = gen_expr(g, e.rhs, DataDest::Alloc);

    let array_assignment = g
        .analysis
        .get_array_assignment(expr_id)
        .expect("missing assignment data");

    let location = g.loc_for_expr(expr_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let ty = g
            .emitter
            .convert_ty_reg(g.sa, array_assignment.item_ty.expect("missing item type"));
        let current = g.alloc_temp(ty);

        let call_type = array_assignment.index_get.expect("missing index_get");
        let fct_id = call_type.fct_id().unwrap();
        let fct = g.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArrayGet);
            g.builder
                .emit_load_array(current, obj_reg, idx_reg, location);
        } else {
            let obj_ty = g.ty(object_id);

            let type_params = obj_ty.type_params();
            let fct_id = g.emitter.convert_function_id(g.sa, fct_id);
            let type_params = g.convert_tya(&type_params);

            let callee_idx = g.builder.add_const_fct_types(fct_id, type_params);
            g.builder
                .emit_invoke_direct(current, callee_idx, &[obj_reg, idx_reg], location);
        }

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, val_reg, location);
        } else {
            gen_method_bin(g, expr_id, current, current, val_reg, location);
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
        let obj_ty = g.ty(object_id);

        let type_params = obj_ty.type_params();
        let bc_fct_id = g.emitter.convert_function_id(g.sa, fct_id);
        let bc_type_params = g.convert_tya(&type_params);

        let callee_idx = g.builder.add_const_fct_types(bc_fct_id, bc_type_params);
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_direct(
            dest,
            callee_idx,
            &[obj_reg, idx_reg, assign_value],
            location,
        );
    }

    g.free_if_temp(obj_reg);
    g.free_if_temp(idx_reg);
    g.free_if_temp(val_reg);
    g.free_if_temp(assign_value);
}

fn gen_expr_assign_method_call(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    lhs_expr_id: ExprId,
    method_call: &MethodCallExpr,
) {
    // Get the field type from the method call expression
    // The type stored on method_call is the field type (e.g., Vec[T] for self.vec)
    let field_ty = g.ty(lhs_expr_id);

    let object_id = method_call.object;
    let index_id = method_call.args[0].expr;

    // First generate code to load the field value (the array/vec)
    let field_reg = gen_expr_field_access(g, lhs_expr_id, object_id);
    let idx_reg = gen_expr(g, index_id, DataDest::Alloc);
    let val_reg = gen_expr(g, e.rhs, DataDest::Alloc);

    let array_assignment = g
        .analysis
        .get_array_assignment(expr_id)
        .expect("missing assignment data");

    let location = g.loc_for_expr(expr_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let ty = g
            .emitter
            .convert_ty_reg(g.sa, array_assignment.item_ty.expect("missing item type"));
        let current = g.alloc_temp(ty);

        let call_type = array_assignment.index_get.expect("missing index_get");
        let fct_id = call_type.fct_id().unwrap();
        let fct = g.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArrayGet);
            g.builder
                .emit_load_array(current, field_reg, idx_reg, location);
        } else {
            let type_params = field_ty.type_params();
            let bc_fct_id = g.emitter.convert_function_id(g.sa, fct_id);
            let bc_type_params = g.convert_tya(&type_params);

            let callee_idx = g.builder.add_const_fct_types(bc_fct_id, bc_type_params);
            g.builder
                .emit_invoke_direct(current, callee_idx, &[field_reg, idx_reg], location);
        }

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, val_reg, location);
        } else {
            gen_method_bin(g, expr_id, current, current, val_reg, location);
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
        let type_params = field_ty.type_params();
        let bc_fct_id = g.emitter.convert_function_id(g.sa, fct_id);
        let bc_type_params = g.convert_tya(&type_params);

        let callee_idx = g.builder.add_const_fct_types(bc_fct_id, bc_type_params);
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_direct(
            dest,
            callee_idx,
            &[field_reg, idx_reg, assign_value],
            location,
        );
    }

    g.free_if_temp(field_reg);
    g.free_if_temp(idx_reg);
    g.free_if_temp(val_reg);
    g.free_if_temp(assign_value);
}

fn gen_expr_field_access(
    g: &mut AstBytecodeGen,
    method_call_id: ExprId,
    object_id: ExprId,
) -> Register {
    let ident_type = g.analysis.get_ident(method_call_id).expect("missing ident");

    match ident_type {
        IdentType::Field(cls_ty, field_index) => {
            let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

            // Get the field type from the class definition
            let cls = g.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let field = g.sa.field(field_id);
            let field_ty = specialize_type(g.sa, field.ty(), &type_params);

            let bc_class_id = g.emitter.convert_class_id(g.sa, cls_id);
            let bc_type_params = g.convert_tya(&type_params);
            let field_idx =
                g.builder
                    .add_const_field_types(bc_class_id, bc_type_params, field_index.0 as u32);

            let obj_reg = gen_expr(g, object_id, DataDest::Alloc);
            let field_ty = g.emitter.convert_ty_reg(g.sa, field_ty);
            let field_reg = g.alloc_temp(field_ty);
            g.builder
                .emit_load_field(field_reg, obj_reg, field_idx, g.loc_for_expr(object_id));
            g.free_if_temp(obj_reg);
            field_reg
        }
        IdentType::StructField(struct_ty, field_index) => {
            let (struct_id, type_params) = struct_ty.to_struct().expect("struct expected");

            // Get the field type from the struct definition
            let struct_ = g.sa.struct_(struct_id);
            let field_id = struct_.field_id(field_index);
            let field = g.sa.field(field_id);
            let field_ty = specialize_type(g.sa, field.ty(), &type_params);

            let bc_struct_id = g.emitter.convert_struct_id(g.sa, struct_id);
            let bc_type_params = g.convert_tya(&type_params);
            let field_idx = g.builder.add_const_struct_field(
                bc_struct_id,
                bc_type_params,
                field_index.0 as u32,
            );

            let obj_reg = gen_expr(g, object_id, DataDest::Alloc);
            let field_ty = g.emitter.convert_ty_reg(g.sa, field_ty);
            let field_reg = g.alloc_temp(field_ty);
            g.builder
                .emit_load_field(field_reg, obj_reg, field_idx, g.loc_for_expr(object_id));
            g.free_if_temp(obj_reg);
            field_reg
        }
        _ => unreachable!(),
    }
}

fn gen_expr_assign_dot(g: &mut AstBytecodeGen, expr_id: ExprId, e: &AssignExpr, field: &FieldExpr) {
    let ident_type = g.analysis.get_ident(e.lhs).expect("missing ident");

    match ident_type {
        IdentType::TupleField(tuple_ty, element_idx) => {
            gen_expr_assign_tuple_field(g, expr_id, e, tuple_ty, element_idx);
        }
        IdentType::StructField(struct_ty, field_index) => {
            gen_expr_assign_struct_field(g, expr_id, e, struct_ty, field_index);
        }
        IdentType::Field(cls_ty, field_index) => {
            gen_expr_assign_class_field(g, expr_id, e, field, cls_ty, field_index);
        }
        _ => unreachable!(),
    }
}

fn gen_expr_assign_tuple_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    tuple_ty: SourceType,
    element_idx: u32,
) {
    let subtypes = tuple_ty.tuple_subtypes().expect("tuple expected");
    let element_ty = subtypes[element_idx as usize].clone();

    // Collect chain of value-type field accesses, including the field being assigned to
    let chain = collect_value_type_field_chain(g, e.lhs);

    let base = gen_expr(g, chain.base_expr, DataDest::Alloc);
    let value = gen_expr(g, e.rhs, DataDest::Alloc);

    let location = g.loc_for_expr(expr_id);

    // Emit GetFieldAddress for all fields in the chain
    let address = emit_field_chain_addresses(g, base, &chain.field_chain, location);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let ty = g.emitter.convert_ty_reg(g.sa, element_ty);
        let current = g.alloc_temp(ty);
        g.builder.emit_load_address(current, address);

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder
        .emit_store_at_address(assign_value, address, location);
    g.free_temp(address);

    if e.op != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }

    g.free_if_temp(base);
    g.free_if_temp(value);
}

fn gen_expr_assign_struct_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    struct_ty: SourceType,
    field_index: crate::sema::FieldIndex,
) {
    let (struct_id, type_params) = struct_ty.to_struct().expect("struct expected");

    // Collect chain of value-type field accesses, including the field being assigned to
    let chain = collect_value_type_field_chain(g, e.lhs);

    let base = gen_expr(g, chain.base_expr, DataDest::Alloc);
    let value = gen_expr(g, e.rhs, DataDest::Alloc);

    let location = g.loc_for_expr(expr_id);

    // Emit GetFieldAddress for all fields in the chain
    let address = emit_field_chain_addresses(g, base, &chain.field_chain, location);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let struct_ = g.sa.struct_(struct_id);
        let field_id = struct_.field_id(field_index);
        let ty = g.sa.field(field_id).ty();
        let ty = specialize_type(g.sa, ty, &type_params);
        let ty = g.emitter.convert_ty_reg(g.sa, ty);
        let current = g.alloc_temp(ty);
        g.builder.emit_load_address(current, address);

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder
        .emit_store_at_address(assign_value, address, location);
    g.free_temp(address);

    if e.op != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }

    g.free_if_temp(base);
    g.free_if_temp(value);
}

fn gen_expr_assign_class_field(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    field: &FieldExpr,
    cls_ty: SourceType,
    field_index: crate::sema::FieldIndex,
) {
    let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

    let bc_class_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&type_params);
    let field_idx =
        g.builder
            .add_const_field_types(bc_class_id, bc_type_params, field_index.0 as u32);

    let obj = gen_expr(g, field.lhs, DataDest::Alloc);
    let value = gen_expr(g, e.rhs, DataDest::Alloc);

    let location = g.loc_for_expr(expr_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let cls = g.sa.class(cls_id);
        let field_id = cls.field_id(field_index);
        let ty = g.sa.field(field_id).ty();
        let ty = g.emitter.convert_ty_reg(g.sa, ty);
        let current = g.alloc_temp(ty);
        g.builder.emit_load_field(current, obj, field_idx, location);

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder
        .emit_store_field(assign_value, obj, field_idx, location);

    if e.op != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }

    g.free_if_temp(obj);
    g.free_if_temp(value);
}

fn gen_expr_assign_context(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    outer_context_id: OuterContextIdx,
    context_field_id: ContextFieldId,
    value: Register,
) {
    let location = g.loc_for_expr(expr_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let current = load_from_outer_context(g, outer_context_id, context_field_id, location);

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
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

    if e.op != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }
}

fn gen_expr_assign_var(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    var_id: VarId,
    value: Register,
) {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let location = g.loc_for_expr(expr_id);
        let current = match var.location {
            VarLocation::Context(scope_id, field_id) => {
                let ty = g.emitter.convert_ty_reg(g.sa, var.ty.clone());
                let dest_reg = g.alloc_temp(ty);
                load_from_context(g, dest_reg, scope_id, field_id, location);
                dest_reg
            }

            VarLocation::Stack => var_reg(g, var_id),
        };

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
        }

        current
    } else {
        value
    };

    match var.location {
        VarLocation::Context(scope_id, field_id) => {
            store_in_context(g, assign_value, scope_id, field_id, g.loc_for_expr(expr_id));
        }

        VarLocation::Stack => {
            let var_reg = var_reg(g, var_id);
            g.builder.emit_mov(var_reg, assign_value);
        }
    }

    if e.op != ast::AssignOp::Assign {
        g.free_if_temp(assign_value);
    }
}

fn gen_expr_assign_global(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &AssignExpr,
    gid: GlobalDefinitionId,
    value: Register,
) {
    let bc_gid = g.emitter.convert_global_id(g.sa, gid);
    let location = g.loc_for_expr(expr_id);

    let assign_value = if e.op != ast::AssignOp::Assign {
        let global = g.sa.global(gid);
        let ty = g.emitter.convert_ty_reg(g.sa, global.ty());
        let current = g.alloc_temp(ty);
        g.builder.emit_load_global(current, bc_gid, location);

        if let Some(info) = g.get_intrinsic(expr_id) {
            gen_intrinsic_bin(g, info.intrinsic, current, current, value, location);
        } else {
            gen_method_bin(g, expr_id, current, current, value, location);
        }

        current
    } else {
        value
    };

    g.builder.emit_store_global(assign_value, bc_gid);

    if e.op != ast::AssignOp::Assign {
        g.free_temp(assign_value);
    }
}

fn gen_method_bin(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, &call_type, callee.return_type());

    let arguments = &[lhs_reg, rhs_reg];

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            arguments,
            location,
        );
    } else {
        emit_invoke_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            arguments,
            location,
        );
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
        g.emitter.convert_class_id(g.sa, lambda_cls_id),
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
            let bc_class_id = g.emitter.convert_class_id(g.sa, outer_cls_id);
            let bc_type_params = g.convert_tya(&g.identity_type_params());

            let idx = g
                .builder
                .add_const_field_types(bc_class_id, bc_type_params, 0);
            g.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
        }
    }

    let outer_context_info = outer_contexts[level.0].clone();
    let outer_cls_id = outer_context_info.class_id();
    let field_index = field_id_from_context_idx(context_idx, outer_context_info.has_parent_slot());
    let bc_class_id = g.emitter.convert_class_id(g.sa, outer_cls_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let idx = g
        .builder
        .add_const_field_types(bc_class_id, bc_type_params, field_index.0 as u32);
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
    let bc_lambda_cls_id = g.emitter.convert_class_id(g.sa, lambda_cls_id);
    let idx = g
        .builder
        .add_const_field_types(bc_lambda_cls_id, BytecodeTypeArray::empty(), 0);
    g.builder
        .emit_load_field(outer_context_reg, self_reg, idx, location);

    let outer_contexts = g.analysis.outer_contexts();
    assert!(context_id.0 < outer_contexts.len());

    for outer_context_class in outer_contexts.iter().skip(context_id.0 + 1).rev() {
        if outer_context_class.has_class_id() {
            let outer_cls_id = outer_context_class.class_id();
            let bc_class_id = g.emitter.convert_class_id(g.sa, outer_cls_id);
            let bc_type_params = g.convert_tya(&g.identity_type_params());
            let idx = g
                .builder
                .add_const_field_types(bc_class_id, bc_type_params, 0);
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

    let ty: BytecodeType = g.emitter.convert_ty_reg(g.sa, field.ty());
    let dest = g.alloc_temp(ty);

    let bc_class_id = g.emitter.convert_class_id(g.sa, outer_cls_id);
    let bc_type_params = g.convert_tya(&g.identity_type_params());
    let idx = g
        .builder
        .add_const_field_types(bc_class_id, bc_type_params, field_index.0 as u32);
    g.builder
        .emit_load_field(dest, outer_context_reg, idx, location);

    g.free_temp(outer_context_reg);

    dest
}
