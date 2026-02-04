use dora_bytecode::{BytecodeType, Register};

use super::bin::gen_intrinsic_bin;
use super::call::{
    emit_call_inst, emit_intrinsic_array_set, emit_intrinsic_bin, emit_intrinsic_un_id,
};
use super::{add_const_pool_entry_for_call, ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest, IntrinsicInfo};
use crate::sema::{
    CallType, ExprId, IdentType, Intrinsic, MethodCallExpr, emit_as_bytecode_operation,
};
use crate::specialize::specialize_type;
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr_method_call(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MethodCallExpr,
    dest: DataDest,
) -> Register {
    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");

    // Handle lambda field calls
    if let CallType::Lambda(ref params, ref return_type) = *call_type {
        return gen_expr_method_call_lambda(
            g,
            expr_id,
            e,
            params.clone(),
            return_type.clone(),
            dest,
        );
    }

    // Check for intrinsics
    if let Some(info) = g.get_intrinsic(expr_id) {
        if emit_as_bytecode_operation(info.intrinsic) {
            // For field calls (CallType::Expr), load the field first then apply intrinsic
            if matches!(*call_type, CallType::Expr(..)) {
                return gen_expr_method_call_field_intrinsic(g, expr_id, e, info, dest);
            } else {
                return gen_expr_method_call_intrinsic(g, expr_id, e, info, dest);
            }
        }
    }

    // Find method that is called
    let callee_id = call_type.fct_id().expect("FctId missing");
    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let return_type = g.analysis.ty(expr_id);

    // Allocate register for result
    let return_ty = g.emitter.convert_ty_reg(g.sa, return_type.clone());
    let return_reg = ensure_register(g, dest, return_ty);

    // Evaluate object/self argument
    // For CallType::Expr (calling a field), we need to load the field value first
    let object_reg = if matches!(*call_type, CallType::Expr(..)) {
        gen_expr_method_call_field_object(g, expr_id, e)
    } else {
        gen_expr(g, e.object, DataDest::Alloc)
    };

    // Evaluate function arguments
    let mut arguments = emit_method_call_arguments(g, e);
    arguments.insert(0, object_reg);

    // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
    emit_call_inst(
        g,
        return_reg,
        callee_idx,
        &call_type,
        &arguments,
        g.loc_for_expr(expr_id),
    );

    g.free_if_temp(object_reg);
    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    return_reg
}

fn gen_expr_method_call_field_object(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MethodCallExpr,
) -> Register {
    let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");

    match ident_type {
        IdentType::Field(cls_ty, field_index) => {
            let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

            // Get the field type from the class definition
            let cls = g.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let field = g.sa.field(field_id);
            let field_ty = specialize_type(g.sa, field.ty(), &type_params);

            let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
            let bc_type_params = g.convert_tya(&type_params);
            let field_idx =
                g.builder
                    .add_const_field_types(bc_cls_id, bc_type_params, field_index.0 as u32);

            let obj_reg = gen_expr(g, e.object, DataDest::Alloc);
            let field_ty = g.emitter.convert_ty_reg(g.sa, field_ty);
            let field_reg = g.alloc_temp(field_ty);
            g.builder
                .emit_load_field(field_reg, obj_reg, field_idx, g.loc_for_expr(e.object));
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

            let obj_reg = gen_expr(g, e.object, DataDest::Alloc);
            let field_ty = g.emitter.convert_ty_reg(g.sa, field_ty);
            let field_reg = g.alloc_temp(field_ty);
            g.builder
                .emit_load_field(field_reg, obj_reg, field_idx, g.loc_for_expr(e.object));
            g.free_if_temp(obj_reg);
            field_reg
        }
        _ => unreachable!(),
    }
}

fn gen_expr_method_call_field_intrinsic(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MethodCallExpr,
    info: IntrinsicInfo,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;

    // First load the field value
    let field_reg = gen_expr_method_call_field_object(g, expr_id, e);

    match e.args.len() {
        0 => {
            // Unary intrinsic on field (e.g., array.size())
            let fct_id = info.fct_id.expect("missing function");
            let fct = g.sa.fct(fct_id);
            let result_type = g.emitter.convert_ty(g.sa, fct.return_type());
            let dest_reg = ensure_register(g, dest, result_type);
            let location = g.loc_for_expr(expr_id);

            match intrinsic {
                Intrinsic::ArrayLen | Intrinsic::StrLen => {
                    g.builder.emit_array_length(dest_reg, field_reg, location);
                }
                _ => {
                    // Other unary intrinsics not expected on fields
                    unimplemented!("unary intrinsic {:?} on field", intrinsic);
                }
            }

            g.free_if_temp(field_reg);
            dest_reg
        }
        1 => {
            // Binary intrinsic on field (e.g., array.get(idx))
            let idx_reg = gen_expr(g, e.args[0].expr, DataDest::Alloc);

            let location = g.loc_for_expr(expr_id);

            match intrinsic {
                Intrinsic::ArrayGet | Intrinsic::StrGet => {
                    // Get element type from the array
                    let ident_type = g.analysis.get_ident(expr_id).expect("missing ident");
                    let field_ty = match ident_type {
                        IdentType::Field(cls_ty, field_index) => {
                            let (cls_id, type_params) = cls_ty.to_class().expect("class");
                            let cls = g.sa.class(cls_id);
                            let field_id = cls.field_id(field_index);
                            let field = g.sa.field(field_id);
                            specialize_type(g.sa, field.ty(), &type_params)
                        }
                        IdentType::StructField(struct_ty, field_index) => {
                            let (struct_id, type_params) = struct_ty.to_struct().expect("struct");
                            let struct_ = g.sa.struct_(struct_id);
                            let field_id = struct_.field_id(field_index);
                            let field = g.sa.field(field_id);
                            specialize_type(g.sa, field.ty(), &type_params)
                        }
                        _ => unreachable!(),
                    };

                    let ty: BytecodeType = if field_ty.cls_id() == Some(g.sa.known.classes.string())
                    {
                        BytecodeType::UInt8
                    } else {
                        let element_ty = field_ty.type_params()[0].clone();
                        g.emitter.convert_ty_reg(g.sa, element_ty)
                    };

                    let dest_reg = ensure_register(g, dest, ty);
                    g.builder
                        .emit_load_array(dest_reg, field_reg, idx_reg, location);

                    g.free_if_temp(field_reg);
                    g.free_if_temp(idx_reg);
                    dest_reg
                }
                _ => {
                    // Other binary intrinsics
                    let fct_id = info.fct_id.expect("missing function");
                    let fct = g.sa.fct(fct_id);
                    let result_type = g.emitter.convert_ty(g.sa, fct.return_type());
                    let dest_reg = ensure_register(g, dest, result_type);

                    gen_intrinsic_bin(g, intrinsic, dest_reg, field_reg, idx_reg, location);

                    g.free_if_temp(field_reg);
                    g.free_if_temp(idx_reg);
                    dest_reg
                }
            }
        }
        2 => {
            // ArraySet intrinsic (array.set(idx, value))
            assert_eq!(intrinsic, Intrinsic::ArraySet);

            let idx_reg = gen_expr(g, e.args[0].expr, DataDest::Alloc);
            let val_reg = gen_expr(g, e.args[1].expr, DataDest::Alloc);

            let location = g.loc_for_expr(expr_id);
            g.builder
                .emit_store_array(val_reg, field_reg, idx_reg, location);

            g.free_if_temp(field_reg);
            g.free_if_temp(idx_reg);
            g.free_if_temp(val_reg);

            g.ensure_unit_register()
        }
        _ => unreachable!(),
    }
}

fn gen_expr_method_call_lambda(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MethodCallExpr,
    params: SourceTypeArray,
    return_type: SourceType,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();

    // Load the lambda field value
    let lambda_object = gen_expr_method_call_field_object(g, expr_id, e);
    arguments.push(lambda_object);

    for arg in &e.args {
        arguments.push(gen_expr(g, arg.expr, DataDest::Alloc));
    }

    let bc_params = g.convert_tya(&params);
    let bc_return_type = g.emitter.convert_ty(g.sa, return_type.clone());
    let idx = g.builder.add_const_lambda(bc_params, bc_return_type);

    let location = g.loc_for_expr(expr_id);
    let dest_reg = if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder
            .emit_invoke_lambda(dest, idx, &arguments, location);
        dest
    } else {
        let bytecode_ty = g.emitter.convert_ty_reg(g.sa, return_type);
        let dest_reg = ensure_register(g, dest, bytecode_ty);
        g.builder
            .emit_invoke_lambda(dest_reg, idx, &arguments, location);
        dest_reg
    };

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_method_call_intrinsic(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MethodCallExpr,
    info: IntrinsicInfo,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;
    let location = g.loc_for_expr(expr_id);

    match e.args.len() {
        0 => emit_intrinsic_un_id(g, e.object, info, location, dest),
        1 => emit_intrinsic_bin(g, e.object, e.args[0].expr, info, location, dest),
        2 => {
            assert_eq!(intrinsic, Intrinsic::ArraySet);
            emit_intrinsic_array_set(g, e.object, e.args[0].expr, e.args[1].expr, location, dest)
        }
        _ => unreachable!(),
    }
}

fn emit_method_call_arguments(g: &mut AstBytecodeGen, e: &MethodCallExpr) -> Vec<Register> {
    let mut registers = Vec::new();

    for arg in &e.args {
        let reg = gen_expr(g, arg.expr, DataDest::Alloc);
        registers.push(reg);
    }

    registers
}
