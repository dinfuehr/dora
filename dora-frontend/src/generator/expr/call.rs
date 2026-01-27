use dora_bytecode::{BytecodeType, ConstPoolIdx, Location, Register};

use super::bin::gen_intrinsic_bin;
use super::{add_const_pool_entry_for_call, ensure_register, gen_expr, specialize_type_for_call};
use crate::generator::{AstBytecodeGen, DataDest, IntrinsicInfo};
use crate::sema::{
    CallExpr, CallType, ClassDefinitionId, ExprId, FctDefinition, Intrinsic, StructDefinitionId,
    emit_as_bytecode_operation,
};
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr_call(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    dest: DataDest,
) -> Register {
    if let Some(info) = g.get_intrinsic(expr_id) {
        if emit_as_bytecode_operation(info.intrinsic) {
            return gen_expr_call_intrinsic(g, expr_id, e, info, dest);
        }
    }

    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");

    match *call_type {
        CallType::NewEnum(ref enum_ty, variant_idx) => {
            return gen_expr_call_enum(g, expr_id, e, enum_ty.clone(), variant_idx, dest);
        }

        CallType::NewStruct(struct_id, ref type_params) => {
            return gen_expr_call_struct(g, expr_id, e, struct_id, type_params, dest);
        }

        CallType::NewClass(cls_id, ref type_params) => {
            return gen_expr_call_class(g, expr_id, e, cls_id, type_params, dest);
        }

        CallType::Lambda(ref params, ref return_type) => {
            return gen_expr_call_lambda(g, expr_id, e, params.clone(), return_type.clone(), dest);
        }

        CallType::Expr(..)
        | CallType::Method(..)
        | CallType::GenericMethod(..)
        | CallType::GenericStaticMethod(..)
        | CallType::GenericMethodSelf(..)
        | CallType::GenericMethodNew { .. }
        | CallType::GenericStaticMethodSelf(..)
        | CallType::TraitObjectMethod(..)
        | CallType::Fct(..) => {}

        _ => panic!("unknown call type = {:?}", call_type),
    }

    // Find method that is called
    let callee_id = call_type.fct_id().expect("FctId missing");
    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    // Determine types for arguments and return values
    let (arg_types, _return_type) = determine_callee_types(g, &call_type, &*callee);
    let return_type = g.analysis.ty(expr_id);

    // Allocate register for result
    let return_ty = g.emitter.convert_ty_reg(g.sa, return_type.clone());
    let return_reg = ensure_register(g, dest, return_ty);

    // Evaluate object/self argument
    let object_argument = emit_call_object_argument(g, e, &call_type);

    // Evaluate function arguments
    let arguments = emit_call_arguments(g, e, &*callee, &call_type, &arg_types);

    if let Some(obj_reg) = object_argument {
        g.builder.emit_push_register(obj_reg);
    }
    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
    emit_call_inst(
        g,
        return_reg,
        callee_idx,
        &call_type,
        g.loc_for_expr(expr_id),
    );

    if let Some(obj_reg) = object_argument {
        g.free_if_temp(obj_reg);
    }

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    return_reg
}

fn gen_expr_call_enum(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    enum_ty: SourceType,
    variant_idx: u32,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();

    for arg in &e.args {
        arguments.push(gen_expr(g, arg.expr, DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let (enum_id, type_params) = enum_ty.to_enum().expect("enum expected");

    let bc_enum_id = g.emitter.convert_enum_id(g.sa, enum_id);
    let bc_type_params = g.convert_tya(&type_params);
    let idx = g
        .builder
        .add_const_enum_variant(bc_enum_id, bc_type_params, variant_idx);
    let bytecode_ty = g.emitter.convert_ty_reg(g.sa, enum_ty);
    let dest_reg = ensure_register(g, dest, bytecode_ty);
    g.builder
        .emit_new_enum(dest_reg, idx, g.loc_for_expr(expr_id));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_lambda(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    params: SourceTypeArray,
    return_type: SourceType,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();

    let lambda_object = gen_expr(g, e.callee, DataDest::Alloc);
    arguments.push(lambda_object);

    for arg in &e.args {
        arguments.push(gen_expr(g, arg.expr, DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let bc_params = g.convert_tya(&params);
    let bc_return_type = g.emitter.convert_ty(g.sa, return_type.clone());
    let idx = g.builder.add_const_lambda(bc_params, bc_return_type);

    let location = g.loc_for_expr(expr_id);
    let dest_reg = if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_lambda(dest, idx, location);
        dest
    } else {
        let bytecode_ty = g.emitter.convert_ty_reg(g.sa, return_type);
        let dest_reg = ensure_register(g, dest, bytecode_ty);
        g.builder.emit_invoke_lambda(dest_reg, idx, location);
        dest_reg
    };

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_struct(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    struct_id: StructDefinitionId,
    type_params: &SourceTypeArray,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();

    for arg in &e.args {
        arguments.push(gen_expr(g, arg.expr, DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let struct_id = g.emitter.convert_struct_id(g.sa, struct_id);
    let bc_type_params = g.convert_tya(&type_params);

    let idx = g
        .builder
        .add_const_struct(struct_id, bc_type_params.clone());
    let bytecode_ty = BytecodeType::Struct(struct_id, bc_type_params);
    let dest_reg = ensure_register(g, dest, bytecode_ty);
    g.builder
        .emit_new_struct(dest_reg, idx, g.loc_for_expr(expr_id));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_class(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    cls_id: ClassDefinitionId,
    type_params: &SourceTypeArray,
    dest: DataDest,
) -> Register {
    let mut arguments: Vec<Option<Register>> = vec![None; e.args.len()];

    for arg in &e.args {
        let reg = gen_expr(g, arg.expr, DataDest::Alloc);
        let target_idx = g
            .analysis
            .get_argument(arg.expr)
            .expect("missing argument idx");

        arguments[target_idx] = Some(reg);
    }

    for &arg_reg in &arguments {
        let arg_reg = arg_reg.expect("missing register");
        g.builder.emit_push_register(arg_reg);
    }

    let cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(type_params);
    let idx = g.builder.add_const_cls_types(cls_id, bc_type_params);
    let dest_reg = ensure_register(g, dest, BytecodeType::Ptr);
    g.builder
        .emit_new_object_initialized(dest_reg, idx, g.loc_for_expr(expr_id));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg.expect("missing register"));
    }

    dest_reg
}

fn gen_expr_call_intrinsic(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    info: IntrinsicInfo,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;
    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");

    let location = g.loc_for_expr(expr_id);

    if call_type.is_method() {
        // For method calls, the callee is the object
        let object = e.callee;

        match e.args.len() {
            0 => emit_intrinsic_un_id(g, object, info, location, dest),
            1 => emit_intrinsic_bin(g, object, e.args[0].expr, info, location, dest),
            2 => {
                assert_eq!(intrinsic, Intrinsic::ArraySet);
                emit_intrinsic_array_set(g, object, e.args[0].expr, e.args[1].expr, location, dest)
            }
            _ => unreachable!(),
        }
    } else {
        match intrinsic {
            Intrinsic::Assert => gen_expr_assert(g, expr_id, e, dest),

            Intrinsic::ArrayGet => {
                emit_intrinsic_array_get(g, e.callee, e.args[0].expr, location, dest)
            }

            Intrinsic::ArrayNewOfSize => emit_intrinsic_new_array(g, expr_id, e, dest),

            Intrinsic::ArrayWithValues => {
                let ty = g.ty(expr_id);

                let (cls_id, type_params) = ty.to_class().expect("class expected");
                assert_eq!(cls_id, g.sa.known.classes.array());
                assert_eq!(1, type_params.len());
                let element_ty = type_params[0].clone();
                emit_array_with_variadic_arguments_hir(g, expr_id, e, &[element_ty], 0, dest)
            }

            _ => panic!("unimplemented intrinsic {:?}", intrinsic),
        }
    }
}

pub(super) fn gen_expr_assert(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    _dest: DataDest,
) -> Register {
    let assert_reg = gen_expr(g, e.args[0].expr, DataDest::Alloc);
    g.builder.emit_push_register(assert_reg);
    let fid = g.sa.known.functions.assert();
    let idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(g.sa, fid));
    let dest = g.ensure_unit_register();
    g.builder
        .emit_invoke_static(dest, idx, g.loc_for_expr(expr_id));
    g.free_if_temp(assert_reg);
    dest
}

pub(super) fn determine_callee_types(
    g: &mut AstBytecodeGen,
    call_type: &CallType,
    fct: &FctDefinition,
) -> (Vec<SourceType>, SourceType) {
    let return_type = specialize_type_for_call(g, &call_type, fct.return_type());

    let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

    if fct.has_hidden_self_argument() {
        let self_type = match call_type {
            CallType::TraitObjectMethod(trait_ty, _) => {
                assert!(fct.params_with_self()[0].ty().is_self() && !fct.is_static);
                trait_ty.clone()
            }
            _ => {
                let arg = fct.params_with_self()[0].ty().clone();
                specialize_type_for_call(g, &call_type, arg.clone())
            }
        };

        arg_types.push(self_type);
    }

    for arg in fct.params_without_self() {
        let arg = specialize_type_for_call(g, &call_type, arg.ty());
        arg_types.push(arg);
    }

    (arg_types, return_type)
}

pub(super) fn emit_call_object_argument(
    g: &mut AstBytecodeGen,
    e: &CallExpr,
    call_type: &CallType,
) -> Option<Register> {
    match *call_type {
        CallType::Method(..)
        | CallType::GenericMethod(..)
        | CallType::GenericMethodSelf(..)
        | CallType::GenericMethodNew { .. }
        | CallType::TraitObjectMethod(..) => {
            // For method calls, the callee is either a path to the method (obj.method)
            // or the object itself if there's an explicit object
            // In HIR, the callee field holds the object for method calls
            let reg = gen_expr(g, e.callee, DataDest::Alloc);
            Some(reg)
        }
        CallType::Expr(_, _, _) => Some(gen_expr(g, e.callee, DataDest::Alloc)),
        CallType::GenericStaticMethod(..)
        | CallType::GenericStaticMethodSelf(..)
        | CallType::Fct(..) => None,
        _ => panic!("unexpected call type {:?}", call_type),
    }
}

pub(super) fn emit_call_arguments(
    g: &mut AstBytecodeGen,
    e: &CallExpr,
    callee: &FctDefinition,
    call_type: &CallType,
    arg_types: &[SourceType],
) -> Vec<Register> {
    let mut registers = Vec::new();

    let arg_start_offset = match *call_type {
        CallType::Expr(..) | CallType::Method(..) | CallType::GenericMethod(..) => 1,
        _ => 0,
    };

    let non_variadic_arguments = if callee.params.is_variadic() {
        arg_types.len() - arg_start_offset - 1
    } else {
        arg_types.len()
    };

    for arg in e.args.iter().take(non_variadic_arguments) {
        let reg = gen_expr(g, arg.expr, DataDest::Alloc);
        registers.push(reg);
    }

    if callee.params.is_variadic() {
        let array_reg = emit_array_with_variadic_arguments(
            g,
            e,
            arg_types,
            non_variadic_arguments,
            DataDest::Alloc,
        );
        registers.push(array_reg);
    }

    registers
}

pub(super) fn emit_array_with_variadic_arguments(
    g: &mut AstBytecodeGen,
    e: &CallExpr,
    arg_types: &[SourceType],
    non_variadic_arguments: usize,
    dest: DataDest,
) -> Register {
    let variadic_arguments = e.args.len() - non_variadic_arguments;

    let element_ty = arg_types.last().cloned().unwrap();
    let ty = g.sa.known.array_ty(element_ty.clone());
    let (cls_id, type_params) = ty.to_class().expect("class expected");
    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&type_params);
    let cls_idx = g.builder.add_const_cls_types(bc_cls_id, bc_type_params);

    let length_reg = g.alloc_temp(BytecodeType::Int64);
    g.builder
        .emit_const_int64(length_reg, variadic_arguments as i64);

    let array_reg = ensure_register(g, dest, BytecodeType::Ptr);
    let location = g.loc_for_expr(e.callee);
    g.builder
        .emit_new_array(array_reg, length_reg, cls_idx, location);

    let index_reg = g.alloc_temp(BytecodeType::Int64);

    for (idx, arg) in e.args.iter().skip(non_variadic_arguments).enumerate() {
        let arg_reg = gen_expr(g, arg.expr, DataDest::Alloc);
        g.builder.emit_const_int64(index_reg, idx as i64);
        g.builder
            .emit_store_array(arg_reg, array_reg, index_reg, location);
        g.free_if_temp(arg_reg);
    }

    g.free_if_temp(index_reg);
    g.free_if_temp(length_reg);

    array_reg
}

fn emit_array_with_variadic_arguments_hir(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    arg_types: &[SourceType],
    non_variadic_arguments: usize,
    dest: DataDest,
) -> Register {
    let variadic_arguments = e.args.len() - non_variadic_arguments;

    let element_ty = arg_types.last().cloned().unwrap();
    let ty = g.sa.known.array_ty(element_ty.clone());
    let (cls_id, type_params) = ty.to_class().expect("class expected");
    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&type_params);
    let cls_idx = g.builder.add_const_cls_types(bc_cls_id, bc_type_params);

    let location = g.loc_for_expr(expr_id);

    let length_reg = g.alloc_temp(BytecodeType::Int64);
    g.builder
        .emit_const_int64(length_reg, variadic_arguments as i64);

    let array_reg = ensure_register(g, dest, BytecodeType::Ptr);
    g.builder
        .emit_new_array(array_reg, length_reg, cls_idx, location);

    let index_reg = g.alloc_temp(BytecodeType::Int64);

    for (idx, arg) in e.args.iter().skip(non_variadic_arguments).enumerate() {
        let arg_reg = gen_expr(g, arg.expr, DataDest::Alloc);
        g.builder.emit_const_int64(index_reg, idx as i64);
        g.builder
            .emit_store_array(arg_reg, array_reg, index_reg, location);
        g.free_if_temp(arg_reg);
    }

    g.free_if_temp(index_reg);
    g.free_if_temp(length_reg);

    array_reg
}

pub(super) fn emit_call_inst(
    g: &mut AstBytecodeGen,
    return_reg: Register,
    callee_idx: ConstPoolIdx,
    call_type: &CallType,
    location: Location,
) {
    match *call_type {
        CallType::Method(..) => g
            .builder
            .emit_invoke_direct(return_reg, callee_idx, location),
        CallType::Fct(..) => g
            .builder
            .emit_invoke_static(return_reg, callee_idx, location),
        CallType::Expr(..) => g
            .builder
            .emit_invoke_direct(return_reg, callee_idx, location),
        CallType::TraitObjectMethod(..) => g
            .builder
            .emit_invoke_virtual(return_reg, callee_idx, location),
        CallType::GenericMethod(..)
        | CallType::GenericMethodSelf(..)
        | CallType::GenericMethodNew { .. } => g
            .builder
            .emit_invoke_generic_direct(return_reg, callee_idx, location),
        CallType::GenericStaticMethod(..) | CallType::GenericStaticMethodSelf(..) => g
            .builder
            .emit_invoke_generic_static(return_reg, callee_idx, location),
        CallType::NewClass(..)
        | CallType::NewStruct(..)
        | CallType::NewEnum(..)
        | CallType::Intrinsic(..)
        | CallType::Lambda(..) => unreachable!(),
    }
}

pub(super) fn emit_intrinsic_new_array(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &CallExpr,
    dest: DataDest,
) -> Register {
    let element_ty = g.ty(expr_id);
    let (cls_id, type_params) = element_ty.to_class().expect("class expected");
    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let bc_type_params = g.convert_tya(&type_params);
    let cls_idx = g.builder.add_const_cls_types(bc_cls_id, bc_type_params);

    let array_reg = ensure_register(g, dest, BytecodeType::Ptr);
    let length_reg = gen_expr(g, e.args[0].expr, DataDest::Alloc);

    g.builder
        .emit_new_array(array_reg, length_reg, cls_idx, g.loc_for_expr(expr_id));

    g.free_if_temp(length_reg);

    array_reg
}

pub(super) fn emit_intrinsic_array_get(
    g: &mut AstBytecodeGen,
    obj: ExprId,
    idx: ExprId,
    location: Location,
    dest: DataDest,
) -> Register {
    let ty = g.ty(obj);
    let ty: BytecodeType = if ty.cls_id() == Some(g.sa.known.classes.string()) {
        BytecodeType::UInt8
    } else {
        let ty = ty.type_params();
        let ty = ty[0].clone();

        g.emitter.convert_ty_reg(g.sa, ty)
    };

    let dest = ensure_register(g, dest, ty.clone());

    let arr = gen_expr(g, obj, DataDest::Alloc);
    let idx = gen_expr(g, idx, DataDest::Alloc);

    g.builder.emit_load_array(dest, arr, idx, location);

    g.free_if_temp(arr);
    g.free_if_temp(idx);

    dest
}

pub(super) fn emit_intrinsic_array_set(
    g: &mut AstBytecodeGen,
    arr: ExprId,
    idx: ExprId,
    src: ExprId,
    location: Location,
    _dest: DataDest,
) -> Register {
    let arr = gen_expr(g, arr, DataDest::Alloc);
    let idx = gen_expr(g, idx, DataDest::Alloc);
    let src = gen_expr(g, src, DataDest::Alloc);

    g.builder.emit_store_array(src, arr, idx, location);

    g.free_if_temp(arr);
    g.free_if_temp(idx);
    g.free_if_temp(src);

    g.ensure_unit_register()
}

/// HIR-based version with ExprId for the operand
pub(super) fn emit_intrinsic_un_id(
    g: &mut AstBytecodeGen,
    opnd_id: crate::sema::ExprId,
    info: IntrinsicInfo,
    location: Location,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;

    let fct = g.sa.fct(info.fct_id.expect("missing method"));
    let ty = g.emitter.convert_ty(g.sa, fct.return_type());
    let dest = ensure_register(g, dest, ty);

    let src = gen_expr(g, opnd_id, DataDest::Alloc);

    emit_intrinsic_un_impl(g, intrinsic, src, dest, location);
    dest
}

fn emit_intrinsic_un_impl(
    g: &mut AstBytecodeGen,
    intrinsic: Intrinsic,
    src: Register,
    dest: Register,
    location: Location,
) {
    match intrinsic {
        Intrinsic::ArrayLen | Intrinsic::StrLen => {
            g.builder.emit_array_length(dest, src, location);
        }
        Intrinsic::Int32Neg
        | Intrinsic::Int64Neg
        | Intrinsic::Float32Neg
        | Intrinsic::Float64Neg => g.builder.emit_neg(dest, src, location),
        Intrinsic::BoolNot | Intrinsic::Int32Not | Intrinsic::Int64Not => {
            g.builder.emit_not(dest, src)
        }
        Intrinsic::Float32IsNan => g.builder.emit_test_ne(dest, src, src),
        Intrinsic::Float64IsNan => g.builder.emit_test_ne(dest, src, src),
        _ => {
            panic!("unimplemented intrinsic {:?}", intrinsic);
        }
    }

    g.free_if_temp(src);
}

pub(super) fn emit_intrinsic_bin(
    g: &mut AstBytecodeGen,
    lhs: ExprId,
    rhs: ExprId,
    info: IntrinsicInfo,
    location: Location,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;

    match intrinsic {
        Intrinsic::ArrayGet | Intrinsic::StrGet => {
            return emit_intrinsic_array_get(g, lhs, rhs, location, dest);
        }

        _ => {}
    }

    let fct_id = info.fct_id.expect("missing function");
    let fct = g.sa.fct(fct_id);

    let result_type = g.emitter.convert_ty(g.sa, fct.return_type());

    let dest = ensure_register(g, dest, result_type);

    let lhs_reg = gen_expr(g, lhs, DataDest::Alloc);
    let rhs_reg = gen_expr(g, rhs, DataDest::Alloc);

    gen_intrinsic_bin(g, intrinsic, dest, lhs_reg, rhs_reg, location);

    g.free_if_temp(lhs_reg);
    g.free_if_temp(rhs_reg);

    dest
}
