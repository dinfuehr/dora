use dora_bytecode::{BytecodeType, ConstPoolIdx, Location, Register};
use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

use super::bin::gen_intrinsic_bin;
use super::{add_const_pool_entry_for_call, ensure_register, gen_expr, specialize_type_for_call};
use crate::generator::{AstBytecodeGen, DataDest, IntrinsicInfo};
use crate::sema::{
    CallType, ClassDefinitionId, FctDefinition, Intrinsic, StructDefinitionId,
    emit_as_bytecode_operation,
};
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr_call(
    g: &mut AstBytecodeGen,
    node: ast::AstCallExpr,
    dest: DataDest,
) -> Register {
    let node_id = node.id();
    if let Some(info) = g.get_intrinsic(node_id) {
        if emit_as_bytecode_operation(info.intrinsic) {
            return gen_expr_call_intrinsic(g, node.clone(), info, dest);
        }
    }

    let call_type = g.analysis.get_call_type(node_id).expect("missing CallType");

    match *call_type {
        CallType::NewEnum(ref enum_ty, variant_idx) => {
            return gen_expr_call_enum(g, node.clone(), enum_ty.clone(), variant_idx, dest);
        }

        CallType::NewStruct(struct_id, ref type_params) => {
            return gen_expr_call_struct(g, node.clone(), struct_id, type_params, dest);
        }

        CallType::NewClass(cls_id, ref type_params) => {
            return gen_expr_call_class(g, node.clone(), cls_id, type_params, dest);
        }

        CallType::Lambda(ref params, ref return_type) => {
            return gen_expr_call_lambda(
                g,
                node.clone(),
                params.clone(),
                return_type.clone(),
                dest,
            );
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
    let return_type = g.analysis.ty(node_id);

    // Allocate register for result
    let return_reg = ensure_register(g, dest, g.emitter.convert_ty_reg(return_type.clone()));

    // Evaluate object/self argument
    let object_argument = emit_call_object_argument(g, node.clone(), &call_type);

    // Evaluate function arguments
    let arguments = emit_call_arguments(g, node.clone(), &*callee, &call_type, &arg_types);

    if let Some(obj_reg) = object_argument {
        g.builder.emit_push_register(obj_reg);
    }
    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
    emit_call_inst(g, return_reg, callee_idx, &call_type, g.loc(node.span()));

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
    expr: ast::AstCallExpr,
    enum_ty: SourceType,
    variant_idx: u32,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();
    let argument_list = expr.arg_list();

    for arg in argument_list.items() {
        arguments.push(gen_expr(g, arg.expr().unwrap(), DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let (enum_id, type_params) = enum_ty.to_enum().expect("enum expected");

    let idx = g.builder.add_const_enum_variant(
        g.emitter.convert_enum_id(enum_id),
        g.convert_tya(&type_params),
        variant_idx,
    );
    let bytecode_ty = g.emitter.convert_ty_reg(enum_ty);
    let dest_reg = ensure_register(g, dest, bytecode_ty);
    g.builder.emit_new_enum(dest_reg, idx, g.loc(expr.span()));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_lambda(
    g: &mut AstBytecodeGen,
    node: ast::AstCallExpr,
    params: SourceTypeArray,
    return_type: SourceType,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();

    let lambda_object = gen_expr(g, node.callee(), DataDest::Alloc);
    arguments.push(lambda_object);

    let argument_list = node.arg_list();

    for arg in argument_list.items() {
        arguments.push(gen_expr(g, arg.expr().unwrap(), DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let idx = g.builder.add_const_lambda(
        g.convert_tya(&params),
        g.emitter.convert_ty(return_type.clone()),
    );

    let dest_reg = if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder.emit_invoke_lambda(dest, idx, g.loc(node.span()));
        dest
    } else {
        let bytecode_ty = g.emitter.convert_ty_reg(return_type);
        let dest_reg = ensure_register(g, dest, bytecode_ty);
        g.builder
            .emit_invoke_lambda(dest_reg, idx, g.loc(node.span()));
        dest_reg
    };

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_struct(
    g: &mut AstBytecodeGen,
    expr: ast::AstCallExpr,
    struct_id: StructDefinitionId,
    type_params: &SourceTypeArray,
    dest: DataDest,
) -> Register {
    let mut arguments = Vec::new();
    let argument_list = expr.arg_list();

    for arg in argument_list.items() {
        arguments.push(gen_expr(g, arg.expr().unwrap(), DataDest::Alloc));
    }

    for &arg_reg in &arguments {
        g.builder.emit_push_register(arg_reg);
    }

    let struct_id = g.emitter.convert_struct_id(struct_id);

    let idx = g
        .builder
        .add_const_struct(struct_id, g.convert_tya(&type_params));
    let bytecode_ty = BytecodeType::Struct(struct_id, g.convert_tya(type_params));
    let dest_reg = ensure_register(g, dest, bytecode_ty);
    g.builder.emit_new_struct(dest_reg, idx, g.loc(expr.span()));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg);
    }

    dest_reg
}

fn gen_expr_call_class(
    g: &mut AstBytecodeGen,
    node: ast::AstCallExpr,
    cls_id: ClassDefinitionId,
    type_params: &SourceTypeArray,
    dest: DataDest,
) -> Register {
    let argument_list = node.arg_list();
    let mut arguments: Vec<Option<Register>> = vec![None; argument_list.items().count()];

    for arg in argument_list.items() {
        let reg = gen_expr(g, arg.expr().unwrap(), DataDest::Alloc);
        let target_idx = g
            .analysis
            .get_argument(arg.id())
            .expect("missing argument idx");

        arguments[target_idx] = Some(reg);
    }

    for &arg_reg in &arguments {
        let arg_reg = arg_reg.expect("missing register");
        g.builder.emit_push_register(arg_reg);
    }

    let cls_id = g.emitter.convert_class_id(cls_id);
    let idx = g
        .builder
        .add_const_cls_types(cls_id, g.convert_tya(type_params));
    let dest_reg = ensure_register(g, dest, BytecodeType::Ptr);
    g.builder
        .emit_new_object_initialized(dest_reg, idx, g.loc(node.span()));

    for arg_reg in arguments {
        g.free_if_temp(arg_reg.expect("missing register"));
    }

    dest_reg
}

fn gen_expr_call_intrinsic(
    g: &mut AstBytecodeGen,
    node: ast::AstCallExpr,
    info: IntrinsicInfo,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;
    let call_type = g
        .analysis
        .get_call_type(node.id())
        .expect("missing CallType");

    let argument_list = node.arg_list();

    if call_type.is_method() {
        let object = node.object().unwrap();

        let mut args = argument_list.items();
        match argument_list.items().count() {
            0 => emit_intrinsic_un(g, object, info, g.loc(node.span()), dest),
            1 => emit_intrinsic_bin(
                g,
                object,
                args.next().expect("argument expected").expr().unwrap(),
                info,
                g.loc(node.span()),
                dest,
            ),
            2 => {
                assert_eq!(intrinsic, Intrinsic::ArraySet);
                let first = args.next().expect("argument expected").expr().unwrap();
                let second = args.next().expect("argument expected").expr().unwrap();
                emit_intrinsic_array_set(
                    g,
                    node.object().unwrap(),
                    first,
                    second,
                    g.loc(node.span()),
                    dest,
                )
            }
            _ => unreachable!(),
        }
    } else {
        match intrinsic {
            Intrinsic::Assert => gen_expr_assert(g, node.clone(), dest),

            Intrinsic::ArrayGet => emit_intrinsic_array_get(
                g,
                node.callee(),
                argument_list
                    .items()
                    .next()
                    .expect("argument expected")
                    .expr()
                    .unwrap(),
                g.loc(node.span()),
                dest,
            ),

            Intrinsic::ArrayNewOfSize => emit_intrinsic_new_array(g, node.clone(), dest),

            Intrinsic::ArrayWithValues => {
                let ty = g.ty(node.id());

                let (cls_id, type_params) = ty.to_class().expect("class expected");
                assert_eq!(cls_id, g.sa.known.classes.array());
                assert_eq!(1, type_params.len());
                let element_ty = type_params[0].clone();
                emit_array_with_variadic_arguments(g, node.clone(), &[element_ty], 0, dest)
            }

            _ => panic!("unimplemented intrinsic {:?}", intrinsic),
        }
    }
}

pub(super) fn gen_expr_assert(
    g: &mut AstBytecodeGen,
    expr: ast::AstCallExpr,
    _dest: DataDest,
) -> Register {
    let argument_list = expr.arg_list();
    let arg = argument_list.items().next().expect("missing argument");
    let assert_reg = gen_expr(g, arg.expr().unwrap(), DataDest::Alloc);
    g.builder.emit_push_register(assert_reg);
    let fid = g.sa.known.functions.assert();
    let idx = g.builder.add_const_fct(g.emitter.convert_function_id(fid));
    let dest = g.ensure_unit_register();
    g.builder.emit_invoke_static(dest, idx, g.loc(expr.span()));
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
    expr: ast::AstCallExpr,
    call_type: &CallType,
) -> Option<Register> {
    match *call_type {
        CallType::Method(..)
        | CallType::GenericMethod(..)
        | CallType::GenericMethodSelf(..)
        | CallType::GenericMethodNew { .. }
        | CallType::TraitObjectMethod(..) => {
            let obj_expr = expr.object().unwrap_or_else(|| expr.callee());
            let reg = gen_expr(g, obj_expr, DataDest::Alloc);

            Some(reg)
        }
        CallType::Expr(_, _, _) => Some(gen_expr(g, expr.callee(), DataDest::Alloc)),
        CallType::GenericStaticMethod(..)
        | CallType::GenericStaticMethodSelf(..)
        | CallType::Fct(..) => None,
        _ => panic!("unexpected call type {:?}", call_type),
    }
}

pub(super) fn emit_call_arguments(
    g: &mut AstBytecodeGen,
    expr: ast::AstCallExpr,
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

    let argument_list = expr.arg_list();

    for arg in argument_list.items().take(non_variadic_arguments) {
        let reg = gen_expr(g, arg.expr().unwrap(), DataDest::Alloc);
        registers.push(reg);
    }

    if callee.params.is_variadic() {
        let array_reg = emit_array_with_variadic_arguments(
            g,
            expr.clone(),
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
    expr: ast::AstCallExpr,
    arg_types: &[SourceType],
    non_variadic_arguments: usize,
    dest: DataDest,
) -> Register {
    let argument_list = expr.arg_list();
    let variadic_arguments = argument_list.items().count() - non_variadic_arguments;

    let element_ty = arg_types.last().cloned().unwrap();
    let ty = g.sa.known.array_ty(element_ty.clone());
    let (cls_id, type_params) = ty.to_class().expect("class expected");
    let cls_idx = g.builder.add_const_cls_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&type_params),
    );

    let length_reg = g.alloc_temp(BytecodeType::Int64);
    g.builder
        .emit_const_int64(length_reg, variadic_arguments as i64);

    let array_reg = ensure_register(g, dest, BytecodeType::Ptr);
    g.builder
        .emit_new_array(array_reg, length_reg, cls_idx, g.loc(expr.span()));

    let index_reg = g.alloc_temp(BytecodeType::Int64);

    for (idx, arg) in argument_list
        .items()
        .skip(non_variadic_arguments)
        .enumerate()
    {
        let arg_reg = gen_expr(g, arg.expr().unwrap(), DataDest::Alloc);
        g.builder.emit_const_int64(index_reg, idx as i64);
        g.builder
            .emit_store_array(arg_reg, array_reg, index_reg, g.loc(expr.span()));
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
    call: ast::AstCallExpr,
    dest: DataDest,
) -> Register {
    let node_id = call.id();
    let element_ty = g.ty(node_id);
    let (cls_id, type_params) = element_ty.to_class().expect("class expected");
    let cls_idx = g.builder.add_const_cls_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&type_params),
    );
    let argument_list = call.arg_list();

    let array_reg = ensure_register(g, dest, BytecodeType::Ptr);
    let arg0 = argument_list.items().next().expect("argument expected");
    let length_reg = gen_expr(g, arg0.expr().unwrap(), DataDest::Alloc);

    g.builder
        .emit_new_array(array_reg, length_reg, cls_idx, g.loc(call.span()));

    g.free_if_temp(length_reg);

    array_reg
}

pub(super) fn emit_intrinsic_array_get(
    g: &mut AstBytecodeGen,
    obj: AstExpr,
    idx: AstExpr,
    location: Location,
    dest: DataDest,
) -> Register {
    let ty = g.ty(obj.id());
    let ty: BytecodeType = if ty.cls_id() == Some(g.sa.known.classes.string()) {
        BytecodeType::UInt8
    } else {
        let ty = ty.type_params();
        let ty = ty[0].clone();

        g.emitter.convert_ty_reg(ty)
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
    arr: AstExpr,
    idx: AstExpr,
    src: AstExpr,
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

pub(super) fn emit_intrinsic_un(
    g: &mut AstBytecodeGen,
    opnd: ast::AstExpr,
    info: IntrinsicInfo,
    location: Location,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;

    let fct = g.sa.fct(info.fct_id.expect("missing method"));
    let ty = g.emitter.convert_ty(fct.return_type());
    let dest = ensure_register(g, dest, ty);

    let src = gen_expr(g, opnd, DataDest::Alloc);

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

    dest
}

pub(super) fn emit_intrinsic_bin(
    g: &mut AstBytecodeGen,
    lhs: AstExpr,
    rhs: AstExpr,
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

    let result_type = g.emitter.convert_ty(fct.return_type());

    let dest = ensure_register(g, dest, result_type);

    let lhs_reg = gen_expr(g, lhs, DataDest::Alloc);
    let rhs_reg = gen_expr(g, rhs, DataDest::Alloc);

    gen_intrinsic_bin(g, intrinsic, dest, lhs_reg, rhs_reg, location);

    g.free_if_temp(lhs_reg);
    g.free_if_temp(rhs_reg);

    dest
}
