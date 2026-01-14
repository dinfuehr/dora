use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast::{self, SyntaxNodeBase};

use super::{
    add_const_pool_entry_for_call, determine_callee_types, emit_array_with_variadic_arguments,
    emit_call_arguments, emit_call_inst, emit_call_object_argument, emit_intrinsic_array_get,
    emit_intrinsic_array_set, emit_intrinsic_bin, emit_intrinsic_new_array, emit_intrinsic_un,
    ensure_register, gen_expr, gen_expr_assert,
};
use crate::generator::{AstBytecodeGen, DataDest, IntrinsicInfo};
use crate::sema::{
    CallType, ClassDefinitionId, Intrinsic, StructDefinitionId, emit_as_bytecode_operation,
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
