use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ConstPoolEntry, ConstPoolIdx, FunctionId, Location, Register,
};
use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

mod assign;
mod bin;
mod call;
mod field;
mod if_;
mod match_;
mod method_call;
mod path;
mod un;

use self::bin::gen_expr_bin;
use self::call::gen_expr_call;
use self::field::gen_expr_field;
use self::if_::{gen_expr_condition, gen_expr_if};
use self::match_::gen_match;
use self::method_call::gen_expr_method_call;
use self::path::{gen_expr_path, gen_expr_path_context};
use self::un::gen_expr_un;
use super::pattern::{destruct_pattern, destruct_pattern_or_fail, setup_pattern_vars};
use super::{
    AstBytecodeGen, DataDest, IntrinsicInfo, LoopLabels, SELF_VAR_ID, emit_mov,
    field_id_from_context_idx, last_context_register, var_reg,
};
use crate::sema::{
    CallType, ContextFieldId, Element, EnumDefinitionId, FctDefinition, IdentType, Intrinsic,
    OuterContextIdx, ScopeId,
};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: AstExpr, dest: DataDest) -> Register {
    match expr {
        AstExpr::UnExpr(node) => gen_expr_un(g, node, dest),
        AstExpr::AssignExpr(node) => self::assign::gen_expr_assign(g, node, dest),
        AstExpr::BinExpr(node) => gen_expr_bin(g, node, dest),
        AstExpr::FieldExpr(node) => gen_expr_field(g, node, dest),
        AstExpr::BlockExpr(node) => gen_expr_block(g, node, dest),
        AstExpr::IfExpr(node) => gen_expr_if(g, node, dest),
        AstExpr::TemplateExpr(node) => gen_expr_template(g, node, dest),
        AstExpr::LitCharExpr(node) => gen_expr_lit_char(g, node, dest),
        AstExpr::LitIntExpr(node) => gen_expr_lit_int(g, node, dest, false),
        AstExpr::LitFloatExpr(node) => gen_expr_lit_float(g, node, dest),
        AstExpr::LitStrExpr(node) => gen_expr_lit_string(g, node, dest),
        AstExpr::LitBoolExpr(node) => gen_expr_lit_bool(g, node, dest),
        AstExpr::PathExpr(node) => gen_expr_path(g, node, dest),
        AstExpr::CallExpr(node) => gen_expr_call(g, node, dest),
        AstExpr::ThisExpr(node) => gen_expr_self(g, node, dest),
        AstExpr::AsExpr(node) => gen_expr_as(g, node, dest),
        AstExpr::IsExpr(node) => gen_expr_is(g, node, dest),
        AstExpr::TupleExpr(node) => gen_expr_tuple(g, node, dest),
        AstExpr::ParenExpr(node) => gen_expr(g, node.expr().unwrap(), dest),
        AstExpr::MatchExpr(node) => gen_match(g, node, dest),
        AstExpr::LambdaExpr(node) => gen_expr_lambda(g, node, dest),
        AstExpr::ForExpr(node) => gen_expr_for(g, node, dest),
        AstExpr::WhileExpr(node) => gen_expr_while(g, node, dest),
        AstExpr::BreakExpr(node) => gen_expr_break(g, node, dest),
        AstExpr::ContinueExpr(node) => gen_expr_continue(g, node, dest),
        AstExpr::ReturnExpr(node) => gen_expr_return(g, node, dest),
        AstExpr::MethodCallExpr(node) => gen_expr_method_call(g, node, dest),
        AstExpr::Error(_) => unreachable!(),
    }
}

fn gen_expr_self(g: &mut AstBytecodeGen, expr: ast::AstThisExpr, dest: DataDest) -> Register {
    let expr_id = expr.id();
    if g.is_lambda {
        let ident = g.analysis.get_ident(expr_id).expect("missing ident");
        let (level, context_idx) = match ident {
            IdentType::Context(level, context_idx) => (level, context_idx),
            _ => unreachable!(),
        };
        gen_expr_path_context(g, level, context_idx, dest, g.loc(expr.span()))
    } else {
        let var_reg = var_reg(g, SELF_VAR_ID);

        if dest.is_alloc() {
            return var_reg;
        }

        let dest = dest.reg();

        emit_mov(g, dest, var_reg);

        dest
    }
}

fn gen_expr_lit_char(
    g: &mut AstBytecodeGen,
    node: ast::AstLitCharExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Char);

    let value = g.analysis.const_value(node.id()).to_char();
    g.builder.emit_const_char(dest, value);

    dest
}

pub(super) fn gen_expr_lit_int(
    g: &mut AstBytecodeGen,
    node: ast::AstLitIntExpr,
    dest: DataDest,
    _neg: bool,
) -> Register {
    let node_id = node.id();
    let ty = g.analysis.ty(node_id);
    let value = g.analysis.const_value(node_id);

    let ty = match ty {
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => {
            let dest = ensure_register(g, dest, BytecodeType::Float32);
            g.builder
                .emit_const_float32(dest, value.to_f64().expect("float expected") as f32);
            return dest;
        }
        SourceType::Float64 => {
            let dest = ensure_register(g, dest, BytecodeType::Float64);
            g.builder
                .emit_const_float64(dest, value.to_f64().expect("float expected"));
            return dest;
        }
        _ => unreachable!(),
    };

    let dest = ensure_register(g, dest, ty.clone());
    let value_i64 = value.to_i64().expect("integer expected");

    match ty {
        BytecodeType::UInt8 => g.builder.emit_const_uint8(dest, value_i64 as u8),
        BytecodeType::Int32 => g.builder.emit_const_int32(dest, value_i64 as i32),
        BytecodeType::Int64 => g.builder.emit_const_int64(dest, value_i64),
        _ => unreachable!(),
    }

    dest
}

fn gen_expr_lit_float(
    g: &mut AstBytecodeGen,
    node: ast::AstLitFloatExpr,
    dest: DataDest,
) -> Register {
    let node_id = node.id();
    let ty = g.analysis.ty(node_id);
    let value_f64 = g
        .analysis
        .const_value(node_id)
        .to_f64()
        .expect("float expected");

    let ty = match ty {
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        _ => unreachable!(),
    };

    let dest = ensure_register(g, dest, ty.clone());

    match ty {
        BytecodeType::Float32 => g.builder.emit_const_float32(dest, value_f64 as f32),
        BytecodeType::Float64 => g.builder.emit_const_float64(dest, value_f64),
        _ => unreachable!(),
    }

    dest
}

fn gen_expr_lit_string(
    g: &mut AstBytecodeGen,
    node: ast::AstLitStrExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Ptr);
    let value = g
        .analysis
        .const_value(node.id())
        .to_string()
        .expect("string expected")
        .to_string();
    g.builder.emit_const_string(dest, value);

    dest
}

fn gen_expr_lit_bool(
    g: &mut AstBytecodeGen,
    node: ast::AstLitBoolExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    if node.value() {
        g.builder.emit_const_true(dest);
    } else {
        g.builder.emit_const_false(dest);
    }

    dest
}

fn gen_expr_tuple(g: &mut AstBytecodeGen, e: ast::AstTupleExpr, dest: DataDest) -> Register {
    let node_id = e.id();

    if e.values().count() == 0 {
        return g.ensure_unit_register();
    }

    let ty = g.ty(node_id);

    let result_ty: BytecodeType = g.emitter.convert_ty_reg(ty.clone());
    let result = ensure_register(g, dest, result_ty);

    let mut values = Vec::with_capacity(e.values().count());

    for value in e.values() {
        let value_id = value.id();
        let value_ty = g.ty(value_id);
        let reg = gen_expr(g, value, DataDest::Alloc);

        if !value_ty.is_unit() {
            values.push(reg);
        }
    }

    for &value in &values {
        g.builder.emit_push_register(value);
    }

    let subtypes = ty.tuple_subtypes().expect("tuple expected");
    let idx = g.builder.add_const_tuple(g.convert_tya(&subtypes));
    g.builder.emit_new_tuple(result, idx, g.loc(e.span()));

    for arg_reg in values {
        g.free_if_temp(arg_reg);
    }

    result
}

pub(super) fn gen_intrinsic_bin(
    g: &mut AstBytecodeGen,
    intrinsic: Intrinsic,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    match intrinsic {
        Intrinsic::UInt8Eq
        | Intrinsic::BoolEq
        | Intrinsic::CharEq
        | Intrinsic::Int32Eq
        | Intrinsic::Int64Eq
        | Intrinsic::Float32Eq
        | Intrinsic::Float64Eq => g.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32Add => g.builder.emit_add(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int32Sub => g.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int32Mul => g.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int32Div => g.builder.emit_div(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int32Mod => g.builder.emit_mod(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int32Or => g.builder.emit_or(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32And => g.builder.emit_and(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32Xor => g.builder.emit_xor(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32Shl => g.builder.emit_shl(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32Shr => g.builder.emit_shr(dest, lhs_reg, rhs_reg),
        Intrinsic::Int32Sar => g.builder.emit_sar(dest, lhs_reg, rhs_reg),

        Intrinsic::Int64Add => g.builder.emit_add(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int64Sub => g.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int64Mul => g.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int64Div => g.builder.emit_div(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int64Mod => g.builder.emit_mod(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Int64Or => g.builder.emit_or(dest, lhs_reg, rhs_reg),
        Intrinsic::Int64And => g.builder.emit_and(dest, lhs_reg, rhs_reg),
        Intrinsic::Int64Xor => g.builder.emit_xor(dest, lhs_reg, rhs_reg),
        Intrinsic::Int64Shl => g.builder.emit_shl(dest, lhs_reg, rhs_reg),
        Intrinsic::Int64Shr => g.builder.emit_shr(dest, lhs_reg, rhs_reg),
        Intrinsic::Int64Sar => g.builder.emit_sar(dest, lhs_reg, rhs_reg),

        Intrinsic::Float32Add => g.builder.emit_add(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float32Sub => g.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float32Mul => g.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float32Div => g.builder.emit_div(dest, lhs_reg, rhs_reg, location),

        Intrinsic::Float64Add => g.builder.emit_add(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float64Sub => g.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float64Mul => g.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
        Intrinsic::Float64Div => g.builder.emit_div(dest, lhs_reg, rhs_reg, location),

        _ => unimplemented!(),
    }
}

pub(super) fn gen_method_bin(
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

fn gen_expr_for_effect(g: &mut AstBytecodeGen, expr: ast::AstBlockExpr) {
    let reg = gen_expr(g, expr.into(), DataDest::Alloc);
    g.free_if_temp(reg);
}

fn gen_expr_for(g: &mut AstBytecodeGen, stmt: ast::AstForExpr, _dest: DataDest) -> Register {
    let stmt_ast_id = stmt.id();
    g.push_scope();
    let for_type_info = g
        .analysis
        .get_for_type_info(stmt_ast_id)
        .expect("missing for");

    // Emit: <obj> = <expr> (for <var> in <expr> { ... })
    let object_reg = gen_expr(g, stmt.expr(), DataDest::Alloc);

    let iterator_reg = if let Some((iter_fct_id, iter_type_params)) = for_type_info.iter {
        // Emit: <iterator> = <obj>.iter();
        let iterator_reg = g.alloc_var(BytecodeType::Ptr);
        g.builder.emit_push_register(object_reg);
        let fct_idx = g.builder.add_const_fct_types(
            g.emitter.convert_function_id(iter_fct_id),
            g.convert_tya(&iter_type_params),
        );
        g.builder
            .emit_invoke_direct(iterator_reg, fct_idx, g.loc(stmt.expr().span()));
        iterator_reg
    } else {
        // Object is already the iterator - just use it
        object_reg
    };

    let lbl_cond = g.builder.define_label();
    g.builder.emit_loop_start();

    g.enter_block_context(stmt_ast_id);

    let iterator_type = for_type_info.iterator_type.clone();
    let iterator_type_params = g.convert_tya(&iterator_type.type_params());

    g.builder.emit_push_register(iterator_reg);

    let lbl_end = g.builder.create_label();

    let value_ty = for_type_info.value_type.clone();
    let option_type_params = SourceTypeArray::single(value_ty.clone());

    // Emit: <next-temp> = <iterator>.next()
    let next_result_ty = g.emitter.convert_ty_reg(for_type_info.next_type.clone());
    let next_result_reg = g.alloc_temp(next_result_ty);

    let fct_idx = g.builder.add_const_fct_types(
        FunctionId(
            for_type_info
                .next
                .expect("missing fct id")
                .index()
                .try_into()
                .expect("overflow"),
        ),
        iterator_type_params,
    );

    g.builder.emit_push_register(iterator_reg);
    emit_invoke_direct(
        g,
        for_type_info.next_type.clone(),
        next_result_reg,
        fct_idx,
        g.loc(stmt.expr().span()),
    );

    // Emit: if <next-result>.isNone() then goto lbl_end
    let cond_reg = g.alloc_temp(BytecodeType::Bool);
    let fct_idx = g.builder.add_const_fct_types(
        FunctionId(
            g.sa.known
                .functions
                .option_is_none()
                .index()
                .try_into()
                .expect("overflow"),
        ),
        g.convert_tya(&option_type_params),
    );
    g.builder.emit_push_register(next_result_reg);
    g.builder
        .emit_invoke_direct(cond_reg, fct_idx, g.loc(stmt.expr().span()));
    g.builder.emit_jump_if_true(cond_reg, lbl_end);
    g.free_temp(cond_reg);

    // Emit: <value-reg> = <next-result>.unwrap()
    if value_ty.is_unit() {
        g.free_temp(next_result_reg);
    } else {
        let value_ty = g.emitter.convert_ty_reg(value_ty);
        let value_reg = g.alloc_var(value_ty);
        let fct_idx = g.builder.add_const_fct_types(
            FunctionId(
                g.sa.known
                    .functions
                    .option_unwrap()
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            g.convert_tya(&option_type_params),
        );
        g.builder.emit_push_register(next_result_reg);
        g.builder
            .emit_invoke_direct(value_reg, fct_idx, g.loc(stmt.expr().span()));
        g.free_temp(next_result_reg);

        setup_pattern_vars(g, stmt.pattern());
        destruct_pattern_or_fail(g, stmt.pattern(), value_reg, for_type_info.value_type);
    }

    g.loops.push(LoopLabels::new(lbl_cond, lbl_end));
    gen_expr_for_effect(g, stmt.block());
    g.loops.pop().unwrap();

    g.builder.emit_jump_loop(lbl_cond);
    g.builder.bind_label(lbl_end);

    g.leave_block_context(stmt_ast_id);
    g.pop_scope();

    g.free_if_temp(object_reg);
    g.ensure_unit_register()
}

fn gen_expr_while(g: &mut AstBytecodeGen, node: ast::AstWhileExpr, _dest: DataDest) -> Register {
    let node_id = node.id();
    let cond_lbl = g.builder.define_label();
    let end_lbl = g.builder.create_label();
    g.builder.emit_loop_start();
    g.enter_block_context(node_id);

    gen_expr_condition(g, node.cond(), end_lbl);

    g.loops.push(LoopLabels::new(cond_lbl, end_lbl));
    gen_expr_for_effect(g, node.block());
    g.loops.pop().unwrap();
    g.builder.emit_jump_loop(cond_lbl);
    g.builder.bind_label(end_lbl);
    g.leave_block_context(node_id);
    g.ensure_unit_register()
}

fn gen_expr_return(g: &mut AstBytecodeGen, ret: ast::AstReturnExpr, _dest: DataDest) -> Register {
    let result_reg = if let Some(expr) = ret.expr() {
        gen_expr(g, expr, DataDest::Alloc)
    } else {
        g.ensure_unit_register()
    };

    g.builder.emit_ret(result_reg);
    g.free_if_temp(result_reg);

    g.ensure_unit_register()
}

fn gen_expr_break(g: &mut AstBytecodeGen, _node: ast::AstBreakExpr, _dest: DataDest) -> Register {
    let end = g.loops.last().unwrap().end;
    g.builder.emit_jump(end);
    g.ensure_unit_register()
}

fn gen_expr_continue(
    g: &mut AstBytecodeGen,
    _node: ast::AstContinueExpr,
    _dest: DataDest,
) -> Register {
    let cond = g.loops.last().unwrap().cond;
    g.builder.emit_jump_loop(cond);
    g.ensure_unit_register()
}

pub(super) fn gen_stmt_expr(g: &mut AstBytecodeGen, stmt: ast::AstExprStmt) {
    let reg = gen_expr(g, stmt.expr(), DataDest::Alloc);
    g.free_if_temp(reg);
}

pub(super) fn gen_stmt_let(g: &mut AstBytecodeGen, stmt: ast::AstLet) {
    setup_pattern_vars(g, stmt.pattern());

    if let Some(expr) = stmt.expr() {
        let ty = g.ty(expr.id());
        let value = gen_expr(g, expr, DataDest::Alloc);
        destruct_pattern_or_fail(g, stmt.pattern(), value, ty);
        g.free_if_temp(value);
    }
}

fn gen_expr_template(
    g: &mut AstBytecodeGen,
    expr: ast::AstTemplateExpr,
    dest: DataDest,
) -> Register {
    let buffer_register = ensure_register(g, dest, BytecodeType::Ptr);

    // build StringBuffer::empty() call
    let fct_id = g.sa.known.functions.string_buffer_empty();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(fct_id));
    g.builder
        .emit_invoke_static(buffer_register, fct_idx, g.loc(expr.span()));

    let part_register = g.alloc_temp(BytecodeType::Ptr);

    for part in expr.parts() {
        if let Some(..) = part.clone().to_lit_str_expr() {
            let value = g
                .analysis
                .const_value(part.id())
                .to_string()
                .expect("string expected")
                .to_string();
            g.builder.emit_const_string(part_register, value);
        } else {
            let ty = g.ty(part.id());

            if ty.cls_id() == Some(g.sa.known.classes.string()) {
                gen_expr(g, part, DataDest::Reg(part_register));
            } else if ty.is_type_param() {
                let type_list_id = match ty {
                    SourceType::TypeParam(id) => id,
                    _ => unreachable!(),
                };

                let expr_register = gen_expr(g, part.clone(), DataDest::Alloc);
                g.builder.emit_push_register(expr_register);

                // build toString() call
                let name = g.sa.interner.intern("toString");
                let trait_id = g.sa.known.traits.stringable();
                let trait_ = g.sa.trait_(trait_id);
                let to_string_id = trait_
                    .get_method(name, false)
                    .expect("Stringable::toString() not found");

                let fct_idx = g.builder.add_const(ConstPoolEntry::Generic(
                    type_list_id.index() as u32,
                    g.emitter.convert_function_id(to_string_id),
                    BytecodeTypeArray::empty(),
                    BytecodeTypeArray::empty(),
                ));

                g.builder
                    .emit_invoke_generic_direct(part_register, fct_idx, g.loc(part.span()));

                g.free_if_temp(expr_register);
            } else {
                let expr_register = gen_expr(g, part.clone(), DataDest::Alloc);
                g.builder.emit_push_register(expr_register);

                // build toString() call
                let (to_string_id, type_params) = g
                    .analysis
                    .get_template(part.id())
                    .expect("missing toString id");

                let type_params = g.convert_tya(&type_params);

                let fct_idx = g
                    .builder
                    .add_const_fct_types(g.emitter.convert_function_id(to_string_id), type_params);
                g.builder
                    .emit_invoke_direct(part_register, fct_idx, g.loc(part.span()));

                g.free_if_temp(expr_register);
            }
        }

        // build StringBuffer::append() call
        let fct_id = g.sa.known.functions.string_buffer_append();
        let fct_idx = g
            .builder
            .add_const_fct(g.emitter.convert_function_id(fct_id));
        g.builder.emit_push_register(buffer_register);
        g.builder.emit_push_register(part_register);
        let dest_reg = g.ensure_unit_register();
        g.builder
            .emit_invoke_direct(dest_reg, fct_idx, g.loc(expr.span()));
    }

    g.free_temp(part_register);

    // build StringBuffer::toString() call
    let fct_id = g.sa.known.functions.string_buffer_to_string();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(fct_id));
    g.builder.emit_push_register(buffer_register);
    g.builder
        .emit_invoke_direct(buffer_register, fct_idx, g.loc(expr.span()));

    buffer_register
}

fn gen_expr_as(g: &mut AstBytecodeGen, expr: ast::AstAsExpr, dest: DataDest) -> Register {
    let object_type = g.ty(expr.object().unwrap().id());
    let check_type = g.ty(expr.data_type().unwrap().id());
    assert!(check_type.is_trait_object());

    let check_type = g.emitter.convert_ty(check_type);

    let object = gen_expr(g, expr.object().unwrap(), DataDest::Alloc);
    let idx = g
        .builder
        .add_const_trait(check_type.clone(), g.emitter.convert_ty(object_type));
    let dest = ensure_register(g, dest, check_type);
    g.builder
        .emit_new_trait_object(dest, idx, object, g.loc(expr.span()));
    g.free_if_temp(object);
    dest
}

fn gen_expr_is(g: &mut AstBytecodeGen, node: ast::AstIsExpr, dest: DataDest) -> Register {
    let ty = g.ty(node.value().id());
    let value_reg = gen_expr(g, node.value(), DataDest::Alloc);

    g.push_scope();
    let mismatch_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();
    destruct_pattern(g, node.pattern(), value_reg, ty, Some(mismatch_lbl));
    let dest = ensure_register(g, dest, BytecodeType::Bool);
    g.builder.emit_const_true(dest);
    g.builder.emit_jump(merge_lbl);
    g.builder.bind_label(mismatch_lbl);
    g.builder.emit_const_false(dest);
    g.builder.bind_label(merge_lbl);
    g.pop_scope();

    g.free_if_temp(value_reg);

    dest
}

fn gen_expr_lambda(g: &mut AstBytecodeGen, node: ast::AstLambdaExpr, dest: DataDest) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Ptr);

    let lambda_fct_id = g
        .analysis
        .get_lambda(node.id())
        .expect("missing lambda id")
        .fct_id();

    let lambda_fct = g.sa.fct(lambda_fct_id);
    let lambda_analysis = lambda_fct.analysis();

    let mut outer_context_reg: Option<Register> = None;

    if lambda_analysis.needs_context_slot_in_lambda_object() {
        if let Some(context_register) = last_context_register(g) {
            g.builder.emit_push_register(context_register.clone());
        } else {
            // This lambda doesn't have a context object on its own, simply
            // pass down the parent context (the context in the lambda object).
            assert!(g.is_lambda);
            assert!(g.analysis.needs_context_slot_in_lambda_object());
            outer_context_reg = Some(g.alloc_temp(BytecodeType::Ptr));
            let lambda_cls_id = g.sa.known.classes.lambda();
            let idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(lambda_cls_id),
                BytecodeTypeArray::empty(),
                0,
            );
            g.builder.emit_load_field(
                outer_context_reg.expect("missing reg"),
                var_reg(g, SELF_VAR_ID),
                idx,
                g.loc(node.span()),
            );
            g.builder
                .emit_push_register(outer_context_reg.expect("missing reg"));
        }
    }

    let idx = g.builder.add_const_fct_types(
        g.emitter.convert_function_id(lambda_fct_id),
        g.convert_tya(&g.identity_type_params()),
    );
    g.builder.emit_new_lambda(dest, idx, g.loc(node.span()));

    if let Some(outer_context_reg) = outer_context_reg {
        g.free_if_temp(outer_context_reg);
    }

    dest
}

fn gen_expr_block(g: &mut AstBytecodeGen, block: ast::AstBlockExpr, dest: DataDest) -> Register {
    g.push_scope();

    for stmt in block.stmts_without_tail() {
        g.visit_stmt(stmt);
    }

    let result = if let Some(stmt) = block.tail() {
        let expr_stmt = stmt.as_expr_stmt();
        gen_expr(g, expr_stmt.expr(), dest)
    } else {
        g.ensure_unit_register()
    };

    g.pop_scope();

    result
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

pub(super) fn emit_new_enum(
    g: &mut AstBytecodeGen,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
    location: Location,
    dest: DataDest,
) -> Register {
    let type_params = g.convert_tya(&type_params);
    let enum_id = g.emitter.convert_enum_id(enum_id);
    let bty = BytecodeType::Enum(enum_id, type_params.clone());
    let dest = ensure_register(g, dest, bty);
    let idx = g
        .builder
        .add_const_enum_variant(enum_id, type_params, variant_idx);
    g.builder.emit_new_enum(dest, idx, location);
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

pub(super) fn emit_invoke_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let reg = g.ensure_unit_register();
        g.builder.emit_invoke_direct(reg, callee_id, location);
    } else {
        g.builder
            .emit_invoke_direct(return_reg, callee_id, location);
    }
}

pub(super) fn emit_invoke_generic_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder
            .emit_invoke_generic_direct(dest, callee_id, location);
    } else {
        g.builder
            .emit_invoke_generic_direct(return_reg, callee_id, location);
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
pub(super) fn load_from_context(
    g: &mut AstBytecodeGen,
    dest: Register,
    scope_id: ScopeId,
    field_id: ContextFieldId,
    location: Location,
) {
    let entered_context = &g.entered_contexts[scope_id.0];
    let context_register = entered_context.register.expect("missing register");
    let context_data = entered_context.context_data.clone();
    let cls_id = context_data.class_id();
    let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
    let field_idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&g.identity_type_params()),
        field_id.0 as u32,
    );
    g.builder
        .emit_load_field(dest, context_register, field_idx, location);
}

pub(super) fn ensure_register(
    g: &mut AstBytecodeGen,
    dest: DataDest,
    ty: BytecodeType,
) -> Register {
    match dest {
        DataDest::Alloc => g.alloc_temp(ty),
        DataDest::Reg(reg) => reg,
    }
}

pub(super) fn add_const_pool_entry_for_call(
    g: &mut AstBytecodeGen,
    fct: &FctDefinition,
    call_type: &CallType,
) -> ConstPoolIdx {
    match call_type {
        CallType::GenericStaticMethod(id, .., trait_type_params, fct_type_params)
        | CallType::GenericMethod(id, .., trait_type_params, fct_type_params) => {
            g.builder.add_const(ConstPoolEntry::Generic(
                id.index() as u32,
                g.emitter.convert_function_id(fct.id()),
                g.convert_tya(&trait_type_params),
                g.convert_tya(&fct_type_params),
            ))
        }
        CallType::GenericMethodSelf(_, fct_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethodSelf(_, fct_id, trait_type_params, fct_type_params) => {
            g.builder.add_const(ConstPoolEntry::GenericSelf(
                g.emitter.convert_function_id(*fct_id),
                g.convert_tya(&trait_type_params),
                g.convert_tya(&fct_type_params),
            ))
        }
        CallType::GenericMethodNew {
            object_type,
            trait_ty,
            fct_id,
            fct_type_params,
        } => g.builder.add_const(ConstPoolEntry::GenericNew {
            object_type: g.emitter.convert_ty(object_type.clone()),
            trait_ty: g.emitter.convert_trait_ty(&trait_ty),
            fct_id: g.emitter.convert_function_id(*fct_id),
            fct_type_params: g.convert_tya(fct_type_params),
        }),
        CallType::TraitObjectMethod(trait_object_ty, _) => {
            g.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                g.emitter.convert_ty(trait_object_ty.clone()),
                g.emitter.convert_function_id(fct.id()),
            ))
        }

        CallType::Method(.., type_params)
        | CallType::Expr(.., type_params)
        | CallType::Fct(.., type_params) => {
            assert_eq!(
                fct.type_param_definition().type_param_count(),
                type_params.len()
            );
            g.builder.add_const_fct_types(
                g.emitter.convert_function_id(fct.id()),
                g.convert_tya(&type_params),
            )
        }

        _ => panic!("unexpected call type {:?}", call_type),
    }
}

pub(super) fn specialize_type_for_call(
    g: &AstBytecodeGen,
    call_type: &CallType,
    ty: SourceType,
) -> SourceType {
    match call_type {
        CallType::Fct(_, type_params)
        | CallType::Expr(_, _, type_params)
        | CallType::Method(_, _, type_params) => specialize_type(g.sa, ty, type_params),

        CallType::TraitObjectMethod(trait_ty, _actual_object_ty) => {
            let (trait_id, type_params, assoc_types) = match trait_ty {
                SourceType::TraitObject(trait_id, type_params, assoc_types) => {
                    (*trait_id, type_params, assoc_types)
                }
                _ => unreachable!(),
            };
            specialize_ty_for_trait_object(g.sa, ty, trait_id, type_params, assoc_types)
        }
        CallType::GenericMethod(id, _trait_id, _method_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethod(
            id,
            _trait_id,
            _method_id,
            trait_type_params,
            fct_type_params,
        ) => replace_type(
            g.sa,
            ty,
            Some(&trait_type_params.connect(fct_type_params)),
            Some(SourceType::TypeParam(*id)),
        ),

        CallType::GenericMethodSelf(_trait_id, _fct_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethodSelf(
            _trait_id,
            _fct_id,
            trait_type_params,
            fct_type_params,
        ) => replace_type(
            g.sa,
            ty,
            Some(&trait_type_params.connect(fct_type_params)),
            None,
        ),

        CallType::GenericMethodNew {
            trait_ty,
            fct_type_params,
            ..
        } => replace_type(
            g.sa,
            ty,
            Some(&trait_ty.type_params.connect(fct_type_params)),
            None,
        ),

        CallType::Lambda(..)
        | CallType::NewClass(..)
        | CallType::NewStruct(..)
        | CallType::NewEnum(..)
        | CallType::Intrinsic(..) => unreachable!(),
    }
}
