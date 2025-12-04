use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ConstPoolEntry, FunctionId, Location, Register,
};
use dora_parser::Span;
use dora_parser::ast::{self, AstExpr, CmpOp, SyntaxNodeBase};

use crate::expr_always_returns;
use crate::generator::pattern::{destruct_pattern, destruct_pattern_or_fail, setup_pattern_vars};
use crate::generator::{
    AstBytecodeGen, DataDest, IntrinsicInfo, Label, LoopLabels, SELF_VAR_ID, emit_mov,
    field_id_from_context_idx, last_context_register, store_in_context, var_reg,
};
use crate::sema::{
    CallType, ClassDefinitionId, ConstDefinitionId, ContextFieldId, Element, EnumDefinitionId,
    FctDefinition, FctParent, GlobalDefinitionId, IdentType, Intrinsic, OuterContextIdx, ScopeId,
    Sema, StructDefinitionId, VarId, VarLocation, emit_as_bytecode_operation,
};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::{SourceType, SourceTypeArray};
use dora_bytecode::ConstPoolIdx;

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: AstExpr, dest: DataDest) -> Register {
    match expr {
        AstExpr::Un(node) => gen_expr_un(g, node, dest),
        AstExpr::Bin(node) => gen_expr_bin(g, node, dest),
        AstExpr::DotExpr(node) => gen_expr_dot(g, node, dest),
        AstExpr::Block(node) => gen_expr_block(g, node, dest),
        AstExpr::If(node) => gen_expr_if(g, node, dest),
        AstExpr::Template(node) => gen_expr_template(g, node, dest),
        AstExpr::TypedExpr(node) => gen_expr_type_param(g, node, dest),
        AstExpr::Path(node) => gen_expr_path(g, node, dest),
        AstExpr::LitChar(node) => gen_expr_lit_char(g, node, dest),
        AstExpr::LitInt(node) => gen_expr_lit_int(g, node, dest, false),
        AstExpr::LitFloat(node) => gen_expr_lit_float(g, node, dest),
        AstExpr::LitStr(node) => gen_expr_lit_string(g, node, dest),
        AstExpr::LitBool(node) => gen_expr_lit_bool(g, node, dest),
        AstExpr::NameExpr(node) => gen_expr_ident(g, node, dest),
        AstExpr::Call(node) => gen_expr_call(g, node, dest),
        AstExpr::This(node) => gen_expr_self(g, node, dest),
        AstExpr::Conv(node) => gen_expr_conv(g, node, dest),
        AstExpr::Is(node) => gen_expr_is(g, node, dest),
        AstExpr::Tuple(node) => gen_expr_tuple(g, node, dest),
        AstExpr::Paren(node) => gen_expr(g, node.expr().unwrap(), dest),
        AstExpr::Match(node) => gen_match(g, node, dest),
        AstExpr::Lambda(node) => gen_expr_lambda(g, node, dest),
        AstExpr::For(node) => gen_expr_for(g, node, dest),
        AstExpr::While(node) => gen_expr_while(g, node, dest),
        AstExpr::Break(node) => gen_expr_break(g, node, dest),
        AstExpr::Continue(node) => gen_expr_continue(g, node, dest),
        AstExpr::Return(node) => gen_expr_return(g, node, dest),
        AstExpr::MethodCallExpr(_) => unreachable!(),
        AstExpr::Error(_) => unreachable!(),
    }
}

fn gen_expr_condition(g: &mut AstBytecodeGen, expr: AstExpr, false_lbl: Label) {
    if let Some(bin_expr) = expr.clone().to_bin() {
        if bin_expr.op() == ast::BinOp::And {
            let lhs = bin_expr.lhs();
            let rhs = bin_expr.rhs();

            if let Some(is_expr) = lhs.clone().to_is() {
                let value_reg = gen_expr(g, is_expr.value(), DataDest::Alloc);
                let value_ty = g.ty(is_expr.value().id());
                setup_pattern_vars(g, is_expr.pattern());
                destruct_pattern(g, is_expr.pattern(), value_reg, value_ty, Some(false_lbl));
                g.free_if_temp(value_reg);
            } else {
                let cond_reg = gen_expr(g, lhs, DataDest::Alloc);
                g.builder.emit_jump_if_false(cond_reg, false_lbl);
                g.free_if_temp(cond_reg);
            }

            gen_expr_condition(g, rhs, false_lbl);
            return;
        }
    }

    if let Some(is_expr) = expr.clone().to_is() {
        let value_reg = gen_expr(g, is_expr.value(), DataDest::Alloc);
        let value_ty = g.ty(is_expr.value().id());
        setup_pattern_vars(g, is_expr.pattern());
        destruct_pattern(g, is_expr.pattern(), value_reg, value_ty, Some(false_lbl));
        g.free_if_temp(value_reg);
    } else {
        let cond_reg = gen_expr(g, expr, DataDest::Alloc);
        g.builder.emit_jump_if_false(cond_reg, false_lbl);
        g.free_if_temp(cond_reg);
    }
}

fn gen_expr_self(g: &mut AstBytecodeGen, expr: ast::AstThis, dest: DataDest) -> Register {
    let expr_id = expr.id();
    if g.is_lambda {
        let ident = g.analysis.map_idents.get(expr_id).expect("missing ident");
        let (level, context_idx) = match ident {
            IdentType::Context(level, context_idx) => (*level, *context_idx),
            _ => unreachable!(),
        };
        gen_expr_ident_context(g, level, context_idx, dest, g.loc(expr.span()))
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

fn gen_expr_lit_char(g: &mut AstBytecodeGen, node: ast::AstLitChar, dest: DataDest) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Char);

    let value = g.analysis.const_value(node.id()).to_char();
    g.builder.emit_const_char(dest, value);

    dest
}

fn gen_expr_lit_int(
    g: &mut AstBytecodeGen,
    node: ast::AstLitInt,
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

fn gen_expr_lit_float(g: &mut AstBytecodeGen, node: ast::AstLitFloat, dest: DataDest) -> Register {
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

fn gen_expr_lit_string(g: &mut AstBytecodeGen, node: ast::AstLitStr, dest: DataDest) -> Register {
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

fn gen_expr_lit_bool(g: &mut AstBytecodeGen, node: ast::AstLitBool, dest: DataDest) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    if node.value() {
        g.builder.emit_const_true(dest);
    } else {
        g.builder.emit_const_false(dest);
    }

    dest
}

fn gen_expr_tuple(g: &mut AstBytecodeGen, e: ast::AstTuple, dest: DataDest) -> Register {
    let node_id = e.id();

    if e.values_len() == 0 {
        return g.ensure_unit_register();
    }

    let ty = g.ty(node_id);

    let result_ty: BytecodeType = g.emitter.convert_ty_reg(ty.clone());
    let result = ensure_register(g, dest, result_ty);

    let mut values = Vec::with_capacity(e.values_len());

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

fn gen_expr_un(g: &mut AstBytecodeGen, node: ast::AstUn, dest: DataDest) -> Register {
    let node_id = node.id();
    let opnd = node.opnd();
    if node.op() == ast::UnOp::Neg && opnd.is_lit_int() {
        gen_expr_lit_int(g, opnd.as_lit_int(), dest, true)
    } else if let Some(intrinsic) = g.get_intrinsic(node_id) {
        emit_intrinsic_un(g, opnd, intrinsic, g.loc(node.span()), dest)
    } else {
        gen_expr_un_method(g, node, dest)
    }
}

fn gen_expr_un_method(g: &mut AstBytecodeGen, node: ast::AstUn, dest: DataDest) -> Register {
    let node_id = node.id();
    let opnd = gen_expr(g, node.opnd(), DataDest::Alloc);

    let call_type = g.analysis.map_calls.get(node_id).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type, callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(function_return_type.clone());
    let dest = ensure_register(g, dest, function_return_type_bc);

    g.builder.emit_push_register(opnd);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            g.loc(node.span()),
        );
    } else {
        emit_invoke_direct(
            g,
            function_return_type,
            dest,
            callee_idx,
            g.loc(node.span()),
        );
    }

    g.free_if_temp(opnd);

    dest
}

fn gen_expr_ident(g: &mut AstBytecodeGen, ident: ast::AstNameExpr, dest: DataDest) -> Register {
    let ast_id = ident.id();
    let ident_type = g.analysis.map_idents.get(ast_id).unwrap();

    match ident_type {
        &IdentType::Var(var_id) => gen_expr_ident_var(g, var_id, dest, g.loc(ident.span())),
        &IdentType::Context(level, field_id) => {
            gen_expr_ident_context(g, level, field_id, dest, g.loc(ident.span()))
        }
        &IdentType::Global(gid) => gen_expr_ident_global(g, gid, dest, g.loc(ident.span())),
        &IdentType::Const(cid) => gen_expr_ident_const(g, cid, dest),
        &IdentType::EnumVariant(enum_id, ref type_params, variant_idx) => emit_new_enum(
            g,
            enum_id,
            type_params.clone(),
            variant_idx,
            g.loc(ident.span()),
            dest,
        ),

        &IdentType::Field(..) => unreachable!(),
        &IdentType::Struct(..) => unreachable!(),
        &IdentType::StructField(..) => unreachable!(),

        &IdentType::Fct(..) => unreachable!(),
        &IdentType::Class(..) => unreachable!(),
    }
}

fn gen_expr_ident_context(
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

    assert!(context_id.0 < g.analysis.outer_contexts.len());

    for outer_context_class in g
        .analysis
        .outer_contexts
        .iter()
        .skip(context_id.0 + 1)
        .rev()
    {
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

    let outer_context_info = g.analysis.outer_contexts[context_id.0].clone();
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

fn gen_expr_ident_const(
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

fn gen_expr_ident_global(
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

fn gen_expr_ident_var(
    g: &mut AstBytecodeGen,
    var_id: VarId,
    dest: DataDest,
    location: Location,
) -> Register {
    let var = g.analysis.vars.get_var(var_id);

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

fn gen_intrinsic_bin(
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

fn gen_method_bin(
    g: &mut AstBytecodeGen,
    expr: ast::AstExpr,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    let call_type = g.analysis.map_calls.get(expr.id()).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type, callee.return_type());

    g.builder.emit_push_register(lhs_reg);
    g.builder.emit_push_register(rhs_reg);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(g, function_return_type, dest, callee_idx, location);
    } else {
        emit_invoke_direct(g, function_return_type, dest, callee_idx, location);
    }
}

fn gen_expr_bin_cmp(
    g: &mut AstBytecodeGen,
    node: ast::AstBin,
    cmp_op: CmpOp,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr(g, node.lhs(), DataDest::Alloc);
    let rhs = gen_expr(g, node.rhs(), DataDest::Alloc);

    let result = if let Some(info) = g.get_intrinsic(node.id()) {
        gen_expr_bin_cmp_as_intrinsic(g, cmp_op, info.intrinsic, dest, lhs, rhs)
    } else {
        gen_expr_bin_cmp_as_method(g, node, cmp_op, dest, lhs, rhs)
    };

    g.free_if_temp(lhs);
    g.free_if_temp(rhs);

    result
}

fn gen_expr_bin_cmp_as_intrinsic(
    g: &mut AstBytecodeGen,
    cmp_op: CmpOp,
    intrinsic: Intrinsic,
    dest: DataDest,
    lhs: Register,
    rhs: Register,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    match intrinsic {
        Intrinsic::BoolEq
        | Intrinsic::UInt8Eq
        | Intrinsic::CharEq
        | Intrinsic::EnumEq
        | Intrinsic::EnumNe
        | Intrinsic::Int32Eq
        | Intrinsic::Int64Eq
        | Intrinsic::Float32Eq
        | Intrinsic::Float64Eq => match cmp_op {
            CmpOp::Eq => g.builder.emit_test_eq(dest, lhs, rhs),
            CmpOp::Ne => g.builder.emit_test_ne(dest, lhs, rhs),
            _ => unreachable!(),
        },
        Intrinsic::UInt8Cmp
        | Intrinsic::CharCmp
        | Intrinsic::Int32Cmp
        | Intrinsic::Int64Cmp
        | Intrinsic::Float32Cmp
        | Intrinsic::Float64Cmp => match cmp_op {
            CmpOp::Lt => g.builder.emit_test_lt(dest, lhs, rhs),
            CmpOp::Le => g.builder.emit_test_le(dest, lhs, rhs),
            CmpOp::Ge => g.builder.emit_test_ge(dest, lhs, rhs),
            CmpOp::Gt => g.builder.emit_test_gt(dest, lhs, rhs),
            _ => unreachable!(),
        },

        _ => unreachable!(),
    }

    dest
}

fn gen_expr_bin_cmp_as_method(
    g: &mut AstBytecodeGen,
    node: ast::AstBin,
    cmp_op: CmpOp,
    dest: DataDest,
    lhs: Register,
    rhs: Register,
) -> Register {
    let call_type = g.analysis.map_calls.get(node.id()).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type, callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(function_return_type.clone());

    let return_type = BytecodeType::Bool;

    let dest = ensure_register(g, dest, return_type.clone());

    let result = if function_return_type_bc == return_type {
        dest
    } else {
        let function_result_register_ty: BytecodeType =
            g.emitter.convert_ty_reg(function_return_type.clone());
        g.alloc_temp(function_result_register_ty)
    };

    g.builder.emit_push_register(lhs);
    g.builder.emit_push_register(rhs);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(
            g,
            function_return_type,
            result,
            callee_idx,
            g.loc(node.span()),
        );
    } else {
        emit_invoke_direct(
            g,
            function_return_type,
            result,
            callee_idx,
            g.loc(node.span()),
        );
    }

    match cmp_op {
        CmpOp::Eq => assert_eq!(result, dest),

        CmpOp::Ne => {
            assert_eq!(result, dest);
            g.builder.emit_not(dest, dest);
        }

        CmpOp::Ge | CmpOp::Gt | CmpOp::Le | CmpOp::Lt => {
            assert_ne!(result, dest);

            if is_comparable_method(g.sa, &*callee) {
                convert_ordering_to_bool(g, &node, cmp_op, result, dest);
            } else {
                convert_int_cmp_to_bool(g, cmp_op, result, dest);
            }
        }

        CmpOp::Is | CmpOp::IsNot => unreachable!(),
    }

    if dest != result {
        g.free_temp(result);
    }

    dest
}

fn is_comparable_method(sa: &Sema, fct: &FctDefinition) -> bool {
    match fct.parent {
        FctParent::Impl(impl_id) => {
            let impl_ = &sa.impl_(impl_id);
            impl_.trait_id().expect("trait expected") == sa.known.traits.comparable()
        }

        FctParent::Trait(trait_id) => trait_id == sa.known.traits.comparable(),

        _ => false,
    }
}

fn convert_ordering_to_bool(
    g: &mut AstBytecodeGen,
    node: &ast::AstBin,
    cmp_op: CmpOp,
    result: Register,
    dest: Register,
) {
    let fct_id = match cmp_op {
        CmpOp::Lt => g.sa.known.functions.ordering_is_lt(),
        CmpOp::Le => g.sa.known.functions.ordering_is_le(),
        CmpOp::Gt => g.sa.known.functions.ordering_is_gt(),
        CmpOp::Ge => g.sa.known.functions.ordering_is_ge(),
        ast::CmpOp::Eq | ast::CmpOp::Ne | ast::CmpOp::Is | ast::CmpOp::IsNot => {
            unreachable!()
        }
    };

    g.builder.emit_push_register(result);
    let idx = g.builder.add_const_fct_types(
        g.emitter.convert_function_id(fct_id),
        BytecodeTypeArray::empty(),
    );
    g.builder.emit_invoke_direct(dest, idx, g.loc(node.span()));
}

fn convert_int_cmp_to_bool(
    g: &mut AstBytecodeGen,
    cmp_op: CmpOp,
    result: Register,
    dest: Register,
) {
    let zero = g.alloc_temp(BytecodeType::Int32);
    g.builder.emit_const_int32(zero, 0);

    match cmp_op {
        CmpOp::Lt => g.builder.emit_test_lt(dest, result, zero),
        CmpOp::Le => g.builder.emit_test_le(dest, result, zero),
        CmpOp::Gt => g.builder.emit_test_gt(dest, result, zero),
        CmpOp::Ge => g.builder.emit_test_ge(dest, result, zero),
        ast::CmpOp::Eq | ast::CmpOp::Ne | ast::CmpOp::Is | ast::CmpOp::IsNot => {
            unreachable!()
        }
    }

    g.free_temp(zero);
}

fn gen_match(g: &mut AstBytecodeGen, node: ast::AstMatch, dest: DataDest) -> Register {
    let result_ty = g.ty(node.id());
    let expr_ty = g.ty(node.expr().unwrap().id());

    let result_bc_ty = g.emitter.convert_ty_reg(result_ty);
    let dest = ensure_register(g, dest, result_bc_ty);

    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, node.expr().unwrap(), DataDest::Alloc);

    let num_arms = node.arms().count();

    let mut arm_labels = Vec::with_capacity(num_arms);

    for _ in 0..num_arms {
        arm_labels.push(g.builder.create_label());
    }

    arm_labels.push(fallthrough_lbl);

    for (idx, arm) in node.arms().enumerate() {
        let arm_lbl = arm_labels[idx];
        g.builder.bind_label(arm_lbl);

        let next_arm_lbl = arm_labels[idx + 1];

        g.push_scope();

        setup_pattern_vars(g, arm.pattern());
        destruct_pattern(
            g,
            arm.pattern(),
            expr_reg,
            expr_ty.clone(),
            Some(next_arm_lbl),
        );

        if let Some(cond) = arm.cond() {
            let cond_reg = gen_expr(g, cond, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, next_arm_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr(g, arm.value(), DataDest::Reg(dest));

        g.builder.emit_jump(merge_lbl);
        g.pop_scope();
    }

    g.builder.bind_label(fallthrough_lbl);
    gen_unreachable(g, node.span());

    g.builder.bind_label(merge_lbl);
    g.free_if_temp(expr_reg);

    dest
}

fn gen_unreachable(g: &mut AstBytecodeGen, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty_reg(return_type.clone());
    let dest = g.alloc_temp(register_bty);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_idx = g.builder.add_const_fct_types(
        FunctionId(
            g.sa.known
                .functions
                .unreachable()
                .index()
                .try_into()
                .expect("overflow"),
        ),
        fct_type_params,
    );
    g.builder.emit_invoke_direct(dest, fct_idx, g.loc(span));
    g.builder.emit_ret(dest);
    g.free_temp(dest);
}

fn gen_expr_for_effect(g: &mut AstBytecodeGen, expr: ast::AstBlock) {
    let reg = gen_expr(g, expr.into(), DataDest::Alloc);
    g.free_if_temp(reg);
}

fn gen_expr_for(g: &mut AstBytecodeGen, stmt: ast::AstFor, _dest: DataDest) -> Register {
    let stmt_ast_id = stmt.id();
    g.push_scope();
    let for_type_info = g.analysis.map_fors.get(stmt_ast_id).unwrap().clone();

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

fn gen_expr_while(g: &mut AstBytecodeGen, node: ast::AstWhile, _dest: DataDest) -> Register {
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

fn gen_expr_return(g: &mut AstBytecodeGen, ret: ast::AstReturn, _dest: DataDest) -> Register {
    let result_reg = if let Some(expr) = ret.expr() {
        gen_expr(g, expr, DataDest::Alloc)
    } else {
        g.ensure_unit_register()
    };

    g.builder.emit_ret(result_reg);
    g.free_if_temp(result_reg);

    g.ensure_unit_register()
}

fn gen_expr_break(g: &mut AstBytecodeGen, _node: ast::AstBreak, _dest: DataDest) -> Register {
    let end = g.loops.last().unwrap().end;
    g.builder.emit_jump(end);
    g.ensure_unit_register()
}

fn gen_expr_continue(g: &mut AstBytecodeGen, _node: ast::AstContinue, _dest: DataDest) -> Register {
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

fn gen_expr_type_param(
    g: &mut AstBytecodeGen,
    expr: ast::AstTypedExpr,
    dest: DataDest,
) -> Register {
    let expr_id = expr.id();
    let ident_type = g.analysis.map_idents.get(expr_id).cloned().unwrap();

    match ident_type {
        IdentType::EnumVariant(enum_id, type_params, variant_idx) => emit_new_enum(
            g,
            enum_id,
            type_params,
            variant_idx,
            g.loc(expr.span()),
            dest,
        ),

        _ => unreachable!(),
    }
}

fn gen_expr_template(g: &mut AstBytecodeGen, expr: ast::AstTemplate, dest: DataDest) -> Register {
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
        if let Some(..) = part.clone().to_lit_str() {
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
                    .map_templates
                    .get(part.id())
                    .expect("missing toString id");

                let type_params = g.convert_tya(&type_params);

                let fct_idx = g
                    .builder
                    .add_const_fct_types(g.emitter.convert_function_id(*to_string_id), type_params);
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

fn gen_expr_path(g: &mut AstBytecodeGen, expr: ast::AstPath, dest: DataDest) -> Register {
    let expr_id = expr.id();
    let ident_type = g.analysis.map_idents.get(expr_id).cloned().unwrap();

    match ident_type {
        IdentType::EnumVariant(enum_id, type_params, variant_idx) => emit_new_enum(
            g,
            enum_id,
            type_params,
            variant_idx,
            g.loc(expr.span()),
            dest,
        ),

        IdentType::Const(const_id) => gen_expr_ident_const(g, const_id, dest),

        IdentType::Global(global_id) => {
            gen_expr_ident_global(g, global_id, dest, g.loc(expr.span()))
        }

        IdentType::StructField(..)
        | IdentType::Struct(..)
        | IdentType::Field(..)
        | IdentType::Fct(..)
        | IdentType::Class(..)
        | IdentType::Var(..)
        | IdentType::Context(..) => unreachable!(),
    }
}

fn gen_expr_conv(g: &mut AstBytecodeGen, expr: ast::AstConv, dest: DataDest) -> Register {
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

fn gen_expr_is(g: &mut AstBytecodeGen, node: ast::AstIs, dest: DataDest) -> Register {
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

fn gen_expr_lambda(g: &mut AstBytecodeGen, node: ast::AstLambda, dest: DataDest) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Ptr);

    let lambda_fct_id = g
        .analysis
        .map_lambdas
        .get(node.id())
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

fn gen_expr_if(g: &mut AstBytecodeGen, expr: ast::AstIf, dest: DataDest) -> Register {
    let expr_id = expr.id();
    let ty = g.ty(expr_id);

    let dest = ensure_register(g, dest, g.emitter.convert_ty_reg(ty));
    let else_lbl = g.builder.create_label();

    g.push_scope();

    gen_expr_condition(g, expr.cond(), else_lbl);

    gen_expr(g, expr.then_block(), DataDest::Reg(dest));

    g.pop_scope();

    if let Some(else_block) = expr.else_block() {
        let end_lbl = g.builder.create_label();

        if !expr_always_returns(&g.sa.file(g.file_id).ast(), expr.then_block()) {
            g.builder.emit_jump(end_lbl);
        }

        g.builder.bind_label(else_lbl);
        gen_expr(g, else_block, DataDest::Reg(dest));
        g.builder.bind_label(end_lbl);
    } else {
        g.builder.bind_label(else_lbl);
    }

    dest
}

fn gen_expr_block(g: &mut AstBytecodeGen, block: ast::AstBlock, dest: DataDest) -> Register {
    g.push_scope();

    for stmt in block.stmts() {
        g.visit_stmt(stmt);
    }

    let result = if let Some(expr) = block.expr() {
        gen_expr(g, expr, dest)
    } else {
        g.ensure_unit_register()
    };

    g.pop_scope();

    result
}

fn gen_expr_dot(g: &mut AstBytecodeGen, expr: ast::AstDotExpr, dest: DataDest) -> Register {
    let expr_id = expr.id();
    let object_ty = g.ty(expr.lhs().id());

    if object_ty.is_tuple() {
        return gen_expr_dot_tuple(g, expr, object_ty, dest);
    }

    if let Some((struct_id, type_params)) = object_ty.to_struct() {
        return gen_expr_dot_struct(g, expr, struct_id, type_params, dest);
    }

    let (cls_ty, field_id) = {
        let ident_type = g.analysis.map_idents.get(expr_id).unwrap();

        match ident_type {
            IdentType::Field(ty, field) => (ty.clone(), *field),
            _ => unreachable!(),
        }
    };

    let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

    let field_idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&type_params),
        field_id.0 as u32,
    );

    let field_ty = g.ty(expr_id);

    let field_bc_ty: BytecodeType = g.emitter.convert_ty_reg(field_ty);
    let dest = ensure_register(g, dest, field_bc_ty);
    let obj = gen_expr(g, expr.lhs(), DataDest::Alloc);

    g.builder
        .emit_load_field(dest, obj, field_idx, g.loc(expr.op_span()));
    g.free_if_temp(obj);

    dest
}

fn gen_expr_dot_struct(
    g: &mut AstBytecodeGen,
    expr: ast::AstDotExpr,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
    dest: DataDest,
) -> Register {
    let expr_id = expr.id();
    let struct_obj = gen_expr(g, expr.lhs(), DataDest::Alloc);

    let ident_type = g.analysis.map_idents.get(expr_id).unwrap();

    let field_idx = match ident_type {
        IdentType::StructField(_, field_idx) => *field_idx,
        _ => unreachable!(),
    };

    let fty = g.ty(expr_id);

    if fty.is_unit() {
        g.free_if_temp(struct_obj);
        return g.ensure_unit_register();
    }

    let ty: BytecodeType = g.emitter.convert_ty_reg(fty);
    let dest = ensure_register(g, dest, ty);
    let const_idx = g.builder.add_const_struct_field(
        g.emitter.convert_struct_id(struct_id),
        g.convert_tya(&type_params),
        field_idx.0 as u32,
    );
    g.builder
        .emit_load_struct_field(dest, struct_obj, const_idx);

    g.free_if_temp(struct_obj);

    dest
}

fn gen_expr_dot_tuple(
    g: &mut AstBytecodeGen,
    expr: ast::AstDotExpr,
    tuple_ty: SourceType,
    dest: DataDest,
) -> Register {
    let tuple = gen_expr(g, expr.lhs(), DataDest::Alloc);
    let value_i64 = g
        .analysis
        .const_value(expr.rhs().id())
        .to_i64()
        .expect("integer expected");
    let idx: u32 = value_i64.try_into().expect("too large");

    let subtypes: SourceTypeArray = tuple_ty.tuple_subtypes().expect("tuple expected");
    let ty = subtypes[idx as usize].clone();

    let ty: BytecodeType = g.emitter.convert_ty_reg(ty);
    let dest = ensure_register(g, dest, ty);
    let idx = g
        .builder
        .add_const_tuple_element(g.emitter.convert_ty(tuple_ty), idx);
    g.builder.emit_load_tuple_element(dest, tuple, idx);

    g.free_if_temp(tuple);

    dest
}

fn gen_expr_bin(g: &mut AstBytecodeGen, expr: ast::AstBin, dest: DataDest) -> Register {
    let expr_ast_id = expr.id();
    let op = expr.op();

    if op.is_any_assign() {
        gen_expr_assign(g, expr, dest)
    } else if let ast::BinOp::Cmp(cmp_op) = op {
        if cmp_op == CmpOp::Is || cmp_op == CmpOp::IsNot {
            emit_bin_is(g, expr, dest)
        } else {
            gen_expr_bin_cmp(g, expr, cmp_op, dest)
        }
    } else if op == ast::BinOp::Or {
        emit_bin_or(g, expr, dest)
    } else if op == ast::BinOp::And {
        emit_bin_and(g, expr, dest)
    } else if let Some(info) = g.get_intrinsic(expr_ast_id) {
        emit_intrinsic_bin(g, expr.lhs(), expr.rhs(), info, g.loc(expr.span()), dest)
    } else {
        gen_expr_bin_method(g, expr, dest)
    }
}

fn gen_expr_bin_method(g: &mut AstBytecodeGen, node: ast::AstBin, dest: DataDest) -> Register {
    let lhs = gen_expr(g, node.lhs(), DataDest::Alloc);
    let rhs = gen_expr(g, node.rhs(), DataDest::Alloc);

    let call_type = g.analysis.map_calls.get(node.id()).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, call_type, callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(function_return_type.clone());

    let return_type = match node.op() {
        ast::BinOp::Cmp(_) => BytecodeType::Bool,
        _ => function_return_type_bc.clone(),
    };

    let dest = ensure_register(g, dest, return_type.clone());

    let result = if function_return_type_bc == return_type {
        dest
    } else {
        let function_result_register_ty: BytecodeType =
            g.emitter.convert_ty_reg(function_return_type.clone());
        g.alloc_temp(function_result_register_ty)
    };

    g.builder.emit_push_register(lhs);
    g.builder.emit_push_register(rhs);

    if call_type.is_generic_method() {
        emit_invoke_generic_direct(
            g,
            function_return_type,
            result,
            callee_idx,
            g.loc(node.span()),
        );
    } else {
        emit_invoke_direct(
            g,
            function_return_type,
            result,
            callee_idx,
            g.loc(node.span()),
        );
    }

    g.free_if_temp(lhs);
    g.free_if_temp(rhs);

    match node.op() {
        ast::BinOp::Cmp(ast::CmpOp::Eq) => assert_eq!(result, dest),
        ast::BinOp::Cmp(ast::CmpOp::Ne) => {
            assert_eq!(result, dest);
            g.builder.emit_not(dest, dest);
        }

        ast::BinOp::Cmp(op) => {
            assert_ne!(result, dest);
            let zero = g.alloc_temp(BytecodeType::Int32);
            g.builder.emit_const_int32(zero, 0);

            match op {
                ast::CmpOp::Lt => g.builder.emit_test_lt(dest, result, zero),
                ast::CmpOp::Le => g.builder.emit_test_le(dest, result, zero),
                ast::CmpOp::Gt => g.builder.emit_test_gt(dest, result, zero),
                ast::CmpOp::Ge => g.builder.emit_test_ge(dest, result, zero),
                ast::CmpOp::Eq | ast::CmpOp::Ne | ast::CmpOp::Is | ast::CmpOp::IsNot => {
                    unreachable!()
                }
            }

            g.free_temp(zero);
            g.free_temp(result);
        }
        _ => assert_eq!(result, dest),
    }

    dest
}

fn gen_expr_assert(g: &mut AstBytecodeGen, expr: ast::AstCall, _dest: DataDest) -> Register {
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

fn gen_expr_call(g: &mut AstBytecodeGen, node: ast::AstCall, dest: DataDest) -> Register {
    let node_id = node.id();
    if let Some(info) = g.get_intrinsic(node_id) {
        if emit_as_bytecode_operation(info.intrinsic) {
            return gen_expr_call_intrinsic(g, node.clone(), info, dest);
        }
    }

    let call_type = g.analysis.map_calls.get(node_id).unwrap().clone();

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
    expr: ast::AstCall,
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
    node: ast::AstCall,
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
    expr: ast::AstCall,
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
    node: ast::AstCall,
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
            .map_argument
            .get(arg.id())
            .expect("missing argument idx")
            .clone();

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
    node: ast::AstCall,
    info: IntrinsicInfo,
    dest: DataDest,
) -> Register {
    let intrinsic = info.intrinsic;
    let call_type = g.analysis.map_calls.get(node.id()).unwrap().clone();

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

fn gen_expr_assign(g: &mut AstBytecodeGen, expr: ast::AstBin, _dest: DataDest) -> Register {
    let lhs_expr = expr.lhs();

    if lhs_expr.is_name_expr() {
        let value_reg = gen_expr(g, expr.rhs(), DataDest::Alloc);
        let ident_type = g.analysis.map_idents.get(lhs_expr.id()).unwrap();
        match ident_type {
            &IdentType::Var(var_id) => {
                gen_expr_assign_var(g, expr.clone(), var_id, value_reg);
            }
            &IdentType::Context(level, field_id) => {
                gen_expr_assign_context(g, expr.clone(), level, field_id, value_reg);
            }
            &IdentType::Global(gid) => {
                gen_expr_assign_global(g, expr.clone(), gid, value_reg);
            }
            _ => unreachable!(),
        }
        g.free_if_temp(value_reg);
    } else if lhs_expr.is_path() {
        let value_reg = gen_expr(g, expr.rhs(), DataDest::Alloc);
        let ident_type = g.analysis.map_idents.get(lhs_expr.id()).unwrap();
        match ident_type {
            &IdentType::Global(gid) => {
                gen_expr_assign_global(g, expr.clone(), gid, value_reg);
            }
            _ => unreachable!(),
        }
        g.free_if_temp(value_reg);
    } else {
        match lhs_expr {
            AstExpr::DotExpr(dot) => {
                gen_expr_assign_dot(g, expr.clone(), dot);
            }
            AstExpr::Call(call) => {
                gen_expr_assign_call(g, expr.clone(), call);
            }
            _ => unreachable!(),
        };
    }

    g.ensure_unit_register()
}

fn gen_expr_assign_call(g: &mut AstBytecodeGen, expr: ast::AstBin, call_expr: ast::AstCall) {
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
        .map_array_assignments
        .get(expr.id())
        .expect("missing assignment data")
        .clone();

    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::BinOp::Assign {
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
            gen_method_bin(g, expr.into(), current, current, val_reg, location);
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

fn gen_expr_assign_dot(g: &mut AstBytecodeGen, expr: ast::AstBin, dot: ast::AstDotExpr) {
    let (cls_ty, field_index) = {
        let ident_type = g.analysis.map_idents.get(dot.id()).cloned().unwrap();
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

    let obj = gen_expr(g, dot.lhs(), DataDest::Alloc);
    let value = gen_expr(g, expr.rhs(), DataDest::Alloc);

    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::BinOp::Assign {
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

    if expr.op() != ast::BinOp::Assign {
        g.free_temp(assign_value);
    }

    g.free_if_temp(obj);
    g.free_if_temp(value);
}

fn gen_expr_assign_context(
    g: &mut AstBytecodeGen,
    expr: ast::AstBin,
    outer_context_id: OuterContextIdx,
    context_field_id: ContextFieldId,
    value: Register,
) {
    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::BinOp::Assign {
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

    if expr.op() != ast::BinOp::Assign {
        g.free_temp(assign_value);
    }
}

fn gen_expr_assign_var(g: &mut AstBytecodeGen, expr: ast::AstBin, var_id: VarId, value: Register) {
    let var = g.analysis.vars.get_var(var_id);

    let assign_value = if expr.op() != ast::BinOp::Assign {
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

    if expr.op() != ast::BinOp::Assign {
        g.free_if_temp(assign_value);
    }
}

fn gen_expr_assign_global(
    g: &mut AstBytecodeGen,
    expr: ast::AstBin,
    gid: GlobalDefinitionId,
    value: Register,
) {
    let bc_gid = g.emitter.convert_global_id(gid);
    let location = g.loc(expr.span());

    let assign_value = if expr.op() != ast::BinOp::Assign {
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

    if expr.op() != ast::BinOp::Assign {
        g.free_temp(assign_value);
    }
}

fn emit_new_enum(
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

fn determine_callee_types(
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

fn emit_call_object_argument(
    g: &mut AstBytecodeGen,
    expr: ast::AstCall,
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

fn emit_call_arguments(
    g: &mut AstBytecodeGen,
    expr: ast::AstCall,
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

fn emit_array_with_variadic_arguments(
    g: &mut AstBytecodeGen,
    expr: ast::AstCall,
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

fn emit_call_inst(
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

fn emit_invoke_direct(
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

fn emit_invoke_generic_direct(
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

fn emit_intrinsic_new_array(
    g: &mut AstBytecodeGen,
    call: ast::AstCall,
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

fn emit_bin_is(g: &mut AstBytecodeGen, expr: ast::AstBin, dest: DataDest) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    let lhs_reg = gen_expr(g, expr.lhs(), DataDest::Alloc);
    let rhs_reg = gen_expr(g, expr.rhs(), DataDest::Alloc);

    g.builder.emit_test_identity(dest, lhs_reg, rhs_reg);

    if expr.op() == ast::BinOp::Cmp(ast::CmpOp::IsNot) {
        g.builder.emit_not(dest, dest);
    }

    g.free_if_temp(lhs_reg);
    g.free_if_temp(rhs_reg);

    dest
}

fn emit_bin_or(g: &mut AstBytecodeGen, expr: ast::AstBin, dest: DataDest) -> Register {
    let end_lbl = g.builder.create_label();
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    gen_expr(g, expr.lhs(), DataDest::Reg(dest));
    g.builder.emit_jump_if_true(dest, end_lbl);
    gen_expr(g, expr.rhs(), DataDest::Reg(dest));
    g.builder.bind_label(end_lbl);

    dest
}

fn emit_bin_and(g: &mut AstBytecodeGen, expr: ast::AstBin, dest: DataDest) -> Register {
    let end_lbl = g.builder.create_label();
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    g.push_scope();

    if let Some(is_expr) = expr.lhs().to_is() {
        g.builder.emit_const_false(dest);
        let value = gen_expr(g, is_expr.value(), DataDest::Alloc);
        let ty = g.ty(is_expr.value().id());
        setup_pattern_vars(g, is_expr.pattern());
        destruct_pattern(g, is_expr.pattern(), value, ty, Some(end_lbl));
        g.free_if_temp(value);
    } else {
        gen_expr(g, expr.lhs(), DataDest::Reg(dest));
        g.builder.emit_jump_if_false(dest, end_lbl);
    }

    gen_expr(g, expr.rhs(), DataDest::Reg(dest));
    g.builder.bind_label(end_lbl);

    g.pop_scope();

    dest
}

fn emit_intrinsic_array_get(
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

fn emit_intrinsic_array_set(
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

fn emit_intrinsic_un(
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

fn emit_intrinsic_bin(
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

fn store_in_outer_context(
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

    assert!(level.0 < g.analysis.outer_contexts.len());

    for outer_context_class in g.analysis.outer_contexts.iter().skip(level.0 + 1).rev() {
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

    let outer_context_info = g.analysis.outer_contexts[level.0].clone();
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

fn load_from_outer_context(
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

    assert!(context_id.0 < g.analysis.outer_contexts.len());

    for outer_context_class in g
        .analysis
        .outer_contexts
        .iter()
        .skip(context_id.0 + 1)
        .rev()
    {
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

    let outer_context_info = g.analysis.outer_contexts[context_id.0].clone();
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
fn load_from_context(
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

fn ensure_register(g: &mut AstBytecodeGen, dest: DataDest, ty: BytecodeType) -> Register {
    match dest {
        DataDest::Alloc => g.alloc_temp(ty),
        DataDest::Reg(reg) => reg,
    }
}

fn add_const_pool_entry_for_call(
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

fn specialize_type_for_call(
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
