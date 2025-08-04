use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId, Location, Register};
use dora_parser::ast::{self, CmpOp};
use dora_parser::Span;

use crate::generator::{register_bty_from_ty, AstBytecodeGen, DataDest, Label};
use crate::sema::{FctDefinition, FctParent, Intrinsic, Sema};
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: &ast::ExprData, dest: DataDest) -> Register {
    match *expr {
        ast::ExprData::Un(ref un) => g.visit_expr_un(un, dest),
        ast::ExprData::Bin(ref bin) => g.visit_expr_bin(bin, dest),
        ast::ExprData::Dot(ref field) => g.visit_expr_dot(field, dest),
        ast::ExprData::Block(ref block) => g.visit_expr_block(block, dest),
        ast::ExprData::If(ref expr) => g.visit_expr_if(expr, dest),
        ast::ExprData::Template(ref template) => g.visit_expr_template(template, dest),
        ast::ExprData::TypeParam(ref expr) => g.visit_expr_type_param(expr, dest),
        ast::ExprData::Path(ref path) => g.visit_expr_path(path, dest),
        ast::ExprData::LitChar(ref lit) => g.visit_expr_lit_char(lit, dest),
        ast::ExprData::LitInt(ref lit) => g.visit_expr_lit_int(lit, dest, false),
        ast::ExprData::LitFloat(ref lit) => g.visit_expr_lit_float(lit, dest),
        ast::ExprData::LitStr(ref lit) => g.visit_expr_lit_string(lit, dest),
        ast::ExprData::LitBool(ref lit) => g.visit_expr_lit_bool(lit, dest),
        ast::ExprData::Ident(ref ident) => g.visit_expr_ident(ident, dest),
        ast::ExprData::Call(ref call) => g.visit_expr_call(call, dest),
        ast::ExprData::This(ref expr) => g.visit_expr_self(expr, dest),
        ast::ExprData::Conv(ref conv) => g.visit_expr_conv(conv, dest),
        ast::ExprData::Is(ref node) => g.visit_expr_is(node, dest),
        ast::ExprData::Tuple(ref tuple) => g.visit_expr_tuple(tuple, dest),
        ast::ExprData::Paren(ref paren) => gen_expr(g, &paren.expr, dest),
        ast::ExprData::Match(ref expr) => gen_match(g, expr, dest),
        ast::ExprData::Lambda(ref node) => g.visit_expr_lambda(node, dest),
        ast::ExprData::For(ref node) => g.visit_expr_for(node, dest),
        ast::ExprData::While(ref node) => g.visit_expr_while(node, dest),
        ast::ExprData::Break(ref node) => g.visit_expr_break(node, dest),
        ast::ExprData::Continue(ref node) => g.visit_expr_continue(node, dest),
        ast::ExprData::Return(ref ret) => g.visit_expr_return(ret, dest),
        ast::ExprData::Error { .. } => unreachable!(),
    }
}

pub(super) fn gen_expr_condition(g: &mut AstBytecodeGen, expr: &ast::ExprData, false_lbl: Label) {
    if let Some(bin_expr) = expr.to_bin_and() {
        if let Some(is_expr) = bin_expr.lhs.to_is() {
            let value_reg = gen_expr(g, &is_expr.value, DataDest::Alloc);
            let value_ty = g.ty(is_expr.value.id());
            g.setup_pattern_vars(&is_expr.pattern);
            g.destruct_pattern(&is_expr.pattern, value_reg, value_ty, Some(false_lbl));
            g.free_if_temp(value_reg);
        } else {
            let cond_reg = gen_expr(g, &bin_expr.lhs, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, false_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr_condition(g, &bin_expr.rhs, false_lbl);
    } else if let Some(is_expr) = expr.to_is() {
        let value_reg = gen_expr(g, &is_expr.value, DataDest::Alloc);
        let value_ty = g.ty(is_expr.value.id());
        g.setup_pattern_vars(&is_expr.pattern);
        g.destruct_pattern(&is_expr.pattern, value_reg, value_ty, Some(false_lbl));
        g.free_if_temp(value_reg);
    } else {
        let cond_reg = gen_expr(g, &expr, DataDest::Alloc);
        g.builder.emit_jump_if_false(cond_reg, false_lbl);
        g.free_if_temp(cond_reg);
    }
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
    expr: &ast::ExprBinType,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    let call_type = g.analysis.map_calls.get(expr.id).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = g.add_const_pool_entry_for_call(&callee, &call_type);

    let function_return_type: SourceType =
        g.specialize_type_for_call(call_type, callee.return_type());

    g.builder.emit_push_register(lhs_reg);
    g.builder.emit_push_register(rhs_reg);

    if call_type.is_generic_method() {
        g.emit_invoke_generic_direct(function_return_type, dest, callee_idx, location);
    } else {
        g.emit_invoke_direct(function_return_type, dest, callee_idx, location);
    }
}

pub(super) fn gen_expr_bin_cmp(
    g: &mut AstBytecodeGen,
    node: &ast::ExprBinType,
    cmp_op: CmpOp,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr(g, &node.lhs, DataDest::Alloc);
    let rhs = gen_expr(g, &node.rhs, DataDest::Alloc);

    let result = if let Some(info) = g.get_intrinsic(node.id) {
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
    let dest = g.ensure_register(dest, BytecodeType::Bool);

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
    node: &ast::ExprBinType,
    cmp_op: CmpOp,
    dest: DataDest,
    lhs: Register,
    rhs: Register,
) -> Register {
    let call_type = g.analysis.map_calls.get(node.id).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = g.add_const_pool_entry_for_call(&callee, &call_type);

    let function_return_type: SourceType =
        g.specialize_type_for_call(call_type, callee.return_type());

    let function_return_type_bc: BytecodeType = register_bty_from_ty(function_return_type.clone());

    let return_type = BytecodeType::Bool;

    let dest = g.ensure_register(dest, return_type.clone());

    let result = if function_return_type_bc == return_type {
        dest
    } else {
        let function_result_register_ty: BytecodeType =
            register_bty_from_ty(function_return_type.clone());
        g.alloc_temp(function_result_register_ty)
    };

    g.builder.emit_push_register(lhs);
    g.builder.emit_push_register(rhs);

    if call_type.is_generic_method() {
        g.emit_invoke_generic_direct(function_return_type, result, callee_idx, g.loc(node.span));
    } else {
        g.emit_invoke_direct(function_return_type, result, callee_idx, g.loc(node.span));
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
                convert_ordering_to_bool(g, node, cmp_op, result, dest);
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
    node: &ast::ExprBinType,
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
        FunctionId(fct_id.index().try_into().expect("overflow")),
        BytecodeTypeArray::empty(),
    );
    g.builder.emit_invoke_direct(dest, idx, g.loc(node.span));
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

pub(super) fn gen_match(
    g: &mut AstBytecodeGen,
    node: &ast::ExprMatchType,
    dest: DataDest,
) -> Register {
    let result_ty = g.ty(node.id);
    let expr_ty = g.ty(node.expr.id());

    let result_bc_ty = register_bty_from_ty(result_ty);
    let dest = g.ensure_register(dest, result_bc_ty);

    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, &node.expr, DataDest::Alloc);

    let mut arm_labels = Vec::with_capacity(node.arms.len());

    for _arm in &node.arms {
        arm_labels.push(g.builder.create_label());
    }

    arm_labels.push(fallthrough_lbl);

    for (idx, arm) in node.arms.iter().enumerate() {
        let arm_lbl = arm_labels[idx];
        g.builder.bind_label(arm_lbl);

        let next_arm_lbl = arm_labels[idx + 1];

        g.push_scope();

        let pattern = arm.pattern.as_ref();
        g.setup_pattern_vars(pattern);
        g.destruct_pattern(pattern, expr_reg, expr_ty.clone(), Some(next_arm_lbl));

        if let Some(ref cond) = arm.cond {
            let cond_reg = gen_expr(g, cond, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, next_arm_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr(g, &arm.value, DataDest::Reg(dest));

        g.builder.emit_jump(merge_lbl);
        g.pop_scope();
    }

    g.builder.bind_label(fallthrough_lbl);
    gen_unreachable(g, node.span);

    g.builder.bind_label(merge_lbl);
    g.free_if_temp(expr_reg);

    dest
}

pub(super) fn gen_unreachable(g: &mut AstBytecodeGen, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = register_bty_from_ty(return_type.clone());
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

pub(super) fn gen_fatal_error(g: &mut AstBytecodeGen, msg: &str, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = register_bty_from_ty(return_type.clone());
    let dest_reg = g.alloc_temp(register_bty);
    let msg_reg = g.alloc_temp(BytecodeType::Ptr);
    g.builder.emit_const_string(msg_reg, msg.to_string());
    g.builder.emit_push_register(msg_reg);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_idx = g.builder.add_const_fct_types(
        FunctionId(
            g.sa.known
                .functions
                .fatal_error()
                .index()
                .try_into()
                .expect("overflow"),
        ),
        fct_type_params,
    );
    g.builder.emit_invoke_direct(dest_reg, fct_idx, g.loc(span));
    g.builder.emit_ret(dest_reg);
    g.free_temp(dest_reg);
    g.free_temp(msg_reg);
}
