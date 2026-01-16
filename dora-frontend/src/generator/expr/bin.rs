use dora_bytecode::{BytecodeType, BytecodeTypeArray, Location, Register};
use dora_parser::ast::{self, CmpOp, SyntaxNodeBase};

use super::call::emit_intrinsic_bin;
use super::if_::emit_is;
use super::{
    add_const_pool_entry_for_call, emit_invoke_direct, emit_invoke_generic_direct, ensure_register,
    gen_expr, specialize_type_for_call,
};
use crate::flatten_and;
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{BinExpr, ExprId, FctDefinition, FctParent, Intrinsic, Sema};
use crate::ty::SourceType;

pub(super) fn gen_expr_bin(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &BinExpr,
    expr: ast::AstBinExpr,
    dest: DataDest,
) -> Register {
    let op = expr.op();

    if let ast::BinOp::Cmp(cmp_op) = op {
        if cmp_op == CmpOp::Is || cmp_op == CmpOp::IsNot {
            emit_bin_is(g, expr, dest)
        } else {
            gen_expr_bin_cmp(g, expr_id, expr, cmp_op, dest)
        }
    } else if op == ast::BinOp::Or {
        emit_bin_or(g, expr, dest)
    } else if op == ast::BinOp::And {
        emit_bin_and(g, expr, dest)
    } else if let Some(info) = g.get_intrinsic(expr_id) {
        emit_intrinsic_bin(g, expr.lhs(), expr.rhs(), info, g.loc(expr.span()), dest)
    } else {
        gen_expr_bin_method(g, expr_id, expr, dest)
    }
}

fn gen_expr_bin_cmp(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    node: ast::AstBinExpr,
    cmp_op: CmpOp,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr(g, node.lhs(), DataDest::Alloc);
    let rhs = gen_expr(g, node.rhs(), DataDest::Alloc);

    let result = if let Some(info) = g.get_intrinsic(expr_id) {
        gen_expr_bin_cmp_as_intrinsic(g, cmp_op, info.intrinsic, dest, lhs, rhs)
    } else {
        gen_expr_bin_cmp_as_method(g, expr_id, node, cmp_op, dest, lhs, rhs)
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
    expr_id: ExprId,
    node: ast::AstBinExpr,
    cmp_op: CmpOp,
    dest: DataDest,
    lhs: Register,
    rhs: Register,
) -> Register {
    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, &call_type, callee.return_type());

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
    node: &ast::AstBinExpr,
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

fn gen_expr_bin_method(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    node: ast::AstBinExpr,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr(g, node.lhs(), DataDest::Alloc);
    let rhs = gen_expr(g, node.rhs(), DataDest::Alloc);

    let call_type = g.analysis.get_call_type(expr_id).expect("missing CallType");
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = add_const_pool_entry_for_call(g, &callee, &call_type);

    let function_return_type: SourceType =
        specialize_type_for_call(g, &call_type, callee.return_type());

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

fn emit_bin_is(g: &mut AstBytecodeGen, expr: ast::AstBinExpr, dest: DataDest) -> Register {
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

fn emit_bin_or(g: &mut AstBytecodeGen, expr: ast::AstBinExpr, dest: DataDest) -> Register {
    let end_lbl = g.builder.create_label();
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    gen_expr(g, expr.lhs(), DataDest::Reg(dest));
    g.builder.emit_jump_if_true(dest, end_lbl);
    gen_expr(g, expr.rhs(), DataDest::Reg(dest));
    g.builder.bind_label(end_lbl);

    dest
}

fn emit_bin_and(g: &mut AstBytecodeGen, expr: ast::AstBinExpr, dest: DataDest) -> Register {
    let end_lbl = g.builder.create_label();
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    let conditions = flatten_and(expr);

    g.push_scope();

    let conditions_len = conditions.len();

    for (idx, cond) in conditions.into_iter().enumerate() {
        if cond.is_is_expr() {
            let is_expr = cond.as_is_expr();
            g.builder.emit_const_false(dest);
            emit_is(g, is_expr, end_lbl);
        } else {
            gen_expr(g, cond, DataDest::Reg(dest));
            if idx + 1 != conditions_len {
                g.builder.emit_jump_if_false(dest, end_lbl);
            }
        }
    }

    g.builder.bind_label(end_lbl);

    g.pop_scope();

    dest
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
