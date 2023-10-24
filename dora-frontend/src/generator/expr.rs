use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, EnumId, FunctionId, Intrinsic, Label, Register,
};
use dora_parser::ast::{self, CmpOp};

use crate::generator::{bty_array_from_ty, register_bty_from_ty, AstBytecodeGen, DataDest};
use crate::sema::{EnumDefinitionId, FctDefinition, FctParent, IdentType, Sema};
use crate::ty::SourceType;

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

    let callee = &g.sa.fcts[callee_id];

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
            let impl_ = &sa.impls[impl_id];
            impl_.trait_id() == sa.known.traits.comparable()
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
    let enum_ty = g.ty(node.expr.id());
    let enum_id = enum_ty.enum_id().expect("enum expected");

    let dest = if result_ty.is_unit() {
        None
    } else {
        let result_bc_ty = register_bty_from_ty(result_ty);
        let dest = g.ensure_register(dest, result_bc_ty);
        Some(dest)
    };

    let end_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, &node.expr, DataDest::Alloc);

    let variant_reg = g.alloc_temp(BytecodeType::Int32);
    let idx = g.builder.add_const_enum(
        EnumId(enum_id.index().try_into().expect("overflow")),
        bty_array_from_ty(&enum_ty.type_params()),
    );
    g.builder
        .emit_load_enum_variant(variant_reg, expr_reg, idx, g.loc(node.span));

    let mut labels = Vec::with_capacity(node.cases.len());

    for case in &node.cases {
        for _pattern in &case.patterns {
            labels.push(g.builder.create_label());
        }
    }

    let mut idx = 0;

    for case in &node.cases {
        for pattern in &case.patterns {
            match pattern.data {
                ast::MatchPatternData::Underscore => {
                    g.builder.emit_jump(labels[idx]);
                }

                ast::MatchPatternData::Ident(_) => {
                    match_check_ident(g, pattern, variant_reg, labels[idx]);
                }
            }

            idx += 1;
        }
    }

    g.builder.emit_jump(end_lbl);
    idx = 0;

    for case in &node.cases {
        for pattern in &case.patterns {
            g.builder.bind_label(labels[idx]);

            match_case_body(
                g,
                case,
                pattern,
                enum_id,
                enum_ty.clone(),
                expr_reg,
                dest,
                end_lbl,
            );

            idx += 1;
        }
    }

    g.builder.bind_label(end_lbl);
    g.free_temp(variant_reg);
    g.free_if_temp(expr_reg);

    dest.unwrap_or(Register::invalid())
}

fn match_check_ident(
    g: &mut AstBytecodeGen,
    pattern: &ast::MatchPattern,
    variant_reg: Register,
    code_lbl: Label,
) {
    let variant_idx = match_variant_idx(g, pattern);

    let tmp_reg = g.alloc_temp(BytecodeType::Int32);
    let cmp_reg = g.alloc_temp(BytecodeType::Bool);
    g.builder.emit_const_int32(tmp_reg, variant_idx as i32);
    g.builder.emit_test_eq(cmp_reg, variant_reg, tmp_reg);
    g.builder.emit_jump_if_true(cmp_reg, code_lbl);
    g.free_temp(tmp_reg);
    g.free_temp(cmp_reg);
}

fn match_case_body(
    g: &mut AstBytecodeGen,
    case: &ast::MatchCaseType,
    pattern: &ast::MatchPattern,
    enum_id: EnumDefinitionId,
    enum_ty: SourceType,
    expr_reg: Register,
    dest: Option<Register>,
    end_lbl: Label,
) {
    g.push_scope();

    if let ast::MatchPatternData::Ident(ref ident) = pattern.data {
        if let Some(ref params) = ident.params {
            let variant_idx = match_variant_idx(g, pattern);

            for (subtype_idx, param) in params.iter().enumerate() {
                if let Some(_) = param.name {
                    let idx = g.builder.add_const_enum_element(
                        EnumId(enum_id.index().try_into().expect("overflow")),
                        bty_array_from_ty(&enum_ty.type_params()),
                        variant_idx,
                        subtype_idx as u32,
                    );

                    let var_id = *g.analysis.map_vars.get(param.id).unwrap();

                    let ty = g.var_ty(var_id);

                    if !ty.is_unit() {
                        let ty: BytecodeType = register_bty_from_ty(ty);
                        let var_reg = g.alloc_var(ty);

                        g.var_registers.insert(var_id, var_reg);

                        g.builder
                            .emit_load_enum_element(var_reg, expr_reg, idx, g.loc(param.span));
                    }
                }
            }
        }
    }

    if let Some(dest) = dest {
        gen_expr(g, &case.value, DataDest::Reg(dest));
    } else {
        gen_expr(g, &case.value, DataDest::Effect);
    }

    g.builder.emit_jump(end_lbl);
    g.pop_scope();
}

fn match_variant_idx(g: &AstBytecodeGen, pattern: &ast::MatchPattern) -> u32 {
    let ident_type = g.analysis.map_idents.get(pattern.id).unwrap();

    match ident_type {
        IdentType::EnumValue(_, _, variant_idx) => (*variant_idx).try_into().unwrap(),
        _ => unreachable!(),
    }
}
