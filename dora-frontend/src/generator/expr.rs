use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ConstPoolEntry, FunctionId, Location, Register,
};
use dora_parser::Span;
use dora_parser::ast::{self, Ast, AstExpr, AstId, CmpOp, SyntaxNodeBase};

use crate::expr_always_returns;
use crate::generator::{
    AstBytecodeGen, DataDest, IntrinsicInfo, Label, LoopLabels, SELF_VAR_ID,
    field_id_from_context_idx,
};
use crate::sema::{
    CallType, ClassDefinitionId, ConstDefinitionId, ContextFieldId, Element, EnumDefinitionId,
    FctDefinition, FctParent, GlobalDefinitionId, IdentType, Intrinsic, OuterContextIdx, ScopeId,
    Sema, StructDefinitionId, VarId, VarLocation, emit_as_bytecode_operation,
};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::{SourceType, SourceTypeArray};
use dora_bytecode::ClassId;
use dora_bytecode::ConstPoolIdx;

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: AstExpr, dest: DataDest) -> Register {
    match expr {
        AstExpr::Un(node) => g.visit_expr_un(node.id(), dest),
        AstExpr::Bin(node) => g.visit_expr_bin(node.id(), node.raw_node(), dest),
        AstExpr::DotExpr(node) => g.visit_expr_dot(node.id(), dest),
        AstExpr::Block(node) => g.visit_expr_block(node.id(), dest),
        AstExpr::If(node) => g.visit_expr_if(node.id(), dest),
        AstExpr::Template(node) => g.visit_expr_template(node.id(), node.raw_node(), dest),
        AstExpr::TypedExpr(node) => g.visit_expr_type_param(node.id(), dest),
        AstExpr::Path(node) => g.visit_expr_path(node.id(), dest),
        AstExpr::LitChar(node) => g.visit_expr_lit_char(node.id(), node.raw_node(), dest),
        AstExpr::LitInt(node) => g.visit_expr_lit_int(node.id(), node.raw_node(), dest, false),
        AstExpr::LitFloat(node) => g.visit_expr_lit_float(node.id(), node.raw_node(), dest),
        AstExpr::LitStr(node) => g.visit_expr_lit_string(node.id(), node.raw_node(), dest),
        AstExpr::LitBool(node) => g.visit_expr_lit_bool(node.id(), dest),
        AstExpr::Ident(node) => g.visit_expr_ident(node.id(), dest),
        AstExpr::Call(node) => g.visit_expr_call(node.id(), dest),
        AstExpr::This(node) => g.visit_expr_self(node.id(), dest),
        AstExpr::Conv(node) => g.visit_expr_conv(node.id(), dest),
        AstExpr::Is(node) => g.visit_expr_is(node.id(), dest),
        AstExpr::Tuple(node) => g.visit_expr_tuple(node.id(), dest),
        AstExpr::Paren(node) => gen_expr(g, node.expr(), dest),
        AstExpr::Match(node) => gen_match(g, node.id(), node.raw_node(), dest),
        AstExpr::Lambda(node) => g.visit_expr_lambda(node.id(), node.raw_node(), dest),
        AstExpr::For(node) => g.visit_expr_for(node.id(), dest),
        AstExpr::While(node) => g.visit_expr_while(node.id(), dest),
        AstExpr::Break(node) => g.visit_expr_break(node.id(), dest),
        AstExpr::Continue(node) => g.visit_expr_continue(node.id(), dest),
        AstExpr::Return(node) => g.visit_expr_return(node.id(), dest),
        AstExpr::MethodCallExpr(_) => unreachable!(),
        AstExpr::Error(_) => unreachable!(),
    }
}

pub(super) fn gen_expr_id(g: &mut AstBytecodeGen, expr_id: AstId, dest: DataDest) -> Register {
    let expr = g.node2::<AstExpr>(expr_id);
    gen_expr(g, expr, dest)
}

pub(super) fn gen_expr_condition(g: &mut AstBytecodeGen, expr_id: AstId, false_lbl: Label) {
    let expr = g.node(expr_id);

    if let Some(bin_expr) = expr.to_bin_and() {
        if let Some(is_expr) = g.node(bin_expr.lhs).to_is() {
            let value_reg = gen_expr_id(g, is_expr.value, DataDest::Alloc);
            let value_ty = g.ty(is_expr.value);
            g.setup_pattern_vars(is_expr.pattern);
            g.destruct_pattern(is_expr.pattern, value_reg, value_ty, Some(false_lbl));
            g.free_if_temp(value_reg);
        } else {
            let cond_reg = gen_expr_id(g, bin_expr.lhs, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, false_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr_condition(g, bin_expr.rhs, false_lbl);
    } else if let Some(is_expr) = expr.to_is() {
        let value_reg = gen_expr_id(g, is_expr.value, DataDest::Alloc);
        let value_ty = g.ty(is_expr.value);
        g.setup_pattern_vars(is_expr.pattern);
        g.destruct_pattern(is_expr.pattern, value_reg, value_ty, Some(false_lbl));
        g.free_if_temp(value_reg);
    } else {
        let cond_reg = gen_expr_id(g, expr_id, DataDest::Alloc);
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
    expr_ast_id: AstId,
    dest: Register,
    lhs_reg: Register,
    rhs_reg: Register,
    location: Location,
) {
    let call_type = g.analysis.map_calls.get(expr_ast_id).unwrap();
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
    expr_ast_id: AstId,
    node: &ast::Bin,
    cmp_op: CmpOp,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr_id(g, node.lhs, DataDest::Alloc);
    let rhs = gen_expr_id(g, node.rhs, DataDest::Alloc);

    let result = if let Some(info) = g.get_intrinsic(expr_ast_id) {
        gen_expr_bin_cmp_as_intrinsic(g, cmp_op, info.intrinsic, dest, lhs, rhs)
    } else {
        gen_expr_bin_cmp_as_method(g, expr_ast_id, node, cmp_op, dest, lhs, rhs)
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
    expr_ast_id: AstId,
    node: &ast::Bin,
    cmp_op: CmpOp,
    dest: DataDest,
    lhs: Register,
    rhs: Register,
) -> Register {
    let call_type = g.analysis.map_calls.get(expr_ast_id).unwrap();
    let callee_id = call_type.fct_id().expect("FctId missing");

    let callee = g.sa.fct(callee_id);

    let callee_idx = g.add_const_pool_entry_for_call(&callee, &call_type);

    let function_return_type: SourceType =
        g.specialize_type_for_call(call_type, callee.return_type());

    let function_return_type_bc: BytecodeType =
        g.emitter.convert_ty_reg(function_return_type.clone());

    let return_type = BytecodeType::Bool;

    let dest = g.ensure_register(dest, return_type.clone());

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
    node: &ast::Bin,
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
    node_id: AstId,
    node: &ast::Match,
    dest: DataDest,
) -> Register {
    let result_ty = g.ty(node_id);
    let expr_ty = g.ty(node.expr);

    let result_bc_ty = g.emitter.convert_ty_reg(result_ty);
    let dest = g.ensure_register(dest, result_bc_ty);

    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let expr_reg = gen_expr_id(g, node.expr, DataDest::Alloc);

    let mut arm_labels = Vec::with_capacity(node.arms.len());

    for _arm in &node.arms {
        arm_labels.push(g.builder.create_label());
    }

    arm_labels.push(fallthrough_lbl);

    for (idx, &arm_id) in node.arms.iter().enumerate() {
        let arm_lbl = arm_labels[idx];
        g.builder.bind_label(arm_lbl);

        let next_arm_lbl = arm_labels[idx + 1];

        g.push_scope();

        let arm = g.node(arm_id).as_match_arm();
        g.setup_pattern_vars(arm.pattern);
        g.destruct_pattern(arm.pattern, expr_reg, expr_ty.clone(), Some(next_arm_lbl));

        if let Some(cond) = arm.cond {
            let cond_reg = gen_expr_id(g, cond, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, next_arm_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr_id(g, arm.value, DataDest::Reg(dest));

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

pub(super) fn gen_fatal_error(g: &mut AstBytecodeGen, msg: &str, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty_reg(return_type.clone());
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

impl<'a> AstBytecodeGen<'a> {
    pub(super) fn visit_expr_for(&mut self, stmt_ast_id: AstId, _dest: DataDest) -> Register {
        self.push_scope();
        let for_type_info = self.analysis.map_fors.get(stmt_ast_id).unwrap().clone();

        let stmt = self.node2::<ast::AstFor>(stmt_ast_id);

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = gen_expr(self, stmt.expr(), DataDest::Alloc);

        let iterator_reg = if let Some((iter_fct_id, iter_type_params)) = for_type_info.iter {
            // Emit: <iterator> = <obj>.iter();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.builder.emit_push_register(object_reg);
            let fct_idx = self.builder.add_const_fct_types(
                self.emitter.convert_function_id(iter_fct_id),
                self.convert_tya(&iter_type_params),
            );
            self.builder
                .emit_invoke_direct(iterator_reg, fct_idx, self.loc_id(stmt.expr().id()));
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let lbl_cond = self.builder.define_label();
        self.builder.emit_loop_start();

        self.enter_block_context(stmt_ast_id);

        let iterator_type = for_type_info.iterator_type.clone();
        let iterator_type_params = self.convert_tya(&iterator_type.type_params());

        self.builder.emit_push_register(iterator_reg);

        let lbl_end = self.builder.create_label();

        let value_ty = for_type_info.value_type.clone();
        let option_type_params = SourceTypeArray::single(value_ty.clone());

        // Emit: <next-temp> = <iterator>.next()
        let next_result_ty = self.emitter.convert_ty_reg(for_type_info.next_type.clone());
        let next_result_reg = self.alloc_temp(next_result_ty);

        let fct_idx = self.builder.add_const_fct_types(
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

        self.builder.emit_push_register(iterator_reg);
        self.emit_invoke_direct(
            for_type_info.next_type.clone(),
            next_result_reg,
            fct_idx,
            self.loc_id(stmt.expr().id()),
        );

        // Emit: if <next-result>.isNone() then goto lbl_end
        let cond_reg = self.alloc_temp(BytecodeType::Bool);
        let fct_idx = self.builder.add_const_fct_types(
            FunctionId(
                self.sa
                    .known
                    .functions
                    .option_is_none()
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            self.convert_tya(&option_type_params),
        );
        self.builder.emit_push_register(next_result_reg);
        self.builder
            .emit_invoke_direct(cond_reg, fct_idx, self.loc_id(stmt.expr().id()));
        self.builder.emit_jump_if_true(cond_reg, lbl_end);
        self.free_temp(cond_reg);

        // Emit: <value-reg> = <next-result>.unwrap()
        if value_ty.is_unit() {
            self.free_temp(next_result_reg);
        } else {
            let value_ty = self.emitter.convert_ty_reg(value_ty);
            let value_reg = self.alloc_var(value_ty);
            let fct_idx = self.builder.add_const_fct_types(
                FunctionId(
                    self.sa
                        .known
                        .functions
                        .option_unwrap()
                        .index()
                        .try_into()
                        .expect("overflow"),
                ),
                self.convert_tya(&option_type_params),
            );
            self.builder.emit_push_register(next_result_reg);
            self.builder
                .emit_invoke_direct(value_reg, fct_idx, self.loc_id(stmt.expr().id()));
            self.free_temp(next_result_reg);

            let pattern_id = stmt.pattern().id();
            self.setup_pattern_vars(pattern_id);
            self.destruct_pattern_or_fail(pattern_id, value_reg, for_type_info.value_type);
        }

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.emit_expr_for_effect(stmt.block().id());
        self.loops.pop().unwrap();

        self.builder.emit_jump_loop(lbl_cond);
        self.builder.bind_label(lbl_end);

        self.leave_block_context(stmt_ast_id);
        self.pop_scope();

        self.free_if_temp(object_reg);
        self.ensure_unit_register()
    }

    pub(super) fn visit_stmt_let(&mut self, stmt: ast::AstLet) {
        let pattern_id = stmt.pattern().id();
        self.setup_pattern_vars(pattern_id);

        if let Some(expr) = stmt.expr() {
            let ty = self.ty(expr.id());
            let value = gen_expr(self, expr, DataDest::Alloc);
            self.destruct_pattern_or_fail(pattern_id, value, ty);
            self.free_if_temp(value);
        }
    }

    pub(super) fn visit_expr_while(&mut self, node_id: AstId, _dest: DataDest) -> Register {
        let node = self.node2::<ast::AstWhile>(node_id);
        let cond_lbl = self.builder.define_label();
        let end_lbl = self.builder.create_label();
        self.builder.emit_loop_start();
        self.enter_block_context(node_id);

        gen_expr_condition(self, node.cond().id(), end_lbl);

        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.emit_expr_for_effect(node.block().id());
        self.loops.pop().unwrap();
        self.builder.emit_jump_loop(cond_lbl);
        self.builder.bind_label(end_lbl);
        self.leave_block_context(node_id);
        self.ensure_unit_register()
    }

    pub(super) fn visit_stmt_expr(&mut self, stmt: ast::AstExprStmt) {
        let reg = gen_expr(self, stmt.expr(), DataDest::Alloc);
        self.free_if_temp(reg);
    }

    pub(super) fn visit_expr_return(&mut self, expr_id: AstId, _dest: DataDest) -> Register {
        let ret = self.node2::<ast::AstReturn>(expr_id);
        let result_reg = if let Some(expr) = ret.expr() {
            gen_expr(self, expr, DataDest::Alloc)
        } else {
            self.ensure_unit_register()
        };

        self.builder.emit_ret(result_reg);
        self.free_if_temp(result_reg);

        self.ensure_unit_register()
    }

    pub(super) fn visit_expr_break(&mut self, _id: AstId, _dest: DataDest) -> Register {
        let end = self.loops.last().unwrap().end;
        self.builder.emit_jump(end);
        self.ensure_unit_register()
    }

    pub(super) fn visit_expr_continue(&mut self, _id: AstId, _dest: DataDest) -> Register {
        let cond = self.loops.last().unwrap().cond;
        self.builder.emit_jump_loop(cond);
        self.ensure_unit_register()
    }

    pub(super) fn emit_expr_for_effect(&mut self, expr_id: AstId) {
        let reg = gen_expr_id(self, expr_id, DataDest::Alloc);
        self.free_if_temp(reg);
    }

    pub(super) fn visit_expr_type_param(&mut self, expr_id: AstId, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr_id).cloned().unwrap();
        let expr = self.node2::<ast::AstTypedExpr>(expr_id);

        match ident_type {
            IdentType::EnumVariant(enum_id, type_params, variant_idx) => self.emit_new_enum(
                enum_id,
                type_params,
                variant_idx,
                self.loc(expr.span()),
                dest,
            ),

            _ => unreachable!(),
        }
    }

    pub(super) fn visit_expr_template(
        &mut self,
        _id: AstId,
        expr: &ast::Template,
        dest: DataDest,
    ) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.sa.known.functions.string_buffer_empty();
        let fct_idx = self
            .builder
            .add_const_fct(self.emitter.convert_function_id(fct_id));
        self.builder
            .emit_invoke_static(buffer_register, fct_idx, self.loc(expr.span));

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for &part_id in &expr.parts {
            let part = self.node(part_id);

            if let Some(..) = part.to_lit_str() {
                let value = self
                    .analysis
                    .const_value(part_id)
                    .to_string()
                    .expect("string expected")
                    .to_string();
                self.builder.emit_const_string(part_register, value);
            } else {
                let ty = self.ty(part_id);

                if ty.cls_id() == Some(self.sa.known.classes.string()) {
                    gen_expr_id(self, part_id, DataDest::Reg(part_register));
                } else if ty.is_type_param() {
                    let type_list_id = match ty {
                        SourceType::TypeParam(id) => id,
                        _ => unreachable!(),
                    };

                    let expr_register = gen_expr_id(self, part_id, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let trait_id = self.sa.known.traits.stringable();
                    let trait_ = self.sa.trait_(trait_id);
                    let to_string_id = trait_
                        .get_method(name, false)
                        .expect("Stringable::toString() not found");

                    let fct_idx = self.builder.add_const(ConstPoolEntry::Generic(
                        type_list_id.index() as u32,
                        self.emitter.convert_function_id(to_string_id),
                        BytecodeTypeArray::empty(),
                        BytecodeTypeArray::empty(),
                    ));

                    self.builder.emit_invoke_generic_direct(
                        part_register,
                        fct_idx,
                        self.loc(part.span()),
                    );

                    self.free_if_temp(expr_register);
                } else {
                    let expr_register = gen_expr_id(self, part_id, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let (to_string_id, type_params) = self
                        .analysis
                        .map_templates
                        .get(part_id)
                        .expect("missing toString id");

                    let type_params = self.convert_tya(&type_params);

                    let fct_idx = self.builder.add_const_fct_types(
                        self.emitter.convert_function_id(*to_string_id),
                        type_params,
                    );
                    self.builder
                        .emit_invoke_direct(part_register, fct_idx, self.loc(part.span()));

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.sa.known.functions.string_buffer_append();
            let fct_idx = self
                .builder
                .add_const_fct(self.emitter.convert_function_id(fct_id));
            self.builder.emit_push_register(buffer_register);
            self.builder.emit_push_register(part_register);
            let dest_reg = self.ensure_unit_register();
            self.builder
                .emit_invoke_direct(dest_reg, fct_idx, self.loc(expr.span));
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.sa.known.functions.string_buffer_to_string();
        let fct_idx = self
            .builder
            .add_const_fct(self.emitter.convert_function_id(fct_id));
        self.builder.emit_push_register(buffer_register);
        self.builder
            .emit_invoke_direct(buffer_register, fct_idx, self.loc(expr.span));

        buffer_register
    }

    pub(super) fn visit_expr_path(&mut self, expr_id: AstId, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr_id).cloned().unwrap();
        let expr = self.node2::<ast::AstPath>(expr_id);

        match ident_type {
            IdentType::EnumVariant(enum_id, type_params, variant_idx) => self.emit_new_enum(
                enum_id,
                type_params,
                variant_idx,
                self.loc(expr.span()),
                dest,
            ),

            IdentType::Const(const_id) => self.visit_expr_ident_const(const_id, dest),

            IdentType::Global(global_id) => {
                self.visit_expr_ident_global(global_id, dest, self.loc(expr.span()))
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

    pub(super) fn emit_new_enum(
        &mut self,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant_idx: u32,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let type_params = self.convert_tya(&type_params);
        let enum_id = self.emitter.convert_enum_id(enum_id);
        let bty = BytecodeType::Enum(enum_id, type_params.clone());
        let dest = self.ensure_register(dest, bty);
        let idx = self
            .builder
            .add_const_enum_variant(enum_id, type_params, variant_idx);
        self.builder.emit_new_enum(dest, idx, location);
        dest
    }

    pub(super) fn visit_expr_conv(&mut self, _id: AstId, dest: DataDest) -> Register {
        let expr = self.node2::<ast::AstConv>(_id);
        let object_type = self.ty(expr.object().id());
        let check_type = self.ty(expr.data_type().id());
        assert!(check_type.is_trait_object());

        let check_type = self.emitter.convert_ty(check_type);

        let object = gen_expr(self, expr.object(), DataDest::Alloc);
        let idx = self
            .builder
            .add_const_trait(check_type.clone(), self.emitter.convert_ty(object_type));
        let dest = self.ensure_register(dest, check_type);
        self.builder
            .emit_new_trait_object(dest, idx, object, self.loc(expr.span()));
        self.free_if_temp(object);
        dest
    }

    pub(super) fn visit_expr_is(&mut self, _id: AstId, dest: DataDest) -> Register {
        let node = self.node2::<ast::AstIs>(_id);
        let ty = self.ty(node.value().id());
        let value_reg = gen_expr(self, node.value(), DataDest::Alloc);

        self.push_scope();
        let mismatch_lbl = self.builder.create_label();
        let merge_lbl = self.builder.create_label();
        self.destruct_pattern(node.pattern().id(), value_reg, ty, Some(mismatch_lbl));
        let dest = self.ensure_register(dest, BytecodeType::Bool);
        self.builder.emit_const_true(dest);
        self.builder.emit_jump(merge_lbl);
        self.builder.bind_label(mismatch_lbl);
        self.builder.emit_const_false(dest);
        self.builder.bind_label(merge_lbl);
        self.pop_scope();

        self.free_if_temp(value_reg);

        dest
    }

    pub(super) fn last_context_register(&self) -> Option<Register> {
        self.entered_contexts
            .iter()
            .rev()
            .find_map(|ec| ec.register)
    }

    pub(super) fn visit_expr_lambda(
        &mut self,
        node_id: AstId,
        node: &ast::Lambda,
        dest: DataDest,
    ) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Ptr);

        let fct_node_id = node.fct_id;
        let node = self
            .sa
            .file(self.file_id)
            .node(fct_node_id)
            .to_function()
            .expect("fct expected");

        let lambda_fct_id = self
            .analysis
            .map_lambdas
            .get(node_id)
            .expect("missing lambda id")
            .fct_id();

        let lambda_fct = self.sa.fct(lambda_fct_id);
        let lambda_analysis = lambda_fct.analysis();

        let mut outer_context_reg: Option<Register> = None;

        if lambda_analysis.needs_context_slot_in_lambda_object() {
            if let Some(context_register) = self.last_context_register() {
                self.builder.emit_push_register(context_register.clone());
            } else {
                // This lambda doesn't have a context object on its own, simply
                // pass down the parent context (the context in the lambda object).
                assert!(self.is_lambda);
                assert!(self.analysis.needs_context_slot_in_lambda_object());
                outer_context_reg = Some(self.alloc_temp(BytecodeType::Ptr));
                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    self.emitter.convert_class_id(lambda_cls_id),
                    BytecodeTypeArray::empty(),
                    0,
                );
                self.builder.emit_load_field(
                    outer_context_reg.expect("missing reg"),
                    self.var_reg(SELF_VAR_ID),
                    idx,
                    self.loc(node.span),
                );
                self.builder
                    .emit_push_register(outer_context_reg.expect("missing reg"));
            }
        }

        let idx = self.builder.add_const_fct_types(
            self.emitter.convert_function_id(lambda_fct_id),
            self.convert_tya(&self.identity_type_params()),
        );
        self.builder.emit_new_lambda(dest, idx, self.loc(node.span));

        if let Some(outer_context_reg) = outer_context_reg {
            self.free_if_temp(outer_context_reg);
        }

        dest
    }

    pub(super) fn visit_expr_if(&mut self, expr_id: AstId, dest: DataDest) -> Register {
        let expr = self.node2::<ast::AstIf>(expr_id);
        let ty = self.ty(expr_id);

        let dest = self.ensure_register(dest, self.emitter.convert_ty_reg(ty));
        let else_lbl = self.builder.create_label();

        self.push_scope();

        gen_expr_condition(self, expr.cond().id(), else_lbl);

        gen_expr(self, expr.then_block(), DataDest::Reg(dest));

        self.pop_scope();

        if let Some(else_block) = expr.else_block() {
            let end_lbl = self.builder.create_label();

            if !expr_always_returns(&self.sa.file(self.file_id).ast(), expr.then_block().id()) {
                self.builder.emit_jump(end_lbl);
            }

            self.builder.bind_label(else_lbl);
            gen_expr(self, else_block, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);
        } else {
            self.builder.bind_label(else_lbl);
        }

        dest
    }

    pub(super) fn visit_expr_block(&mut self, block_id: AstId, dest: DataDest) -> Register {
        let block = self.node2::<ast::AstBlock>(block_id);
        self.push_scope();

        for stmt in block.stmts() {
            self.visit_stmt(stmt.id());
        }

        let result = if let Some(expr) = block.expr() {
            gen_expr(self, expr, dest)
        } else {
            self.ensure_unit_register()
        };

        self.pop_scope();

        result
    }

    pub(super) fn visit_expr_dot(&mut self, expr_id: AstId, dest: DataDest) -> Register {
        let expr = self.node2::<ast::AstDotExpr>(expr_id);
        let object_ty = self.ty(expr.lhs().id());

        if object_ty.is_tuple() {
            return self.visit_expr_dot_tuple(expr, object_ty, dest);
        }

        if let Some((struct_id, type_params)) = object_ty.to_struct() {
            return self.visit_expr_dot_struct(expr_id, expr, struct_id, type_params, dest);
        }

        let (cls_ty, field_id) = {
            let ident_type = self.analysis.map_idents.get(expr_id).unwrap();

            match ident_type {
                IdentType::Field(ty, field) => (ty.clone(), *field),
                _ => unreachable!(),
            }
        };

        let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

        let field_idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&type_params),
            field_id.0 as u32,
        );

        let field_ty = self.ty(expr_id);

        let field_bc_ty: BytecodeType = self.emitter.convert_ty_reg(field_ty);
        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = gen_expr(self, expr.lhs(), DataDest::Alloc);

        self.builder
            .emit_load_field(dest, obj, field_idx, self.loc(expr.op_span()));
        self.free_if_temp(obj);

        dest
    }

    pub(super) fn visit_expr_dot_struct(
        &mut self,
        expr_id: AstId,
        expr: ast::AstDotExpr,
        struct_id: StructDefinitionId,
        type_params: SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let struct_obj = gen_expr(self, expr.lhs(), DataDest::Alloc);

        let ident_type = self.analysis.map_idents.get(expr_id).unwrap();

        let field_idx = match ident_type {
            IdentType::StructField(_, field_idx) => *field_idx,
            _ => unreachable!(),
        };

        let fty = self.ty(expr_id);

        if fty.is_unit() {
            self.free_if_temp(struct_obj);
            return self.ensure_unit_register();
        }

        let ty: BytecodeType = self.emitter.convert_ty_reg(fty);
        let dest = self.ensure_register(dest, ty);
        let const_idx = self.builder.add_const_struct_field(
            self.emitter.convert_struct_id(struct_id),
            self.convert_tya(&type_params),
            field_idx.0 as u32,
        );
        self.builder
            .emit_load_struct_field(dest, struct_obj, const_idx);

        self.free_if_temp(struct_obj);

        dest
    }

    pub(super) fn visit_expr_dot_tuple(
        &mut self,
        expr: ast::AstDotExpr,
        tuple_ty: SourceType,
        dest: DataDest,
    ) -> Register {
        let tuple = gen_expr(self, expr.lhs(), DataDest::Alloc);
        let value_i64 = self
            .analysis
            .const_value(expr.rhs().id())
            .to_i64()
            .expect("integer expected");
        let idx: u32 = value_i64.try_into().expect("too large");

        let subtypes: SourceTypeArray = tuple_ty.tuple_subtypes().expect("tuple expected");
        let ty = subtypes[idx as usize].clone();

        let ty: BytecodeType = self.emitter.convert_ty_reg(ty);
        let dest = self.ensure_register(dest, ty);
        let idx = self
            .builder
            .add_const_tuple_element(self.emitter.convert_ty(tuple_ty), idx);
        self.builder.emit_load_tuple_element(dest, tuple, idx);

        self.free_if_temp(tuple);

        dest
    }

    pub(super) fn visit_expr_assert(&mut self, expr_id: AstId, _dest: DataDest) -> Register {
        let expr = self.node2::<ast::AstCall>(expr_id);
        let argument_list = expr.arg_list();
        let arg = argument_list.items().next().expect("missing argument");
        let assert_reg = gen_expr(self, arg.expr(), DataDest::Alloc);
        self.builder.emit_push_register(assert_reg);
        let fid = self.sa.known.functions.assert();
        let idx = self
            .builder
            .add_const_fct(self.emitter.convert_function_id(fid));
        let dest = self.ensure_unit_register();
        self.builder
            .emit_invoke_static(dest, idx, self.loc(expr.span()));
        self.free_if_temp(assert_reg);
        dest
    }

    pub(super) fn visit_expr_call(&mut self, node_id: AstId, dest: DataDest) -> Register {
        let node = self.node2::<ast::AstCall>(node_id);
        if let Some(info) = self.get_intrinsic(node_id) {
            if emit_as_bytecode_operation(info.intrinsic) {
                return self.visit_expr_call_intrinsic(node_id, node.raw_node(), info, dest);
            }
        }

        let call_type = self.analysis.map_calls.get(node_id).unwrap().clone();

        match *call_type {
            CallType::NewEnum(ref enum_ty, variant_idx) => {
                return self.visit_expr_call_enum(node.clone(), enum_ty.clone(), variant_idx, dest);
            }

            CallType::NewStruct(struct_id, ref type_params) => {
                return self.visit_expr_call_struct(node.clone(), struct_id, type_params, dest);
            }

            CallType::NewClass(cls_id, ref type_params) => {
                return self.visit_expr_call_class(
                    node_id,
                    node.clone(),
                    cls_id,
                    type_params,
                    dest,
                );
            }

            CallType::Lambda(ref params, ref return_type) => {
                return self.visit_expr_call_lambda(
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
        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, _return_type) = self.determine_callee_types(&call_type, &*callee);
        let return_type = self.analysis.ty(node_id);

        // Allocate register for result
        let return_reg =
            self.ensure_register(dest, self.emitter.convert_ty_reg(return_type.clone()));

        // Evaluate object/self argument
        let object_argument = self.emit_call_object_argument(node.clone(), &call_type);

        // Evaluate function arguments
        let arguments = self.emit_call_arguments(node.clone(), &*callee, &call_type, &arg_types);

        if let Some(obj_reg) = object_argument {
            self.builder.emit_push_register(obj_reg);
        }
        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(return_reg, callee_idx, &call_type, self.loc(node.span()));

        if let Some(obj_reg) = object_argument {
            self.free_if_temp(obj_reg);
        }

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        return_reg
    }

    pub(super) fn visit_expr_call_enum(
        &mut self,
        expr: ast::AstCall,
        enum_ty: SourceType,
        variant_idx: u32,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();
        let argument_list = expr.arg_list();

        for arg in argument_list.items() {
            arguments.push(gen_expr(self, arg.expr(), DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let (enum_id, type_params) = enum_ty.to_enum().expect("enum expected");

        let idx = self.builder.add_const_enum_variant(
            self.emitter.convert_enum_id(enum_id),
            self.convert_tya(&type_params),
            variant_idx,
        );
        let bytecode_ty = self.emitter.convert_ty_reg(enum_ty);
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder
            .emit_new_enum(dest_reg, idx, self.loc(expr.span()));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    pub(super) fn visit_expr_call_lambda(
        &mut self,
        node: ast::AstCall,
        params: SourceTypeArray,
        return_type: SourceType,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        let lambda_object = gen_expr(self, node.callee(), DataDest::Alloc);
        arguments.push(lambda_object);

        let argument_list = node.arg_list();

        for arg in argument_list.items() {
            arguments.push(gen_expr(self, arg.expr(), DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let idx = self.builder.add_const_lambda(
            self.convert_tya(&params),
            self.emitter.convert_ty(return_type.clone()),
        );

        let dest_reg = if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_lambda(dest, idx, self.loc(node.span()));
            dest
        } else {
            let bytecode_ty = self.emitter.convert_ty_reg(return_type);
            let dest_reg = self.ensure_register(dest, bytecode_ty);
            self.builder
                .emit_invoke_lambda(dest_reg, idx, self.loc(node.span()));
            dest_reg
        };

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    pub(super) fn visit_expr_call_struct(
        &mut self,
        expr: ast::AstCall,
        struct_id: StructDefinitionId,
        type_params: &SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();
        let argument_list = expr.arg_list();

        for arg in argument_list.items() {
            arguments.push(gen_expr(self, arg.expr(), DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let struct_id = self.emitter.convert_struct_id(struct_id);

        let idx = self
            .builder
            .add_const_struct(struct_id, self.convert_tya(&type_params));
        let bytecode_ty = BytecodeType::Struct(struct_id, self.convert_tya(type_params));
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder
            .emit_new_struct(dest_reg, idx, self.loc(expr.span()));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    pub(super) fn visit_expr_call_class(
        &mut self,
        _node_id: AstId,
        node: ast::AstCall,
        cls_id: ClassDefinitionId,
        type_params: &SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let argument_list = node.arg_list();
        let mut arguments: Vec<Option<Register>> = vec![None; argument_list.items_len()];

        for arg in argument_list.items() {
            let reg = gen_expr(self, arg.expr(), DataDest::Alloc);
            let target_idx = self
                .analysis
                .map_argument
                .get(arg.id())
                .expect("missing argument idx")
                .clone();

            arguments[target_idx] = Some(reg);
        }

        for &arg_reg in &arguments {
            let arg_reg = arg_reg.expect("missing register");
            self.builder.emit_push_register(arg_reg);
        }

        let cls_id = self.emitter.convert_class_id(cls_id);
        let idx = self
            .builder
            .add_const_cls_types(cls_id, self.convert_tya(type_params));
        let dest_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_object_initialized(dest_reg, idx, self.loc(node.span()));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg.expect("missing register"));
        }

        dest_reg
    }

    pub(super) fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &FctDefinition,
    ) -> (Vec<SourceType>, SourceType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type());

        let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

        if fct.has_hidden_self_argument() {
            let self_type = match call_type {
                CallType::TraitObjectMethod(trait_ty, _) => {
                    // trait methods use Self as type for self argument but specialize_type_for_call can't handle Self.
                    assert!(fct.params_with_self()[0].ty().is_self() && !fct.is_static);
                    trait_ty.clone()
                }
                _ => {
                    let arg = fct.params_with_self()[0].ty().clone();
                    self.specialize_type_for_call(&call_type, arg.clone())
                }
            };

            arg_types.push(self_type);
        }

        for arg in fct.params_without_self() {
            let arg = self.specialize_type_for_call(&call_type, arg.ty());
            arg_types.push(arg);
        }

        (arg_types, return_type)
    }

    pub(super) fn emit_call_object_argument(
        &mut self,
        expr: ast::AstCall,
        call_type: &CallType,
    ) -> Option<Register> {
        match *call_type {
            CallType::Method(..)
            | CallType::GenericMethod(..)
            | CallType::GenericMethodSelf(..)
            | CallType::GenericMethodNew { .. }
            | CallType::TraitObjectMethod(..) => {
                let obj_expr = expr
                    .raw_node()
                    .object(self.ast_file())
                    .unwrap_or_else(|| expr.callee().id());
                let reg = gen_expr_id(self, obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _, _) => Some(gen_expr(self, expr.callee(), DataDest::Alloc)),
            CallType::GenericStaticMethod(..)
            | CallType::GenericStaticMethodSelf(..)
            | CallType::Fct(..) => None,
            _ => panic!("unexpected call type {:?}", call_type),
        }
    }

    pub(super) fn emit_call_arguments(
        &mut self,
        expr: ast::AstCall,
        callee: &FctDefinition,
        call_type: &CallType,
        arg_types: &[SourceType],
    ) -> Vec<Register> {
        let mut registers = Vec::new();

        // self was already emitted, needs to be ignored here.
        let arg_start_offset = match *call_type {
            CallType::Expr(..) | CallType::Method(..) | CallType::GenericMethod(..) => 1,
            _ => 0,
        };

        // Calculate number of non-variadic arguments
        let non_variadic_arguments = if callee.params.is_variadic() {
            arg_types.len() - arg_start_offset - 1
        } else {
            arg_types.len()
        };

        let argument_list = expr.arg_list();

        // Evaluate non-variadic arguments and track registers.
        for arg in argument_list.items().take(non_variadic_arguments) {
            let reg = gen_expr(self, arg.expr(), DataDest::Alloc);
            registers.push(reg);
        }

        if callee.params.is_variadic() {
            let array_reg = self.emit_array_with_variadic_arguments(
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
        &mut self,
        expr: ast::AstCall,
        arg_types: &[SourceType],
        non_variadic_arguments: usize,
        dest: DataDest,
    ) -> Register {
        let argument_list = expr.arg_list();
        let variadic_arguments = argument_list.items_len() - non_variadic_arguments;

        // We need array of elements
        let element_ty = arg_types.last().cloned().unwrap();
        let ty = self.sa.known.array_ty(element_ty.clone());
        let (cls_id, type_params) = ty.to_class().expect("class expected");
        let cls_idx = self.builder.add_const_cls_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&type_params),
        );

        // Store length in a register
        let length_reg = self.alloc_temp(BytecodeType::Int64);
        self.builder
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_array(array_reg, length_reg, cls_idx, self.loc(expr.span()));

        let index_reg = self.alloc_temp(BytecodeType::Int64);

        // Evaluate rest arguments and store them in array
        for (idx, arg) in argument_list
            .items()
            .skip(non_variadic_arguments)
            .enumerate()
        {
            let arg_reg = gen_expr(self, arg.expr(), DataDest::Alloc);
            self.builder.emit_const_int64(index_reg, idx as i64);
            self.builder
                .emit_store_array(arg_reg, array_reg, index_reg, self.loc(expr.span()));
            self.free_if_temp(arg_reg);
        }

        self.free_if_temp(index_reg);
        self.free_if_temp(length_reg);

        array_reg
    }

    pub(super) fn emit_call_inst(
        &mut self,
        return_reg: Register,
        callee_idx: ConstPoolIdx,
        call_type: &CallType,
        location: Location,
    ) {
        match *call_type {
            CallType::Method(..) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::Fct(..) => {
                self.builder
                    .emit_invoke_static(return_reg, callee_idx, location);
            }
            CallType::Expr(..) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::TraitObjectMethod(..) => {
                self.builder
                    .emit_invoke_virtual(return_reg, callee_idx, location);
            }
            CallType::GenericMethod(..)
            | CallType::GenericMethodSelf(..)
            | CallType::GenericMethodNew { .. } => {
                self.builder
                    .emit_invoke_generic_direct(return_reg, callee_idx, location);
            }
            CallType::GenericStaticMethod(..) | CallType::GenericStaticMethodSelf(..) => {
                self.builder
                    .emit_invoke_generic_static(return_reg, callee_idx, location);
            }
            CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Intrinsic(..)
            | CallType::Lambda(..) => unreachable!(),
        }
    }

    pub(super) fn emit_mov(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.builder.emit_mov(dest, src);
        }
    }

    pub(super) fn emit_invoke_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        location: Location,
    ) {
        if return_type.is_unit() {
            let reg = self.ensure_unit_register();
            self.builder.emit_invoke_direct(reg, callee_id, location);
        } else {
            self.builder
                .emit_invoke_direct(return_reg, callee_id, location);
        }
    }

    pub(super) fn emit_invoke_generic_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        location: Location,
    ) {
        if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_generic_direct(dest, callee_id, location);
        } else {
            self.builder
                .emit_invoke_generic_direct(return_reg, callee_id, location);
        }
    }

    pub(super) fn visit_expr_self(&mut self, expr_id: AstId, dest: DataDest) -> Register {
        let expr = self.node2::<ast::AstThis>(expr_id);
        if self.is_lambda {
            let ident = self
                .analysis
                .map_idents
                .get(expr_id)
                .expect("missing ident");
            let (level, context_idx) = match ident {
                IdentType::Context(level, context_idx) => (*level, *context_idx),
                _ => unreachable!(),
            };
            self.visit_expr_ident_context(level, context_idx, dest, self.loc(expr.span()))
        } else {
            let var_reg = self.var_reg(SELF_VAR_ID);

            if dest.is_alloc() {
                return var_reg;
            }

            let dest = dest.reg();

            self.emit_mov(dest, var_reg);

            dest
        }
    }

    pub(super) fn visit_expr_lit_char(
        &mut self,
        node_id: AstId,
        _node: &ast::LitChar,
        dest: DataDest,
    ) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Char);

        let value = self.analysis.const_value(node_id).to_char();
        self.builder.emit_const_char(dest, value);

        dest
    }

    pub(super) fn visit_expr_lit_int(
        &mut self,
        node_id: AstId,
        _node: &ast::LitInt,
        dest: DataDest,
        _neg: bool,
    ) -> Register {
        let ty = self.analysis.ty(node_id);
        let value = self.analysis.const_value(node_id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => {
                let dest = self.ensure_register(dest, BytecodeType::Float32);
                self.builder
                    .emit_const_float32(dest, value.to_f64().expect("float expected") as f32);
                return dest;
            }
            SourceType::Float64 => {
                let dest = self.ensure_register(dest, BytecodeType::Float64);
                self.builder
                    .emit_const_float64(dest, value.to_f64().expect("float expected"));
                return dest;
            }
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());
        let value_i64 = value.to_i64().expect("integer expected");

        match ty {
            BytecodeType::UInt8 => self.builder.emit_const_uint8(dest, value_i64 as u8),
            BytecodeType::Int32 => self.builder.emit_const_int32(dest, value_i64 as i32),
            BytecodeType::Int64 => self.builder.emit_const_int64(dest, value_i64),
            _ => unreachable!(),
        }

        dest
    }

    pub(super) fn visit_expr_lit_float(
        &mut self,
        node_id: AstId,
        _node: &ast::LitFloat,
        dest: DataDest,
    ) -> Register {
        let ty = self.analysis.ty(node_id);
        let value_f64 = self
            .analysis
            .const_value(node_id)
            .to_f64()
            .expect("float expected");

        let ty = match ty {
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::Float32 => self.builder.emit_const_float32(dest, value_f64 as f32),
            BytecodeType::Float64 => self.builder.emit_const_float64(dest, value_f64),
            _ => unreachable!(),
        }

        dest
    }

    pub(super) fn visit_expr_lit_string(
        &mut self,
        node_id: AstId,
        _lit: &ast::LitStr,
        dest: DataDest,
    ) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Ptr);
        let value = self
            .analysis
            .const_value(node_id)
            .to_string()
            .expect("string expected")
            .to_string();
        self.builder.emit_const_string(dest, value);

        dest
    }

    pub(super) fn visit_expr_lit_bool(&mut self, id: AstId, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Bool);
        let lit = self.node2::<ast::AstLitBool>(id);

        if lit.value() {
            self.builder.emit_const_true(dest);
        } else {
            self.builder.emit_const_false(dest);
        }

        dest
    }

    pub(super) fn visit_expr_tuple(&mut self, node_id: AstId, dest: DataDest) -> Register {
        let e = self.node2::<ast::AstTuple>(node_id);

        if e.values_len() == 0 {
            return self.ensure_unit_register();
        }

        let ty = self.ty(node_id);

        let result_ty: BytecodeType = self.emitter.convert_ty_reg(ty.clone());
        let result = self.ensure_register(dest, result_ty);

        let mut values = Vec::with_capacity(e.values_len());

        for value in e.values() {
            let value_id = value.id();
            let value_ty = self.ty(value_id);
            let reg = gen_expr(self, value, DataDest::Alloc);

            if !value_ty.is_unit() {
                values.push(reg);
            }
        }

        for &value in &values {
            self.builder.emit_push_register(value);
        }

        let subtypes = ty.tuple_subtypes().expect("tuple expected");
        let idx = self.builder.add_const_tuple(self.convert_tya(&subtypes));
        self.builder.emit_new_tuple(result, idx, self.loc(e.span()));

        for arg_reg in values {
            self.free_if_temp(arg_reg);
        }

        result
    }

    pub(super) fn visit_expr_un(&mut self, node_id: AstId, dest: DataDest) -> Register {
        let node = self.node2::<ast::AstUn>(node_id);
        let opnd_id = node.opnd().id();
        if node.op() == ast::UnOp::Neg && self.node(opnd_id).is_lit_int() {
            let lit = self.node(opnd_id).as_lit_int();
            self.visit_expr_lit_int(opnd_id, lit, dest, true)
        } else if let Some(intrinsic) = self.get_intrinsic(node_id) {
            self.emit_intrinsic_un(opnd_id, intrinsic, self.loc(node.span()), dest)
        } else {
            self.visit_expr_un_method(node_id, node, dest)
        }
    }

    pub(super) fn visit_expr_un_method(
        &mut self,
        node_id: AstId,
        node: ast::AstUn,
        dest: DataDest,
    ) -> Register {
        let opnd = gen_expr(self, node.opnd(), DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(node_id).unwrap();
        let callee_id = call_type.fct_id().expect("FctId missing");

        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type());

        let function_return_type_bc: BytecodeType =
            self.emitter.convert_ty_reg(function_return_type.clone());
        let dest = self.ensure_register(dest, function_return_type_bc);

        self.builder.emit_push_register(opnd);

        if call_type.is_generic_method() {
            self.emit_invoke_generic_direct(
                function_return_type,
                dest,
                callee_idx,
                self.loc(node.span()),
            );
        } else {
            self.emit_invoke_direct(
                function_return_type,
                dest,
                callee_idx,
                self.loc(node.span()),
            );
        }

        self.free_if_temp(opnd);

        dest
    }

    pub(super) fn visit_expr_bin(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        dest: DataDest,
    ) -> Register {
        if expr.op.is_any_assign() {
            self.visit_expr_assign(expr_ast_id, expr, dest)
        } else if let ast::BinOp::Cmp(cmp_op) = expr.op {
            if cmp_op == CmpOp::Is || cmp_op == CmpOp::IsNot {
                self.emit_bin_is(expr, dest)
            } else {
                gen_expr_bin_cmp(self, expr_ast_id, expr, cmp_op, dest)
            }
        } else if expr.op == ast::BinOp::Or {
            self.emit_bin_or(expr, dest)
        } else if expr.op == ast::BinOp::And {
            self.emit_bin_and(expr, dest)
        } else if let Some(info) = self.get_intrinsic(expr_ast_id) {
            self.emit_intrinsic_bin(expr.lhs, expr.rhs, info, self.loc(expr.span), dest)
        } else {
            self.visit_expr_bin_method(expr_ast_id, expr, dest)
        }
    }

    pub(super) fn visit_expr_bin_method(
        &mut self,
        node_id: AstId,
        node: &ast::Bin,
        dest: DataDest,
    ) -> Register {
        let lhs = gen_expr_id(self, node.lhs, DataDest::Alloc);
        let rhs = gen_expr_id(self, node.rhs, DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(node_id).unwrap();
        let callee_id = call_type.fct_id().expect("FctId missing");

        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type());

        let function_return_type_bc: BytecodeType =
            self.emitter.convert_ty_reg(function_return_type.clone());

        let return_type = match node.op {
            ast::BinOp::Cmp(_) => BytecodeType::Bool,
            _ => function_return_type_bc.clone(),
        };

        let dest = self.ensure_register(dest, return_type.clone());

        let result = if function_return_type_bc == return_type {
            dest
        } else {
            let function_result_register_ty: BytecodeType =
                self.emitter.convert_ty_reg(function_return_type.clone());
            self.alloc_temp(function_result_register_ty)
        };

        self.builder.emit_push_register(lhs);
        self.builder.emit_push_register(rhs);

        if call_type.is_generic_method() {
            self.emit_invoke_generic_direct(
                function_return_type,
                result,
                callee_idx,
                self.loc(node.span),
            );
        } else {
            self.emit_invoke_direct(
                function_return_type,
                result,
                callee_idx,
                self.loc(node.span),
            );
        }

        self.free_if_temp(lhs);
        self.free_if_temp(rhs);

        match node.op {
            ast::BinOp::Cmp(ast::CmpOp::Eq) => assert_eq!(result, dest),
            ast::BinOp::Cmp(ast::CmpOp::Ne) => {
                assert_eq!(result, dest);
                self.builder.emit_not(dest, dest);
            }

            ast::BinOp::Cmp(op) => {
                assert_ne!(result, dest);
                let zero = self.alloc_temp(BytecodeType::Int32);
                self.builder.emit_const_int32(zero, 0);

                match op {
                    ast::CmpOp::Lt => self.builder.emit_test_lt(dest, result, zero),
                    ast::CmpOp::Le => self.builder.emit_test_le(dest, result, zero),
                    ast::CmpOp::Gt => self.builder.emit_test_gt(dest, result, zero),
                    ast::CmpOp::Ge => self.builder.emit_test_ge(dest, result, zero),
                    ast::CmpOp::Eq | ast::CmpOp::Ne | ast::CmpOp::Is | ast::CmpOp::IsNot => {
                        unreachable!()
                    }
                }

                self.free_temp(zero);
                self.free_temp(result);
            }
            _ => assert_eq!(result, dest),
        }

        dest
    }

    pub(super) fn visit_expr_call_intrinsic(
        &mut self,
        node_id: AstId,
        node: &ast::Call,
        info: IntrinsicInfo,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;
        let call_type = self.analysis.map_calls.get(node_id).unwrap().clone();

        let argument_list = self.node(node.arg_list).as_argument_list();

        if call_type.is_method() {
            let object = node.object(self.ast_file()).unwrap();

            match argument_list.items.len() {
                0 => self.emit_intrinsic_un(object, info, self.loc(node.span), dest),
                1 => self.emit_intrinsic_bin(
                    object,
                    self.node(argument_list.items[0])
                        .to_argument()
                        .expect("argument expecteds")
                        .expr,
                    info,
                    self.loc(node.span),
                    dest,
                ),
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        node.object(self.ast_file()).unwrap(),
                        self.node(argument_list.items[0])
                            .to_argument()
                            .expect("argument expecteds")
                            .expr,
                        self.node(argument_list.items[1])
                            .to_argument()
                            .expect("argument expected")
                            .expr,
                        self.loc(node.span),
                        dest,
                    )
                }
                _ => unreachable!(),
            }
        } else {
            match intrinsic {
                Intrinsic::Assert => self.visit_expr_assert(node_id, dest),

                Intrinsic::ArrayGet => self.emit_intrinsic_array_get(
                    node.callee,
                    self.node(argument_list.items[0])
                        .to_argument()
                        .expect("argument expected")
                        .expr,
                    self.loc(node.span),
                    dest,
                ),

                Intrinsic::ArrayNewOfSize => self.emit_intrinsic_new_array(node_id, node, dest),

                Intrinsic::ArrayWithValues => {
                    let ty = self.ty(node_id);

                    let (cls_id, type_params) = ty.to_class().expect("class expected");
                    assert_eq!(cls_id, self.sa.known.classes.array());
                    assert_eq!(1, type_params.len());
                    let element_ty = type_params[0].clone();
                    let node_wrapper = self.node2::<ast::AstCall>(node_id);
                    self.emit_array_with_variadic_arguments(node_wrapper, &[element_ty], 0, dest)
                }

                _ => panic!("unimplemented intrinsic {:?}", intrinsic),
            }
        }
    }

    pub(super) fn emit_intrinsic_new_array(
        &mut self,
        node_id: AstId,
        expr: &ast::Call,
        dest: DataDest,
    ) -> Register {
        // We need array of elements
        let element_ty = self.ty(node_id);
        let (cls_id, type_params) = element_ty.to_class().expect("class expected");
        let cls_idx = self.builder.add_const_cls_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&type_params),
        );
        let argument_list = self.node(expr.arg_list).as_argument_list();

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let arg0 = self
            .node(argument_list.items[0])
            .to_argument()
            .expect("argument expected");
        let length_reg = gen_expr_id(self, arg0.expr, DataDest::Alloc);

        self.builder
            .emit_new_array(array_reg, length_reg, cls_idx, self.loc(expr.span));

        self.free_if_temp(length_reg);

        array_reg
    }

    pub(super) fn emit_bin_is(&mut self, expr: &ast::Bin, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = gen_expr_id(self, expr.lhs, DataDest::Alloc);
        let rhs_reg = gen_expr_id(self, expr.rhs, DataDest::Alloc);

        self.builder.emit_test_identity(dest, lhs_reg, rhs_reg);

        if expr.op == ast::BinOp::Cmp(ast::CmpOp::IsNot) {
            self.builder.emit_not(dest, dest);
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    pub(super) fn emit_bin_or(&mut self, expr: &ast::Bin, dest: DataDest) -> Register {
        let end_lbl = self.builder.create_label();
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        gen_expr_id(self, expr.lhs, DataDest::Reg(dest));
        self.builder.emit_jump_if_true(dest, end_lbl);
        gen_expr_id(self, expr.rhs, DataDest::Reg(dest));
        self.builder.bind_label(end_lbl);

        dest
    }

    pub(super) fn emit_bin_and(&mut self, expr: &ast::Bin, dest: DataDest) -> Register {
        let end_lbl = self.builder.create_label();
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        self.push_scope();

        if let Some(is_expr) = self.node(expr.lhs).to_is() {
            self.builder.emit_const_false(dest);
            let value = gen_expr_id(self, is_expr.value, DataDest::Alloc);
            let ty = self.ty(is_expr.value);
            self.setup_pattern_vars(is_expr.pattern);
            self.destruct_pattern(is_expr.pattern, value, ty, Some(end_lbl));
            self.free_if_temp(value);
        } else {
            gen_expr_id(self, expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_false(dest, end_lbl);
        }

        gen_expr_id(self, expr.rhs, DataDest::Reg(dest));
        self.builder.bind_label(end_lbl);

        self.pop_scope();

        dest
    }

    pub(super) fn emit_intrinsic_array_get(
        &mut self,
        obj: AstId,
        idx: AstId,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let ty = self.ty(obj);
        let ty: BytecodeType = if ty.cls_id() == Some(self.sa.known.classes.string()) {
            BytecodeType::UInt8
        } else {
            let ty = ty.type_params();
            let ty = ty[0].clone();

            self.emitter.convert_ty_reg(ty)
        };

        let dest = self.ensure_register(dest, ty.clone());

        let arr = gen_expr_id(self, obj, DataDest::Alloc);
        let idx = gen_expr_id(self, idx, DataDest::Alloc);

        self.builder.emit_load_array(dest, arr, idx, location);

        self.free_if_temp(arr);
        self.free_if_temp(idx);

        dest
    }

    pub(super) fn emit_intrinsic_array_set(
        &mut self,
        arr: AstId,
        idx: AstId,
        src: AstId,
        location: Location,
        _dest: DataDest,
    ) -> Register {
        let arr = gen_expr_id(self, arr, DataDest::Alloc);
        let idx = gen_expr_id(self, idx, DataDest::Alloc);
        let src = gen_expr_id(self, src, DataDest::Alloc);

        self.builder.emit_store_array(src, arr, idx, location);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        self.ensure_unit_register()
    }

    pub(super) fn emit_intrinsic_un(
        &mut self,
        opnd: AstId,
        info: IntrinsicInfo,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        let fct = self.sa.fct(info.fct_id.expect("missing method"));
        let ty = self.emitter.convert_ty(fct.return_type());
        let dest = self.ensure_register(dest, ty);

        let src = gen_expr_id(self, opnd, DataDest::Alloc);

        match intrinsic {
            Intrinsic::ArrayLen | Intrinsic::StrLen => {
                self.builder.emit_array_length(dest, src, location);
            }
            Intrinsic::Int32Neg
            | Intrinsic::Int64Neg
            | Intrinsic::Float32Neg
            | Intrinsic::Float64Neg => self.builder.emit_neg(dest, src, location),
            Intrinsic::BoolNot | Intrinsic::Int32Not | Intrinsic::Int64Not => {
                self.builder.emit_not(dest, src)
            }
            Intrinsic::Float32IsNan => self.builder.emit_test_ne(dest, src, src),
            Intrinsic::Float64IsNan => self.builder.emit_test_ne(dest, src, src),
            _ => {
                panic!("unimplemented intrinsic {:?}", intrinsic);
            }
        }

        self.free_if_temp(src);

        dest
    }

    pub(super) fn emit_intrinsic_bin(
        &mut self,
        lhs: AstId,
        rhs: AstId,
        info: IntrinsicInfo,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        match intrinsic {
            Intrinsic::ArrayGet | Intrinsic::StrGet => {
                return self.emit_intrinsic_array_get(lhs, rhs, location, dest);
            }

            _ => {}
        }

        let fct_id = info.fct_id.expect("missing function");
        let fct = self.sa.fct(fct_id);

        let result_type = self.emitter.convert_ty(fct.return_type());

        let dest = self.ensure_register(dest, result_type);

        let lhs_reg = gen_expr_id(self, lhs, DataDest::Alloc);
        let rhs_reg = gen_expr_id(self, rhs, DataDest::Alloc);

        gen_intrinsic_bin(self, intrinsic, dest, lhs_reg, rhs_reg, location);

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    pub(super) fn visit_expr_assign(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        _dest: DataDest,
    ) -> Register {
        if self.node(expr.lhs).is_ident() {
            let value_reg = gen_expr_id(self, expr.rhs, DataDest::Alloc);
            let ident_type = self.analysis.map_idents.get(expr.lhs).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => {
                    self.visit_expr_assign_var(expr_ast_id, expr, var_id, value_reg);
                }
                &IdentType::Context(level, field_id) => {
                    self.visit_expr_assign_context(expr_ast_id, expr, level, field_id, value_reg);
                }
                &IdentType::Global(gid) => {
                    self.visit_expr_assign_global(expr_ast_id, expr, gid, value_reg);
                }
                _ => unreachable!(),
            }
            self.free_if_temp(value_reg);
        } else if self.node(expr.lhs).is_path() {
            let value_reg = gen_expr_id(self, expr.rhs, DataDest::Alloc);
            let ident_type = self.analysis.map_idents.get(expr.lhs).unwrap();
            match ident_type {
                &IdentType::Global(gid) => {
                    self.visit_expr_assign_global(expr_ast_id, expr, gid, value_reg);
                }
                _ => unreachable!(),
            }
            self.free_if_temp(value_reg);
        } else {
            match *self.node(expr.lhs) {
                Ast::DotExpr(ref dot) => {
                    self.visit_expr_assign_dot(expr_ast_id, expr, expr.lhs, dot)
                }
                Ast::Call(ref call) => self.visit_expr_assign_call(expr_ast_id, expr, call),
                _ => unreachable!(),
            };
        }

        self.ensure_unit_register()
    }

    pub(super) fn visit_expr_assign_call(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        call_expr: &ast::Call,
    ) {
        let object = call_expr.callee;
        let argument_list = self.node(call_expr.arg_list).as_argument_list();

        let arg0 = self
            .node(argument_list.items[0])
            .to_argument()
            .expect("argument expected");
        let index = arg0.expr;
        let value = expr.rhs;

        let obj_reg = gen_expr_id(self, object, DataDest::Alloc);
        let idx_reg = gen_expr_id(self, index, DataDest::Alloc);
        let val_reg = gen_expr_id(self, value, DataDest::Alloc);

        let array_assignment = self
            .analysis
            .map_array_assignments
            .get(expr_ast_id)
            .expect("missing assignment data")
            .clone();

        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let ty = self
                .emitter
                .convert_ty_reg(array_assignment.item_ty.expect("missing item type"));
            let current = self.alloc_temp(ty);

            let call_type = array_assignment.index_get.expect("missing index_get");
            let fct_id = call_type.fct_id().unwrap();
            let fct = self.sa.fct(fct_id);

            if let Some(intrinsic) = fct.intrinsic.get() {
                assert_eq!(*intrinsic, Intrinsic::ArrayGet);
                self.builder
                    .emit_load_array(current, obj_reg, idx_reg, location);
            } else {
                let obj_ty = self.ty(object);

                self.builder.emit_push_register(obj_reg);
                self.builder.emit_push_register(idx_reg);

                let type_params = obj_ty.type_params();

                let callee_idx = self.builder.add_const_fct_types(
                    self.emitter.convert_function_id(fct_id),
                    self.convert_tya(&type_params),
                );
                self.builder
                    .emit_invoke_direct(current, callee_idx, location);
            }

            if let Some(info) = self.get_intrinsic(expr_ast_id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, val_reg, location);
            } else {
                gen_method_bin(self, expr_ast_id, current, current, val_reg, location);
            }

            current
        } else {
            val_reg
        };

        let call_type = array_assignment.index_set.expect("missing index_set");
        let fct_id = call_type.fct_id().unwrap();
        let fct = self.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArraySet);
            self.builder
                .emit_store_array(assign_value, obj_reg, idx_reg, location);
        } else {
            let obj_ty = self.ty(object);

            self.builder.emit_push_register(obj_reg);
            self.builder.emit_push_register(idx_reg);
            self.builder.emit_push_register(assign_value);

            let type_params = obj_ty.type_params();

            let callee_idx = self.builder.add_const_fct_types(
                self.emitter.convert_function_id(fct_id),
                self.convert_tya(&type_params),
            );
            let dest = self.ensure_unit_register();
            self.builder.emit_invoke_direct(dest, callee_idx, location);
        }

        self.free_if_temp(obj_reg);
        self.free_if_temp(idx_reg);
        self.free_if_temp(val_reg);
        self.free_if_temp(assign_value);
    }

    pub(super) fn visit_expr_assign_dot(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        dot_ast_id: AstId,
        dot: &ast::DotExpr,
    ) {
        let (cls_ty, field_index) = {
            let ident_type = self.analysis.map_idents.get(dot_ast_id).cloned().unwrap();
            match ident_type {
                IdentType::Field(class, field) => (class, field),
                _ => unreachable!(),
            }
        };

        let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

        let field_idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&type_params),
            field_index.0 as u32,
        );

        let obj = gen_expr_id(self, dot.lhs, DataDest::Alloc);
        let value = gen_expr_id(self, expr.rhs, DataDest::Alloc);

        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let cls = self.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let ty = self.sa.field(field_id).ty();
            let ty = self.emitter.convert_ty_reg(ty);
            let current = self.alloc_temp(ty);
            self.builder
                .emit_load_field(current, obj, field_idx, location);

            if let Some(info) = self.get_intrinsic(expr_ast_id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr_ast_id, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.builder
            .emit_store_field(assign_value, obj, field_idx, location);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }

        self.free_if_temp(obj);
        self.free_if_temp(value);
    }

    pub(super) fn visit_expr_assign_context(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        outer_context_id: OuterContextIdx,
        context_field_id: ContextFieldId,
        value: Register,
    ) {
        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let current =
                self.load_from_outer_context(outer_context_id, context_field_id, location);

            if let Some(info) = self.get_intrinsic(expr_ast_id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr_ast_id, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.store_in_outer_context(outer_context_id, context_field_id, assign_value, location);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }
    }

    pub(super) fn visit_expr_assign_var(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        var_id: VarId,
        value: Register,
    ) {
        let var = self.analysis.vars.get_var(var_id);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let current = match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    let ty = self.emitter.convert_ty_reg(var.ty.clone());
                    let dest_reg = self.alloc_temp(ty);
                    self.load_from_context(dest_reg, scope_id, field_id, self.loc(expr.span));
                    dest_reg
                }

                VarLocation::Stack => self.var_reg(var_id),
            };

            let location = self.loc(expr.span);

            if let Some(info) = self.get_intrinsic(expr_ast_id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr_ast_id, current, current, value, location);
            }

            current
        } else {
            value
        };

        match var.location {
            VarLocation::Context(scope_id, field_id) => {
                self.store_in_context(assign_value, scope_id, field_id, self.loc(expr.span));
            }

            VarLocation::Stack => {
                let var_reg = self.var_reg(var_id);
                self.builder.emit_mov(var_reg, assign_value);
            }
        }

        if expr.op != ast::BinOp::Assign {
            self.free_if_temp(assign_value);
        }
    }

    pub(super) fn visit_expr_assign_global(
        &mut self,
        expr_ast_id: AstId,
        expr: &ast::Bin,
        gid: GlobalDefinitionId,
        value: Register,
    ) {
        let bc_gid = self.emitter.convert_global_id(gid);
        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let global = self.sa.global(gid);
            let ty = self.emitter.convert_ty_reg(global.ty());
            let current = self.alloc_temp(ty);
            self.builder.emit_load_global(current, bc_gid, location);

            if let Some(info) = self.get_intrinsic(expr_ast_id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr_ast_id, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.builder.emit_store_global(assign_value, bc_gid);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }
    }

    pub(super) fn visit_expr_ident(&mut self, ast_id: AstId, dest: DataDest) -> Register {
        let ident = self.node2::<ast::AstIdent>(ast_id);
        let ident_type = self.analysis.map_idents.get(ast_id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => {
                self.visit_expr_ident_var(var_id, dest, self.loc(ident.span()))
            }
            &IdentType::Context(level, field_id) => {
                self.visit_expr_ident_context(level, field_id, dest, self.loc(ident.span()))
            }
            &IdentType::Global(gid) => {
                self.visit_expr_ident_global(gid, dest, self.loc(ident.span()))
            }
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            &IdentType::EnumVariant(enum_id, ref type_params, variant_idx) => self.emit_new_enum(
                enum_id,
                type_params.clone(),
                variant_idx,
                self.loc(ident.span()),
                dest,
            ),

            &IdentType::Field(..) => unreachable!(),
            &IdentType::Struct(..) => unreachable!(),
            &IdentType::StructField(..) => unreachable!(),

            &IdentType::Fct(..) => unreachable!(),
            &IdentType::Class(..) => unreachable!(),
        }
    }

    pub(super) fn visit_expr_ident_context(
        &mut self,
        context_id: OuterContextIdx,
        field_id: ContextFieldId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        assert!(self.is_lambda);
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object (in self register).
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(lambda_cls_id),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(context_id.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self
            .analysis
            .outer_contexts
            .iter()
            .skip(context_id.0 + 1)
            .rev()
        {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();
                let idx = self.builder.add_const_field_types(
                    self.emitter.convert_class_id(outer_cls_id),
                    self.convert_tya(&self.identity_type_params()),
                    0,
                );
                assert!(outer_context_class.has_parent_slot());
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        let outer_context_info = self.analysis.outer_contexts[context_id.0].clone();
        let outer_cls_id = outer_context_info.class_id();

        let outer_cls = self.sa.class(outer_cls_id);
        let field_index = field_id_from_context_idx(field_id, outer_context_info.has_parent_slot());
        let field_id = outer_cls.field_id(field_index);
        let field = self.sa.field(field_id);

        let ty: BytecodeType = self.emitter.convert_ty_reg(field.ty());
        let value_reg = self.ensure_register(dest, ty);

        let idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(outer_cls_id),
            self.convert_tya(&self.identity_type_params()),
            field_index.0 as u32,
        );
        self.builder
            .emit_load_field(value_reg, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);

        value_reg
    }

    pub(super) fn visit_expr_ident_const(
        &mut self,
        const_id: ConstDefinitionId,
        dest: DataDest,
    ) -> Register {
        let const_ = self.sa.const_(const_id);
        let ty = const_.ty();

        let bytecode_ty = self.emitter.convert_ty_reg(ty.clone());
        let dest = self.ensure_register(dest, bytecode_ty);

        let const_id = self.emitter.convert_const_id(const_id);
        self.builder.emit_load_const(dest, const_id);

        dest
    }

    pub(super) fn visit_expr_ident_global(
        &mut self,
        gid: GlobalDefinitionId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let global_var = self.sa.global(gid);

        let ty: BytecodeType = self.emitter.convert_ty_reg(global_var.ty());
        let dest = self.ensure_register(dest, ty);

        self.builder
            .emit_load_global(dest, self.emitter.convert_global_id(gid), location);

        dest
    }

    pub(super) fn visit_expr_ident_var(
        &mut self,
        var_id: VarId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(scope_id, field_idx) => {
                let ty = self.emitter.convert_ty_reg(var.ty.clone());
                let dest_reg = self.ensure_register(dest, ty);
                self.load_from_context(dest_reg, scope_id, field_idx, location);
                dest_reg
            }

            VarLocation::Stack => {
                let var_reg = self.var_reg(var_id);

                if dest.is_alloc() {
                    return var_reg;
                }

                let dest = dest.reg();
                self.emit_mov(dest, var_reg);

                dest
            }
        }
    }

    pub(super) fn store_in_outer_context(
        &mut self,
        level: OuterContextIdx,
        context_idx: ContextFieldId,
        value: Register,
        location: Location,
    ) {
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object in self.
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(lambda_cls_id),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(level.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self.analysis.outer_contexts.iter().skip(level.0 + 1).rev() {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();

                let idx = self.builder.add_const_field_types(
                    self.emitter.convert_class_id(outer_cls_id),
                    self.convert_tya(&self.identity_type_params()),
                    0,
                );
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        // Store value in context field
        let outer_context_info = self.analysis.outer_contexts[level.0].clone();

        let field_id = field_id_from_context_idx(context_idx, outer_context_info.has_parent_slot());
        let idx = self.builder.add_const_field_types(
            ClassId(
                outer_context_info
                    .class_id()
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            self.convert_tya(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(value, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);
    }

    pub(super) fn load_from_outer_context(
        &mut self,
        context_id: OuterContextIdx,
        field_id: ContextFieldId,
        location: Location,
    ) -> Register {
        assert!(self.is_lambda);
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object (in self register).
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(lambda_cls_id),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(context_id.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self
            .analysis
            .outer_contexts
            .iter()
            .skip(context_id.0 + 1)
            .rev()
        {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();
                let idx = self.builder.add_const_field_types(
                    self.emitter.convert_class_id(outer_cls_id),
                    self.convert_tya(&self.identity_type_params()),
                    0,
                );
                assert!(outer_context_class.has_parent_slot());
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        let outer_context_info = self.analysis.outer_contexts[context_id.0].clone();
        let outer_cls_id = outer_context_info.class_id();

        let outer_cls = self.sa.class(outer_cls_id);
        let field_index = field_id_from_context_idx(field_id, outer_context_info.has_parent_slot());
        let field_id = outer_cls.field_id(field_index);
        let field = self.sa.field(field_id);

        let ty: BytecodeType = self.emitter.convert_ty_reg(field.ty());
        let dest = self.alloc_temp(ty);

        let idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(outer_cls_id),
            self.convert_tya(&self.identity_type_params()),
            field_index.0 as u32,
        );
        self.builder
            .emit_load_field(dest, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);

        dest
    }

    pub(super) fn store_in_context(
        &mut self,
        src: Register,
        scope_id: ScopeId,
        field_id: ContextFieldId,
        location: Location,
    ) {
        let entered_context = &self.entered_contexts[scope_id.0];
        let context_register = entered_context.register.expect("missing register");
        let context_data = entered_context.context_data.clone();
        let cls_id = context_data.class_id();
        let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
        let field_idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(src, context_register, field_idx, location);
    }

    pub(super) fn load_from_context(
        &mut self,
        dest: Register,
        scope_id: ScopeId,
        field_id: ContextFieldId,
        location: Location,
    ) {
        // Load context object.
        let entered_context = &self.entered_contexts[scope_id.0];
        let context_register = entered_context.register.expect("missing register");
        let context_data = entered_context.context_data.clone();
        let cls_id = context_data.class_id();
        let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
        let field_idx = self.builder.add_const_field_types(
            self.emitter.convert_class_id(cls_id),
            self.convert_tya(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_load_field(dest, context_register, field_idx, location);
    }

    pub(super) fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
    }

    pub(super) fn set_var_reg(&mut self, var_id: VarId, reg: Register) {
        let old = self.var_registers.insert(var_id, reg);
        assert!(old.is_none());
    }

    pub(super) fn ensure_register(&mut self, dest: DataDest, ty: BytecodeType) -> Register {
        match dest {
            DataDest::Alloc => self.alloc_temp(ty),
            DataDest::Reg(reg) => reg,
        }
    }

    pub(super) fn add_const_pool_entry_for_call(
        &mut self,
        fct: &FctDefinition,
        call_type: &CallType,
    ) -> ConstPoolIdx {
        match call_type {
            CallType::GenericStaticMethod(id, .., trait_type_params, fct_type_params)
            | CallType::GenericMethod(id, .., trait_type_params, fct_type_params) => {
                self.builder.add_const(ConstPoolEntry::Generic(
                    id.index() as u32,
                    self.emitter.convert_function_id(fct.id()),
                    self.convert_tya(&trait_type_params),
                    self.convert_tya(&fct_type_params),
                ))
            }
            CallType::GenericMethodSelf(_, fct_id, trait_type_params, fct_type_params)
            | CallType::GenericStaticMethodSelf(_, fct_id, trait_type_params, fct_type_params) => {
                self.builder.add_const(ConstPoolEntry::GenericSelf(
                    self.emitter.convert_function_id(*fct_id),
                    self.convert_tya(&trait_type_params),
                    self.convert_tya(&fct_type_params),
                ))
            }
            CallType::GenericMethodNew {
                object_type,
                trait_ty,
                fct_id,
                fct_type_params,
            } => self.builder.add_const(ConstPoolEntry::GenericNew {
                object_type: self.emitter.convert_ty(object_type.clone()),
                trait_ty: self.emitter.convert_trait_ty(&trait_ty),
                fct_id: self.emitter.convert_function_id(*fct_id),
                fct_type_params: self.convert_tya(fct_type_params),
            }),
            CallType::TraitObjectMethod(trait_object_ty, _) => {
                self.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                    self.emitter.convert_ty(trait_object_ty.clone()),
                    self.emitter.convert_function_id(fct.id()),
                ))
            }

            CallType::Method(.., type_params)
            | CallType::Expr(.., type_params)
            | CallType::Fct(.., type_params) => {
                assert_eq!(
                    fct.type_param_definition().type_param_count(),
                    type_params.len()
                );
                self.builder.add_const_fct_types(
                    self.emitter.convert_function_id(fct.id()),
                    self.convert_tya(&type_params),
                )
            }

            _ => panic!("unexpected call type {:?}", call_type),
        }
    }

    pub(super) fn specialize_type_for_call(
        &self,
        call_type: &CallType,
        ty: SourceType,
    ) -> SourceType {
        match call_type {
            CallType::Fct(_, type_params)
            | CallType::Expr(_, _, type_params)
            | CallType::Method(_, _, type_params) => specialize_type(self.sa, ty, type_params),

            CallType::TraitObjectMethod(trait_ty, _actual_object_ty) => {
                let (trait_id, type_params, assoc_types) = match trait_ty {
                    SourceType::TraitObject(trait_id, type_params, assoc_types) => {
                        (*trait_id, type_params, assoc_types)
                    }
                    _ => unreachable!(),
                };
                specialize_ty_for_trait_object(self.sa, ty, trait_id, type_params, assoc_types)
            }
            CallType::GenericMethod(
                id,
                _trait_id,
                _method_id,
                trait_type_params,
                fct_type_params,
            )
            | CallType::GenericStaticMethod(
                id,
                _trait_id,
                _method_id,
                trait_type_params,
                fct_type_params,
            ) => replace_type(
                self.sa,
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
                self.sa,
                ty,
                Some(&trait_type_params.connect(fct_type_params)),
                None,
            ),

            CallType::GenericMethodNew {
                trait_ty,
                fct_type_params,
                ..
            } => replace_type(
                self.sa,
                ty,
                Some(&trait_ty.type_params.connect(fct_type_params)),
                None,
            ),

            CallType::Lambda(..)
            | CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Intrinsic(..) => {
                unreachable!()
            }
        }
    }
}
