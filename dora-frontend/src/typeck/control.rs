use dora_parser::ast::{self, AstId};

use crate::error::msg::ErrorMessage;
use crate::expr_always_returns;
use crate::sema::{FctDefinitionId, ForTypeInfo, find_impl};
use crate::ty::{self, TraitType};
use crate::typeck::{TypeCheck, check_expr, check_pattern};
use crate::{SourceType, SourceTypeArray, specialize_type};

pub(super) fn check_expr_while(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::ExprWhileType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.enter_block_scope();

    let cond_ty = check_expr_condition(ck, node.cond);

    if !cond_ty.is_error() && !cond_ty.is_bool() {
        let cond_ty = ck.ty_name(&cond_ty);
        let msg = ErrorMessage::WhileCondType(cond_ty);
        ck.sa.report(ck.file_id, node.span, msg);
    }

    check_loop_body(ck, node.block);
    ck.leave_block_scope(node_id);
    SourceType::Unit
}

fn check_loop_body(ck: &mut TypeCheck, expr_id: AstId) {
    let old_in_loop = ck.in_loop;
    ck.in_loop = true;
    check_expr(ck, expr_id, SourceType::Any);
    ck.in_loop = old_in_loop;
}

pub(super) fn check_expr_for(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::ExprForType,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, node.expr, SourceType::Any);

    if object_type.is_error() {
        check_for_body(ck, node_id, node, ty::error());
        return SourceType::Unit;
    }

    if let Some((for_type_info, ret_type)) = type_supports_iterator_trait(ck, object_type.clone()) {
        // store fct ids for code generation
        ck.analysis.map_fors.insert(node_id, for_type_info);
        check_for_body(ck, node_id, node, ret_type);
        return SourceType::Unit;
    }

    if let Some(into_iterator_data) = type_supports_into_iterator_trait(ck, object_type.clone()) {
        let ret_type = if let Some((mut for_type_info, ret_type)) =
            type_supports_iterator_trait(ck, into_iterator_data.iterator_type.clone())
        {
            if let Some(iter_impl_fct_id) = into_iterator_data.iter_impl_fct_id {
                // store fct ids for code generation
                for_type_info.iter = Some((iter_impl_fct_id, into_iterator_data.bindings));
                ck.analysis.map_fors.insert(node_id, for_type_info);
            }

            ret_type
        } else {
            SourceType::Error
        };

        check_for_body(ck, node_id, node, ret_type);
        return SourceType::Unit;
    }

    let name = ck.ty_name(&object_type);
    let msg = ErrorMessage::TypeNotUsableInForIn(name);
    ck.sa.report(ck.file_id, ck.span(node.expr), msg);

    // set invalid error type
    check_for_body(ck, node_id, node, ty::error());
    SourceType::Unit
}

fn check_for_body(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::ExprForType,
    ty: SourceType,
) {
    ck.symtable.push_level();
    ck.enter_block_scope();
    check_pattern(ck, node.pattern, ty);
    check_loop_body(ck, node.block);
    ck.leave_block_scope(node_id);
    ck.symtable.pop_level();
}

struct IntoIteratorData {
    iter_impl_fct_id: Option<FctDefinitionId>,
    bindings: SourceTypeArray,
    iterator_type: SourceType,
}

fn type_supports_into_iterator_trait(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<IntoIteratorData> {
    let into_iterator_trait_id = ck.sa.known.traits.into_iterator();
    let into_iterator_trait = ck.sa.trait_(into_iterator_trait_id);

    let iter_name = ck.sa.interner.intern("iter");
    let iterator_type_name = ck.sa.interner.intern("IteratorType");

    let iter_trait_fct_id = into_iterator_trait
        .get_method(iter_name, false)
        .expect("missing next() in trait");

    let iterator_type_trait_alias_id = into_iterator_trait
        .alias_names()
        .get(&iterator_type_name)
        .cloned()
        .expect("missing Item alias");

    let trait_ty = TraitType::from_trait_id(into_iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        object_type.clone(),
        &ck.type_param_definition,
        trait_ty,
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);

        let iter_impl_fct_id = impl_.trait_method_map().get(&iter_trait_fct_id).cloned();

        let iterator_type = if let Some(iterator_type_impl_alias_id) = impl_
            .trait_alias_map()
            .get(&iterator_type_trait_alias_id)
            .cloned()
        {
            let iterator_type_impl_alias = ck.sa.alias(iterator_type_impl_alias_id);

            specialize_type(ck.sa, iterator_type_impl_alias.ty(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        Some(IntoIteratorData {
            iter_impl_fct_id,
            bindings: impl_match.bindings,
            iterator_type,
        })
    } else {
        None
    }
}

fn type_supports_iterator_trait(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<(ForTypeInfo, SourceType)> {
    let iterator_trait_id = ck.sa.known.traits.iterator();
    let iterator_trait = ck.sa.trait_(iterator_trait_id);

    let next_name = ck.sa.interner.intern("next");
    let item_name = ck.sa.interner.intern("Item");

    let next_trait_fct_id = iterator_trait
        .get_method(next_name, false)
        .expect("missing next() in trait");

    let item_trait_alias_id = iterator_trait
        .alias_names()
        .get(&item_name)
        .cloned()
        .expect("missing Item alias");

    let trait_ty = TraitType::from_trait_id(iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        object_type.clone(),
        &ck.type_param_definition,
        trait_ty,
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);
        let next_impl_fct_id = impl_.trait_method_map().get(&next_trait_fct_id).cloned();

        let next_type = if let Some(next_impl_fct_id) = next_impl_fct_id {
            let next_impl_fct = ck.sa.fct(next_impl_fct_id);
            specialize_type(ck.sa, next_impl_fct.return_type(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        let value_type = if let Some(item_impl_alias_id) =
            impl_.trait_alias_map().get(&item_trait_alias_id).cloned()
        {
            let impl_alias = ck.sa.alias(item_impl_alias_id);
            specialize_type(ck.sa, impl_alias.ty(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        Some((
            ForTypeInfo {
                iter: None,
                next: next_impl_fct_id,
                iterator_type: object_type,
                next_type,
                value_type: value_type.clone(),
            },
            value_type,
        ))
    } else {
        None
    }
}

pub(super) fn check_expr_return(
    ck: &mut TypeCheck,
    _id: ast::AstId,
    expr: &ast::ExprReturnType,
    _expected_ty: SourceType,
) -> SourceType {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = expr
            .expr
            .map(|expr_id| check_expr(ck, expr_id, expected_ty.clone()))
            .unwrap_or(SourceType::Unit);

        ck.check_fct_return_type(expected_ty, expr.span, expr_type);
    } else {
        ck.sa
            .report(ck.file_id, expr.span, ErrorMessage::InvalidReturn);

        if let Some(expr_id) = expr.expr {
            check_expr(ck, expr_id, SourceType::Any);
        }
    }

    SourceType::Unit
}

pub(super) fn check_expr_if(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::ExprIfType,
    expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    let ty = check_expr_condition(ck, node.cond);

    if !ty.is_bool() && !ty.is_error() {
        let expr_type = ck.ty_name(&ty);
        let msg = ErrorMessage::IfCondType(expr_type);
        ck.sa.report(ck.file_id, ck.span(node.cond), msg);
    }

    let then_type = check_expr(ck, node.then_block, expected_ty.clone());

    ck.symtable.pop_level();

    let merged_type = if let Some(else_block) = node.else_block {
        let else_type = check_expr(ck, else_block, expected_ty);

        let ast_file = ck.sa.file(ck.file_id).ast().as_ref();

        if expr_always_returns(ast_file, node.then_block) {
            else_type
        } else if expr_always_returns(ast_file, else_block) {
            then_type
        } else if then_type.is_error() {
            else_type
        } else if else_type.is_error() {
            then_type
        } else if !then_type.allows(ck.sa, else_type.clone()) {
            let then_type_name = ck.ty_name(&then_type);
            let else_type_name = ck.ty_name(&else_type);
            let msg = ErrorMessage::IfBranchTypesIncompatible(then_type_name, else_type_name);
            ck.sa.report(ck.file_id, node.span, msg);
            then_type
        } else {
            then_type
        }
    } else {
        SourceType::Unit
    };

    ck.analysis.set_ty(node_id, merged_type.clone());

    merged_type
}

pub fn check_expr_condition(ck: &mut TypeCheck, cond_id: AstId) -> SourceType {
    let cond = ck.node(cond_id);

    if let Some(bin_expr) = cond.to_bin_and() {
        if let Some(lhs_is_expr) = ck.node(bin_expr.lhs).to_is() {
            let ty = check_expr(ck, lhs_is_expr.value, SourceType::Any);
            check_pattern(ck, lhs_is_expr.pattern, ty);
        } else {
            let lhs_ty = check_expr(ck, bin_expr.lhs, SourceType::Bool);

            if !lhs_ty.is_bool() && !lhs_ty.is_error() {
                let lhs_ty = lhs_ty.name(ck.sa);
                let msg = ErrorMessage::WrongType("Bool".into(), lhs_ty);
                ck.sa.report(ck.file_id, ck.span(bin_expr.lhs), msg);
            }
        }

        let rhs_ty = check_expr_condition(ck, bin_expr.rhs);

        if !rhs_ty.is_bool() && !rhs_ty.is_error() {
            let rhs_ty = rhs_ty.name(ck.sa);
            let msg = ErrorMessage::WrongType("Bool".into(), rhs_ty);
            ck.sa.report(ck.file_id, ck.span(bin_expr.rhs), msg);
        }

        SourceType::Bool
    } else if let Some(is_expr) = cond.to_is() {
        let ty = check_expr(ck, is_expr.value, SourceType::Any);
        check_pattern(ck, is_expr.pattern, ty);
        SourceType::Bool
    } else {
        check_expr(ck, cond_id, SourceType::Bool)
    }
}

pub(super) fn check_expr_break_and_continue(
    ck: &mut TypeCheck,
    expr_id: AstId,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.in_loop {
        ck.sa
            .report(ck.file_id, ck.span(expr_id), ErrorMessage::OutsideLoop);
    }

    SourceType::Unit
}

pub(super) fn check_expr_match(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::ExprMatchType,
    expected_ty: SourceType,
) -> SourceType {
    let expr_type = check_expr(ck, node.expr, SourceType::Any);
    ck.analysis.set_ty(node.expr, expr_type.clone());
    let mut result_type = ty::error();

    for &arm_id in &node.arms {
        let arm = ck.node(arm_id).to_match_arm().expect("arm expected");
        ck.symtable.push_level();
        check_expr_match_arm(
            ck,
            arm,
            expr_type.clone(),
            expected_ty.clone(),
            &mut result_type,
        );
        ck.symtable.pop_level();
    }

    ck.analysis.set_ty(node_id, result_type.clone());

    result_type
}

fn check_expr_match_arm(
    ck: &mut TypeCheck,
    arm: &ast::MatchArmType,
    expr_ty: SourceType,
    expected_ty: SourceType,
    result_type: &mut SourceType,
) {
    check_pattern(ck, arm.pattern, expr_ty);

    if let Some(cond) = arm.cond {
        let cond_ty = check_expr(ck, cond, SourceType::Bool);

        if !cond_ty.is_bool() && !cond_ty.is_error() {
            let cond_ty = ck.ty_name(&cond_ty);
            let msg = ErrorMessage::IfCondType(cond_ty);
            ck.sa.report(ck.file_id, ck.span(cond), msg);
        }
    }

    let arm_ty = check_expr(ck, arm.value, expected_ty.clone());

    if result_type.is_error() {
        *result_type = arm_ty;
    } else if arm_ty.is_error() {
        // Ignore this arm.
    } else if !result_type.allows(ck.sa, arm_ty.clone()) {
        let result_type_name = ck.ty_name(&result_type);
        let arm_ty_name = ck.ty_name(&arm_ty);
        let msg = ErrorMessage::MatchBranchTypesIncompatible(result_type_name, arm_ty_name);
        ck.sa.report(ck.file_id, ck.span(arm.value), msg);
    }
}

pub(super) fn get_subpatterns<'a>(
    ck: &mut TypeCheck<'a>,
    pattern_id: ast::AstId,
) -> Option<&'a Vec<AstId>> {
    let pattern = ck.node(pattern_id);

    match pattern {
        ast::Ast::IdentPattern(..) => None,
        ast::Ast::ConstructorPattern(p) => p.params.as_ref(),
        _ => unreachable!(),
    }
}
