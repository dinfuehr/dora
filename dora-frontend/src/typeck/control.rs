use dora_parser::ast;

use crate::error::msg::ErrorMessage;
use crate::expr_always_returns;
use crate::sema::{FctDefinitionId, ForTypeInfo};
use crate::ty::SourceType;
use crate::typeck::{check_expr, check_let_pattern, MethodLookup, TypeCheck};

pub(super) fn check_expr_while(
    ck: &mut TypeCheck,
    stmt: &ast::ExprWhileType,
    _expected_ty: SourceType,
) -> SourceType {
    let expr_type = check_expr(ck, &stmt.cond, SourceType::Bool);

    if !expr_type.is_error() && !expr_type.is_bool() {
        let expr_type = ck.ty_name(&expr_type);
        let msg = ErrorMessage::WhileCondType(expr_type);
        ck.sa.report(ck.file_id, stmt.span, msg);
    }

    check_loop_body(ck, &stmt.block);
    SourceType::Unit
}

fn check_loop_body(ck: &mut TypeCheck, expr: &ast::ExprData) {
    let old_in_loop = ck.in_loop;
    ck.in_loop = true;
    check_expr(ck, expr, SourceType::Any);
    ck.in_loop = old_in_loop;
}

pub(super) fn check_expr_for(
    ck: &mut TypeCheck,
    stmt: &ast::ExprForType,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, &stmt.expr, SourceType::Any);

    if object_type.is_error() {
        ck.symtable.push_level();
        check_let_pattern(ck, &stmt.pattern, SourceType::Error);
        check_expr(ck, &stmt.block, SourceType::Any);
        ck.symtable.pop_level();
        return SourceType::Unit;
    }

    if let Some((for_type_info, ret_type)) =
        type_supports_iterator_protocol(ck, object_type.clone())
    {
        ck.symtable.push_level();
        // set variable type to return type of next
        check_let_pattern(ck, &stmt.pattern, ret_type);
        // store fct ids for code generation
        ck.analysis.map_fors.insert(stmt.id, for_type_info);
        check_loop_body(ck, &stmt.block);
        ck.symtable.pop_level();
        return SourceType::Unit;
    }

    if let Some((make_iterator, iterator_type)) =
        type_supports_make_iterator(ck, object_type.clone())
    {
        if let Some((mut for_type_info, ret_type)) =
            type_supports_iterator_protocol(ck, iterator_type.clone())
        {
            ck.symtable.push_level();

            // set variable type to return type of next
            check_let_pattern(ck, &stmt.pattern, ret_type);

            // store fct ids for code generation
            for_type_info.make_iterator = Some(make_iterator);
            ck.analysis.map_fors.insert(stmt.id, for_type_info);

            check_loop_body(ck, &stmt.block);
            ck.symtable.pop_level();
            return SourceType::Unit;
        }
    }

    let name = ck.ty_name(&object_type);
    let msg = ErrorMessage::TypeNotUsableInForIn(name);
    ck.sa.report(ck.file_id, stmt.expr.span(), msg);

    // set invalid error type
    ck.symtable.push_level();
    check_let_pattern(ck, &stmt.pattern, SourceType::Error);
    check_loop_body(ck, &stmt.block);
    ck.symtable.pop_level();
    SourceType::Unit
}

fn type_supports_make_iterator(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<(FctDefinitionId, SourceType)> {
    let make_iterator_name = ck.sa.interner.intern("makeIterator");

    let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_defs)
        .no_error_reporting()
        .method(object_type)
        .name(make_iterator_name)
        .args(&[])
        .find();

    if lookup.find() {
        let make_iterator_id = lookup.found_fct_id().unwrap();
        let make_iterator_ret = lookup.found_ret().unwrap();

        Some((make_iterator_id, make_iterator_ret))
    } else {
        None
    }
}

fn type_supports_iterator_protocol(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<(ForTypeInfo, SourceType)> {
    let next_name = ck.sa.interner.intern("next");

    let lookup_next = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_defs)
        .no_error_reporting()
        .method(object_type.clone())
        .name(next_name)
        .args(&[])
        .find();

    if !lookup_next.find() {
        return None;
    }

    let next_result_type = lookup_next.found_ret().unwrap();

    let value_type = if let SourceType::Enum(enum_id, type_params) = next_result_type.clone() {
        if enum_id == ck.sa.known.enums.option() {
            assert_eq!(type_params.len(), 1);
            type_params[0].clone()
        } else {
            return None;
        }
    } else {
        return None;
    };

    Some((
        ForTypeInfo {
            make_iterator: None,
            next: lookup_next.found_fct_id().expect("fct_id missing"),
            iterator_type: object_type,
            next_type: next_result_type,
            value_type: value_type.clone(),
        },
        value_type,
    ))
}

pub(super) fn check_expr_return(
    ck: &mut TypeCheck,
    expr: &ast::ExprReturnType,
    _expected_ty: SourceType,
) -> SourceType {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = expr
            .expr
            .as_ref()
            .map(|expr| check_expr(ck, &expr, expected_ty.clone()))
            .unwrap_or(SourceType::Unit);

        ck.check_fct_return_type(expected_ty, expr.span, expr_type);
    } else {
        ck.sa
            .report(ck.file_id, expr.span, ErrorMessage::InvalidReturn);

        if let Some(ref expr) = expr.expr {
            check_expr(ck, expr.as_ref(), SourceType::Any);
        }
    }

    SourceType::Unit
}

pub(super) fn check_expr_if(
    ck: &mut TypeCheck,
    expr: &ast::ExprIfType,
    expected_ty: SourceType,
) -> SourceType {
    let expr_type = check_expr(ck, &expr.cond, SourceType::Any);

    if !expr_type.is_bool() && !expr_type.is_error() {
        let expr_type = ck.ty_name(&expr_type);
        let msg = ErrorMessage::IfCondType(expr_type);
        ck.sa.report(ck.file_id, expr.span, msg);
    }

    let then_type = check_expr(ck, &expr.then_block, expected_ty.clone());

    let merged_type = if let Some(ref else_block) = expr.else_block {
        let else_type = check_expr(ck, else_block, expected_ty);

        if expr_always_returns(&expr.then_block) {
            else_type
        } else if expr_always_returns(else_block) {
            then_type
        } else if then_type.is_error() {
            else_type
        } else if else_type.is_error() {
            then_type
        } else if !then_type.allows(ck.sa, else_type.clone()) {
            let then_type_name = ck.ty_name(&then_type);
            let else_type_name = ck.ty_name(&else_type);
            let msg = ErrorMessage::IfBranchTypesIncompatible(then_type_name, else_type_name);
            ck.sa.report(ck.file_id, expr.span, msg);
            then_type
        } else {
            then_type
        }
    } else {
        SourceType::Unit
    };

    ck.analysis.set_ty(expr.id, merged_type.clone());

    merged_type
}

pub(super) fn check_expr_break_and_continue(
    ck: &mut TypeCheck,
    expr: &ast::ExprData,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.in_loop {
        ck.sa
            .report(ck.file_id, expr.span(), ErrorMessage::OutsideLoop);
    }

    SourceType::Unit
}
