use std::collections::HashMap;

use dora_parser::ast;
use fixedbitset::FixedBitSet;

use crate::error::msg::ErrorMessage;
use crate::expr_always_returns;
use crate::interner::Name;
use crate::sema::{EnumDefinitionId, FctDefinitionId, ForTypeInfo, IdentType};
use crate::specialize::replace_type_param;
use crate::sym::SymbolKind;
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeck::{check_expr, check_let_pattern, read_path, MethodLookup, TypeCheck};

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

pub(super) fn check_expr_match(
    ck: &mut TypeCheck,
    node: &ast::ExprMatchType,
    expected_ty: SourceType,
) -> SourceType {
    let expr_type = check_expr(ck, &node.expr, SourceType::Any);
    let mut result_type = SourceType::Error;

    if !expr_type.is_enum() {
        ck.sa
            .report(ck.file_id, node.span, ErrorMessage::EnumExpected);
    }

    let expr_enum_id = expr_type.enum_id();
    let expr_type_params = expr_type.type_params();

    let enum_variants = if let Some(expr_enum_id) = expr_enum_id {
        let enum_ = ck.sa.enums[expr_enum_id].read();
        enum_.variants.len()
    } else {
        0
    };

    let mut used_variants = FixedBitSet::with_capacity(enum_variants);

    for case in &node.cases {
        ck.symtable.push_level();
        check_expr_match_case(
            ck,
            case,
            expr_enum_id,
            expr_type_params.clone(),
            &mut used_variants,
            &mut result_type,
            expected_ty.clone(),
        );
        ck.symtable.pop_level();
    }

    used_variants.toggle_range(..);

    if used_variants.count_ones(..) != 0 {
        let msg = ErrorMessage::MatchUncoveredVariant;
        ck.sa.report(ck.file_id, node.expr.span(), msg);
    }

    ck.analysis.set_ty(node.id, result_type.clone());

    result_type
}

fn check_expr_match_case(
    ck: &mut TypeCheck,
    case: &ast::MatchCaseType,
    expr_enum_id: Option<EnumDefinitionId>,
    expr_type_params: SourceTypeArray,
    used_variants: &mut FixedBitSet,
    result_type: &mut SourceType,
    expected_ty: SourceType,
) {
    let mut has_arguments = false;

    for pattern in &case.patterns {
        let arguments = check_expr_match_pattern(
            ck,
            expr_enum_id,
            expr_type_params.clone(),
            case,
            pattern,
            used_variants,
        );

        if !arguments.is_empty() {
            has_arguments = true;
        }
    }

    if has_arguments && case.patterns.len() > 1 {
        let msg = ErrorMessage::MatchMultiplePatternsWithParamsNotSupported;
        ck.sa.report(ck.file_id, case.span, msg);
    }

    let case_ty = check_expr(ck, &case.value, expected_ty.clone());

    if result_type.is_error() {
        *result_type = case_ty;
    } else if case_ty.is_error() {
        // ignore this case
    } else if !result_type.allows(ck.sa, case_ty.clone()) {
        let result_type_name = ck.ty_name(&result_type);
        let case_ty_name = ck.ty_name(&case_ty);
        let msg = ErrorMessage::MatchBranchTypesIncompatible(result_type_name, case_ty_name);
        ck.sa.report(ck.file_id, case.value.span(), msg);
    }
}

fn check_expr_match_pattern(
    ck: &mut TypeCheck,
    expr_enum_id: Option<EnumDefinitionId>,
    expr_type_params: SourceTypeArray,
    case: &ast::MatchCaseType,
    pattern: &ast::MatchPattern,
    used_variants: &mut FixedBitSet,
) -> HashMap<Name, SourceType> {
    match pattern.data {
        ast::MatchPatternData::Underscore => {
            let mut negated_used_variants = used_variants.clone();
            negated_used_variants.toggle_range(..);

            if negated_used_variants.count_ones(..) == 0 {
                let msg = ErrorMessage::MatchUnreachablePattern;
                ck.sa.report(ck.file_id, case.span, msg);
            }

            used_variants.insert_range(..);
            HashMap::new()
        }

        ast::MatchPatternData::Ident(ref ident) => {
            let sym = read_path(ck, &ident.path);

            match sym {
                Ok(SymbolKind::EnumVariant(enum_id, variant_idx)) => {
                    if Some(enum_id) == expr_enum_id {
                        check_expr_match_pattern_enum_variant(
                            ck,
                            enum_id,
                            variant_idx,
                            expr_type_params,
                            case,
                            pattern,
                            ident,
                            used_variants,
                        )
                    } else {
                        let msg = ErrorMessage::EnumVariantExpected;
                        ck.sa.report(ck.file_id, ident.path.span, msg);
                        HashMap::new()
                    }
                }

                Ok(_) => {
                    let msg = ErrorMessage::EnumVariantExpected;
                    ck.sa.report(ck.file_id, ident.path.span, msg);
                    HashMap::new()
                }

                Err(()) => HashMap::new(),
            }
        }
    }
}

fn check_expr_match_pattern_enum_variant(
    ck: &mut TypeCheck,
    enum_id: EnumDefinitionId,
    variant_idx: u32,
    expr_type_params: SourceTypeArray,
    case: &ast::MatchCaseType,
    pattern: &ast::MatchPattern,
    ident: &ast::MatchPatternIdent,
    used_variants: &mut FixedBitSet,
) -> HashMap<Name, SourceType> {
    if used_variants.contains(variant_idx as usize) {
        let msg = ErrorMessage::MatchUnreachablePattern;
        ck.sa.report(ck.file_id, case.span, msg);
    }

    used_variants.insert(variant_idx as usize);
    ck.analysis.map_idents.insert(
        pattern.id,
        IdentType::EnumValue(enum_id, expr_type_params.clone(), variant_idx),
    );

    let enum_ = ck.sa.enums.idx(enum_id);
    let enum_ = enum_.read();
    let variant = &enum_.variants[variant_idx as usize];

    let given_params = if let Some(ref params) = ident.params {
        params.len()
    } else {
        0
    };

    if given_params == 0 && ident.params.is_some() {
        let msg = ErrorMessage::MatchPatternNoParens;
        ck.sa.report(ck.file_id, case.span, msg);
    }

    let expected_params = variant.types.len();

    if given_params != expected_params {
        let msg = ErrorMessage::MatchPatternWrongNumberOfParams(given_params, expected_params);
        ck.sa.report(ck.file_id, case.span, msg);
    }

    let mut used_idents: HashMap<Name, SourceType> = HashMap::new();

    if let Some(ref params) = ident.params {
        for (idx, param) in params.iter().enumerate() {
            if let Some(ident) = &param.name {
                let ty = if idx < variant.types.len() {
                    variant.types[idx].clone()
                } else {
                    SourceType::Error
                };

                let ty = replace_type_param(ck.sa, ty, &expr_type_params, None);

                let iname = ck.sa.interner.intern(&ident.name_as_string);

                if used_idents.insert(iname, ty.clone()).is_some() {
                    let msg = ErrorMessage::VarAlreadyInPattern;
                    ck.sa.report(ck.file_id, param.span, msg);
                }

                let var_id = ck.vars.add_var(iname, ty, param.mutable);
                ck.add_local(var_id, param.span);
                ck.analysis
                    .map_vars
                    .insert(param.id, ck.vars.local_var_id(var_id));
            }
        }
    }

    used_idents
}