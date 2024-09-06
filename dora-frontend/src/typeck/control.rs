use std::collections::HashMap;
use std::sync::Arc;

use dora_parser::ast;
use fixedbitset::FixedBitSet;

use crate::error::msg::ErrorMessage;
use crate::expr_always_returns;
use crate::interner::Name;
use crate::sema::{find_impl, EnumDefinitionId, FctDefinitionId, ForTypeInfo, IdentType};
use crate::sym::SymbolKind;
use crate::typeck::{add_local, check_expr, check_pattern, read_ident, read_path, TypeCheck};
use crate::{replace_type, specialize_type, AliasReplacement, SourceType, SourceTypeArray};

pub(super) fn check_expr_while(
    ck: &mut TypeCheck,
    stmt: &ast::ExprWhileType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.enter_block_scope();
    let expr_type = check_expr(ck, &stmt.cond, SourceType::Bool);

    if !expr_type.is_error() && !expr_type.is_bool() {
        let expr_type = ck.ty_name(&expr_type);
        let msg = ErrorMessage::WhileCondType(expr_type);
        ck.sa.report(ck.file_id, stmt.span, msg);
    }

    check_loop_body(ck, &stmt.block);
    ck.leave_block_scope(stmt.id);
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
        check_for_body(ck, stmt, SourceType::Error);
        return SourceType::Unit;
    }

    if let Some((for_type_info, ret_type)) = type_supports_iterator_trait(ck, object_type.clone()) {
        // store fct ids for code generation
        ck.analysis.map_fors.insert(stmt.id, for_type_info);
        check_for_body(ck, stmt, ret_type);
        return SourceType::Unit;
    }

    if let Some((iter_fct_id, iterator_type)) =
        type_supports_into_iterator_trait(ck, object_type.clone())
    {
        let (mut for_type_info, ret_type) = type_supports_iterator_trait(ck, iterator_type.clone())
            .expect("type not implementing iterator trait");

        // store fct ids for code generation
        for_type_info.iter = Some(iter_fct_id);
        ck.analysis.map_fors.insert(stmt.id, for_type_info);

        check_for_body(ck, stmt, ret_type);
        return SourceType::Unit;
    }

    let name = ck.ty_name(&object_type);
    let msg = ErrorMessage::TypeNotUsableInForIn(name);
    ck.sa.report(ck.file_id, stmt.expr.span(), msg);

    // set invalid error type
    check_for_body(ck, stmt, SourceType::Error);
    SourceType::Unit
}

fn check_for_body(ck: &mut TypeCheck, stmt: &ast::ExprForType, ty: SourceType) {
    ck.symtable.push_level();
    ck.enter_block_scope();
    check_pattern(ck, &stmt.pattern, ty);
    check_loop_body(ck, &stmt.block);
    ck.leave_block_scope(stmt.id);
    ck.symtable.pop_level();
}

fn type_supports_into_iterator_trait(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<(FctDefinitionId, SourceType)> {
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

    let trait_ty = SourceType::new_trait(into_iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        object_type.clone(),
        &ck.type_param_defs,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);

        let iter_impl_fct_id = impl_
            .trait_method_map()
            .get(&iter_trait_fct_id)
            .cloned()
            .expect("missing impl next() method");

        let iterator_type_impl_alias_id = impl_
            .trait_alias_map()
            .get(&iterator_type_trait_alias_id)
            .cloned()
            .expect("missing impl alias");

        let iterator_type_impl_alias = ck.sa.alias(iterator_type_impl_alias_id);

        let iterator_type =
            specialize_type(ck.sa, iterator_type_impl_alias.ty(), &impl_match.binding);

        Some((iter_impl_fct_id, iterator_type))
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

    let trait_ty = SourceType::new_trait(iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        object_type.clone(),
        &ck.type_param_defs,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);

        let next_impl_fct_id = impl_
            .trait_method_map()
            .get(&next_trait_fct_id)
            .cloned()
            .expect("missing impl next() method");

        let next_impl_fct = ck.sa.fct(next_impl_fct_id);

        let item_impl_alias_id = impl_
            .trait_alias_map()
            .get(&item_trait_alias_id)
            .cloned()
            .expect("missing impl alias");

        let impl_alias = ck.sa.alias(item_impl_alias_id);

        let value_type = specialize_type(ck.sa, impl_alias.ty(), &impl_match.binding);
        let next_type = specialize_type(ck.sa, next_impl_fct.return_type(), &impl_match.binding);

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
    ck.symtable.push_level();

    if let Some((is_expr, cond)) = is_with_condition(&expr.cond) {
        let ty = check_expr(ck, &is_expr.value, SourceType::Any);
        check_pattern(ck, &is_expr.pattern, ty);
        if let Some(cond) = cond {
            let ty = check_expr(ck, cond, SourceType::Bool);

            if !ty.is_bool() && !ty.is_error() {
                let expr_type = ck.ty_name(&ty);
                let msg = ErrorMessage::IfCondType(expr_type);
                ck.sa.report(ck.file_id, expr.span, msg);
            }
        }
    } else {
        let ty = check_expr(ck, &expr.cond, SourceType::Any);

        if !ty.is_bool() && !ty.is_error() {
            let expr_type = ck.ty_name(&ty);
            let msg = ErrorMessage::IfCondType(expr_type);
            ck.sa.report(ck.file_id, expr.span, msg);
        }
    }

    let then_type = check_expr(ck, &expr.then_block, expected_ty.clone());

    ck.symtable.pop_level();

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

pub fn is_with_condition(e: &ast::Expr) -> Option<(&ast::ExprIsType, Option<&ast::Expr>)> {
    if let Some(is_expr) = e.to_is() {
        Some((is_expr, None))
    } else if let Some(e) = e.to_bin() {
        if e.lhs.is_is() && e.op == ast::BinOp::And {
            let is_expr = e.lhs.to_is().expect("missing is");
            Some((is_expr, Some(&e.rhs)))
        } else {
            None
        }
    } else {
        None
    }
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
        ck.sa.enum_(expr_enum_id).variants().len()
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
    pattern: &ast::Pattern,
    used_variants: &mut FixedBitSet,
) -> HashMap<Name, SourceType> {
    match pattern {
        ast::Pattern::Underscore(..) => {
            let mut negated_used_variants = used_variants.clone();
            negated_used_variants.toggle_range(..);

            if negated_used_variants.count_ones(..) == 0 {
                let msg = ErrorMessage::MatchUnreachablePattern;
                ck.sa.report(ck.file_id, case.span, msg);
            }

            used_variants.insert_range(..);
            HashMap::new()
        }

        ast::Pattern::LitBool(..) => unreachable!(),

        ast::Pattern::Tuple(..) => unimplemented!(),

        ast::Pattern::Ident(ref ident) => {
            let sym = read_ident(ck, &ident.name);

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
                            used_variants,
                        )
                    } else {
                        let msg = ErrorMessage::EnumVariantExpected;
                        ck.sa.report(ck.file_id, ident.span, msg);
                        HashMap::new()
                    }
                }

                Ok(_) => {
                    let msg = ErrorMessage::EnumVariantExpected;
                    ck.sa.report(ck.file_id, ident.span, msg);
                    HashMap::new()
                }

                Err(()) => HashMap::new(),
            }
        }

        ast::Pattern::ClassOrStructOrEnum(ref ident) => {
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
    pattern: &ast::Pattern,
    used_variants: &mut FixedBitSet,
) -> HashMap<Name, SourceType> {
    if used_variants.contains(variant_idx as usize) {
        let msg = ErrorMessage::MatchUnreachablePattern;
        ck.sa.report(ck.file_id, case.span, msg);
    }

    used_variants.insert(variant_idx as usize);
    ck.analysis.map_idents.insert(
        pattern.id(),
        IdentType::EnumVariant(enum_id, expr_type_params.clone(), variant_idx),
    );

    let enum_ = ck.sa.enum_(enum_id);
    let variant = &enum_.variants()[variant_idx as usize];

    let params = class_or_struct_or_enum_params(pattern);
    let given_params = params.map(|x| x.len()).unwrap_or(0);

    if given_params == 0 && params.is_some() {
        let msg = ErrorMessage::MatchPatternNoParens;
        ck.sa.report(ck.file_id, case.span, msg);
    }

    let expected_params = variant.types().len();

    if given_params != expected_params {
        let msg = ErrorMessage::PatternWrongNumberOfParams(given_params, expected_params);
        ck.sa.report(ck.file_id, case.span, msg);
    }

    let mut used_idents: HashMap<Name, SourceType> = HashMap::new();

    if let Some(params) = params {
        for (idx, param) in params.iter().enumerate() {
            match param.as_ref() {
                ast::Pattern::Underscore(..) => {
                    // Do nothing.
                }

                ast::Pattern::LitBool(..) => unreachable!(),

                ast::Pattern::Ident(ref ident) => {
                    let ty = if idx < variant.types().len() {
                        variant.types()[idx].clone()
                    } else {
                        SourceType::Error
                    };

                    let ty = replace_type(
                        ck.sa,
                        ty,
                        Some(&expr_type_params),
                        None,
                        AliasReplacement::None,
                    );

                    let iname = ck.sa.interner.intern(&ident.name.name_as_string);

                    if used_idents.insert(iname, ty.clone()).is_some() {
                        let msg = ErrorMessage::VarAlreadyInPattern;
                        ck.sa.report(ck.file_id, ident.span, msg);
                    }

                    let var_id = ck.vars.add_var(iname, ty, ident.mutable);
                    add_local(ck.sa, ck.symtable, ck.vars, var_id, ck.file_id, ident.span);
                    ck.analysis
                        .map_idents
                        .insert(ident.id, IdentType::Var(ck.vars.local_var_id(var_id)));
                }

                ast::Pattern::ClassOrStructOrEnum(..) | ast::Pattern::Tuple(..) => unreachable!(),
            }
        }
    }

    used_idents
}

pub(super) fn class_or_struct_or_enum_params(p: &ast::Pattern) -> Option<&Vec<Arc<ast::Pattern>>> {
    match p {
        ast::Pattern::Underscore(..) | ast::Pattern::Tuple(..) | ast::Pattern::LitBool(..) => {
            unreachable!()
        }
        ast::Pattern::Ident(..) => None,
        ast::Pattern::ClassOrStructOrEnum(p) => p.params.as_ref(),
    }
}
