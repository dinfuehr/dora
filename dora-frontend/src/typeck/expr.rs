use dora_parser::ast;
use fixedbitset::FixedBitSet;

use std::collections::HashMap;

use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{create_tuple, EnumDefinitionId, IdentType};
use crate::specialize::replace_type_param;
use crate::sym::SymbolKind;
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeck::{
    check_expr_call, check_expr_for, check_expr_if, check_expr_return, check_expr_while, TypeCheck,
};

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    e: &ast::ExprData,
    expected_ty: SourceType,
) -> SourceType {
    match *e {
        ast::ExprData::LitChar(ref expr) => ck.check_expr_lit_char(expr, expected_ty),
        ast::ExprData::LitInt(ref expr) => ck.check_expr_lit_int(expr, false, expected_ty),
        ast::ExprData::LitFloat(ref expr) => ck.check_expr_lit_float(expr, false, expected_ty),
        ast::ExprData::LitStr(ref expr) => ck.check_expr_lit_str(expr, expected_ty),
        ast::ExprData::Template(ref expr) => ck.check_expr_template(expr, expected_ty),
        ast::ExprData::LitBool(ref expr) => ck.check_expr_lit_bool(expr, expected_ty),
        ast::ExprData::Ident(ref expr) => check_expr_ident(ck, expr, expected_ty),
        ast::ExprData::Un(ref expr) => ck.check_expr_un(expr, expected_ty),
        ast::ExprData::Bin(ref expr) => ck.check_expr_bin(expr, expected_ty),
        ast::ExprData::Call(ref expr) => check_expr_call(ck, expr, expected_ty),
        ast::ExprData::TypeParam(ref expr) => ck.check_expr_type_param(expr, expected_ty),
        ast::ExprData::Path(ref expr) => ck.check_expr_path(expr, expected_ty),
        ast::ExprData::Dot(ref expr) => ck.check_expr_dot(expr, expected_ty),
        ast::ExprData::This(ref expr) => ck.check_expr_this(expr, expected_ty),
        ast::ExprData::Conv(ref expr) => ck.check_expr_conv(expr, expected_ty),
        ast::ExprData::Lambda(ref expr) => ck.check_expr_lambda(expr, expected_ty),
        ast::ExprData::Block(ref expr) => check_expr_block(ck, expr, expected_ty),
        ast::ExprData::If(ref expr) => check_expr_if(ck, expr, expected_ty),
        ast::ExprData::Tuple(ref expr) => check_expr_tuple(ck, expr, expected_ty),
        ast::ExprData::Paren(ref expr) => check_expr_paren(ck, expr, expected_ty),
        ast::ExprData::Match(ref expr) => check_expr_match(ck, expr, expected_ty),
        ast::ExprData::For(ref expr) => check_expr_for(ck, expr, expected_ty),
        ast::ExprData::While(ref expr) => check_expr_while(ck, expr, expected_ty),
        ast::ExprData::Return(ref expr) => check_expr_return(ck, expr, expected_ty),
        ast::ExprData::Break(..) | ast::ExprData::Continue(..) => {
            ck.check_expr_break_and_continue(e, expected_ty)
        }
        ast::ExprData::Error { .. } => SourceType::Error,
    }
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
            let sym = ck.read_path(&ident.path);

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

pub(super) fn check_expr_block(
    ck: &mut TypeCheck,
    block: &ast::ExprBlockType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    for stmt in &block.stmts {
        ck.visit_stmt(stmt);
    }

    let ty = if let Some(ref expr) = block.expr {
        check_expr(ck, expr, SourceType::Any)
    } else {
        SourceType::Unit
    };

    ck.analysis.set_ty(block.id, ty.clone());
    ck.symtable.pop_level();

    ty
}

pub(super) fn check_expr_tuple(
    ck: &mut TypeCheck,
    tuple: &ast::ExprTupleType,
    _expected_ty: SourceType,
) -> SourceType {
    let mut subtypes = Vec::new();

    if tuple.values.is_empty() {
        ck.analysis.set_ty(tuple.id, SourceType::Unit);
        return SourceType::Unit;
    }

    for value in &tuple.values {
        let subtype = check_expr(ck, value, SourceType::Any);
        subtypes.push(subtype);
    }

    let ty = create_tuple(ck.sa, subtypes);
    ck.analysis.set_ty(tuple.id, ty.clone());

    ty
}

pub(super) fn check_expr_paren(
    ck: &mut TypeCheck,
    paren: &ast::ExprParenType,
    _expected_ty: SourceType,
) -> SourceType {
    let ty = check_expr(ck, &paren.expr, SourceType::Any);
    ck.analysis.set_ty(paren.id, ty.clone());

    ty
}

pub(super) fn check_expr_ident(
    ck: &mut TypeCheck,
    e: &ast::ExprIdentType,
    expected_ty: SourceType,
) -> SourceType {
    let interned_name: Name = ck.sa.interner.intern(&e.name);
    let sym = ck.symtable.get(interned_name);

    match sym {
        Some(SymbolKind::Var(var_id)) => {
            let ty = ck.vars.get_var(var_id).ty.clone();
            ck.analysis.set_ty(e.id, ty.clone());

            // Variable may have to be context-allocated.
            let ident = ck
                .vars
                .check_context_allocated(var_id, &mut ck.outer_context_access_in_function);
            ck.analysis.map_idents.insert(e.id, ident);

            ty
        }

        Some(SymbolKind::Global(globalid)) => {
            let global_var = ck.sa.globals.idx(globalid);
            let ty = global_var.read().ty.clone();
            ck.analysis.set_ty(e.id, ty.clone());

            ck.analysis
                .map_idents
                .insert(e.id, IdentType::Global(globalid));

            ty
        }

        Some(SymbolKind::Const(const_id)) => {
            let const_ = &ck.sa.consts[const_id];

            ck.analysis.set_ty(e.id, const_.ty());

            ck.analysis
                .map_idents
                .insert(e.id, IdentType::Const(const_id));

            const_.ty()
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => ck.check_enum_value_without_args_id(
            e.id,
            e.span,
            expected_ty,
            enum_id,
            SourceTypeArray::empty(),
            variant_idx,
        ),

        None => {
            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::UnknownIdentifier(e.name.clone()),
            );
            SourceType::Error
        }

        _ => {
            ck.sa
                .report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            SourceType::Error
        }
    }
}
