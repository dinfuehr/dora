use std::sync::Arc;

use dora_bytecode::Intrinsic;
use dora_parser::{ast, Span};
use fixedbitset::FixedBitSet;

use std::collections::HashMap;

use crate::access::{
    class_field_accessible_from, const_accessible_from, enum_accessible_from,
    global_accessible_from, module_accessible_from, struct_field_accessible_from,
};
use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use crate::sema::{
    create_tuple, find_field_in_class, find_impl, implements_trait, AnalysisData, CallType,
    EnumDefinitionId, FctDefinition, FctParent, IdentType, ModuleDefinitionId, NestedVarId,
};
use crate::specialize::replace_type_param;
use crate::sym::SymbolKind;
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeck::{
    check_expr_break_and_continue, check_expr_call, check_expr_call_enum_args, check_expr_for,
    check_expr_if, check_expr_return, check_expr_while, check_lit_char, check_lit_float,
    check_lit_int, check_lit_str, check_stmt, is_simple_enum, lookup_method, TypeCheck,
};
use crate::typeparamck::{self, ErrorReporting};

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    e: &ast::ExprData,
    expected_ty: SourceType,
) -> SourceType {
    match *e {
        ast::ExprData::LitChar(ref expr) => check_expr_lit_char(ck, expr, expected_ty),
        ast::ExprData::LitInt(ref expr) => check_expr_lit_int(ck, expr, false, expected_ty),
        ast::ExprData::LitFloat(ref expr) => check_expr_lit_float(ck, expr, false, expected_ty),
        ast::ExprData::LitStr(ref expr) => check_expr_lit_str(ck, expr, expected_ty),
        ast::ExprData::Template(ref expr) => check_expr_template(ck, expr, expected_ty),
        ast::ExprData::LitBool(ref expr) => check_expr_lit_bool(ck, expr, expected_ty),
        ast::ExprData::Ident(ref expr) => check_expr_ident(ck, expr, expected_ty),
        ast::ExprData::Un(ref expr) => check_expr_un(ck, expr, expected_ty),
        ast::ExprData::Bin(ref expr) => check_expr_bin(ck, expr, expected_ty),
        ast::ExprData::Call(ref expr) => check_expr_call(ck, expr, expected_ty),
        ast::ExprData::TypeParam(ref expr) => check_expr_type_param(ck, expr, expected_ty),
        ast::ExprData::Path(ref expr) => check_expr_path(ck, expr, expected_ty),
        ast::ExprData::Dot(ref expr) => check_expr_dot(ck, expr, expected_ty),
        ast::ExprData::This(ref expr) => check_expr_this(ck, expr, expected_ty),
        ast::ExprData::Conv(ref expr) => check_expr_conv(ck, expr, expected_ty),
        ast::ExprData::Lambda(ref expr) => check_expr_lambda(ck, expr, expected_ty),
        ast::ExprData::Block(ref expr) => check_expr_block(ck, expr, expected_ty),
        ast::ExprData::If(ref expr) => check_expr_if(ck, expr, expected_ty),
        ast::ExprData::Tuple(ref expr) => check_expr_tuple(ck, expr, expected_ty),
        ast::ExprData::Paren(ref expr) => check_expr_paren(ck, expr, expected_ty),
        ast::ExprData::Match(ref expr) => check_expr_match(ck, expr, expected_ty),
        ast::ExprData::For(ref expr) => check_expr_for(ck, expr, expected_ty),
        ast::ExprData::While(ref expr) => check_expr_while(ck, expr, expected_ty),
        ast::ExprData::Return(ref expr) => check_expr_return(ck, expr, expected_ty),
        ast::ExprData::Break(..) | ast::ExprData::Continue(..) => {
            check_expr_break_and_continue(ck, e, expected_ty)
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

pub(super) fn check_expr_block(
    ck: &mut TypeCheck,
    block: &ast::ExprBlockType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    for stmt in &block.stmts {
        check_stmt(ck, stmt);
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

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_value_without_args_id(
            ck,
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

pub(super) fn check_expr_assign(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    if e.lhs.is_call() {
        check_expr_assign_call(ck, e);
    } else if e.lhs.is_dot() {
        check_expr_assign_field(ck, e);
    } else if e.lhs.is_ident() {
        check_expr_assign_ident(ck, e);
    } else {
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::LvalueExpected);
    }

    ck.analysis.set_ty(e.id, SourceType::Unit);
}

fn check_expr_assign_ident(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    ck.analysis.set_ty(e.id, SourceType::Unit);

    let lhs_ident = e.lhs.to_ident().unwrap();
    let sym = ck.symtable.get_string(ck.sa, &lhs_ident.name);

    let lhs_type = match sym {
        Some(SymbolKind::Var(var_id)) => {
            if !ck.vars.get_var(var_id).mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            // Variable may have to be context-allocated.
            let ident = ck
                .vars
                .check_context_allocated(var_id, &mut ck.outer_context_access_in_function);
            ck.analysis.map_idents.insert(e.lhs.id(), ident);

            ck.vars.get_var(var_id).ty.clone()
        }

        Some(SymbolKind::Global(global_id)) => {
            let global_var = ck.sa.globals.idx(global_id);
            let global_var = global_var.read();

            if !e.initializer && !global_var.mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            ck.analysis
                .map_idents
                .insert(e.lhs.id(), IdentType::Global(global_id));
            global_var.ty.clone()
        }

        None => {
            ck.sa.report(
                ck.file_id,
                lhs_ident.span,
                ErrorMessage::UnknownIdentifier(lhs_ident.name.clone()),
            );

            return;
        }

        _ => {
            ck.sa
                .report(ck.file_id, lhs_ident.span, ErrorMessage::LvalueExpected);

            return;
        }
    };

    let rhs_type = check_expr(ck, &e.rhs, lhs_type.clone());

    if !lhs_type.is_error() && !rhs_type.is_error() && !lhs_type.allows(ck.sa, rhs_type.clone()) {
        let ident = e.lhs.to_ident().unwrap();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);

        ck.analysis.set_ty(e.id, SourceType::Unit);

        let msg = ErrorMessage::AssignType(ident.name.clone(), lhs_type, rhs_type);
        ck.sa.report(ck.file_id, e.span, msg);
    }
}

fn check_expr_assign_call(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    let call = e.lhs.to_call().unwrap();
    let expr_type = check_expr(ck, &call.callee, SourceType::Any);

    let mut arg_types: Vec<SourceType> = call
        .args
        .iter()
        .map(|arg| check_expr(ck, arg, SourceType::Any))
        .collect();

    let value_type = check_expr(ck, &e.rhs, SourceType::Any);

    let name = ck.sa.interner.intern("set");
    arg_types.push(value_type);

    if let Some(descriptor) = ck.find_method(
        e.span,
        expr_type.clone(),
        false,
        name,
        &arg_types,
        &SourceTypeArray::empty(),
    ) {
        let call_type = CallType::Expr(expr_type, descriptor.fct_id, descriptor.type_params);
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));
    }
}

fn check_expr_assign_field(ck: &mut TypeCheck, e: &ast::ExprBinType) {
    let field_expr = e.lhs.to_dot().unwrap();

    let name = match field_expr.rhs.to_ident() {
        Some(ident) => ident.name.clone(),

        None => {
            let msg = ErrorMessage::NameExpected;
            ck.sa.report(ck.file_id, e.span, msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            return;
        }
    };

    let interned_name = ck.sa.interner.intern(&name);

    let object_type = check_expr(ck, &field_expr.lhs, SourceType::Any);

    if object_type.cls_id().is_some() {
        if let Some((cls_ty, field_id, _)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_name)
        {
            let ident_type = IdentType::Field(cls_ty.clone(), field_id);
            ck.analysis
                .map_idents
                .insert_or_replace(e.lhs.id(), ident_type);

            let cls = &ck.sa.classes[cls_ty.cls_id().expect("no class")];
            let field = &cls.fields[field_id];

            let class_type_params = cls_ty.type_params();

            let fty = replace_type_param(ck.sa, field.ty(), &class_type_params, None);

            if !e.initializer && !field.mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            let rhs_type = check_expr(ck, &e.rhs, fty.clone());

            if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                let object_type = ck.ty_name(&object_type);
                let lhs_type = ck.ty_name(&fty);
                let rhs_type = ck.ty_name(&rhs_type);

                let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                ck.sa.report(ck.file_id, e.span, msg);
            }

            ck.analysis.set_ty(e.id, SourceType::Unit);
            return;
        }
    }

    if object_type.is_struct() {
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::StructFieldImmutable);

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        check_expr(ck, &e.rhs, SourceType::Any);

        ck.analysis.set_ty(e.id, SourceType::Unit);
        return;
    }

    // We want to see syntax expressions in the assignment expressions even when we can't
    // find the given field.
    check_expr(ck, &e.rhs, SourceType::Any);

    // field not found, report error
    let expr_name = ck.ty_name(&object_type);
    let msg = ErrorMessage::UnknownField(name, expr_name);
    ck.sa.report(ck.file_id, field_expr.op_span, msg);

    ck.analysis.set_ty(e.id, SourceType::Unit);
}

pub(super) fn check_expr_dot(
    ck: &mut TypeCheck,
    e: &ast::ExprDotType,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, &e.lhs, SourceType::Any);

    if object_type.is_tuple() {
        return check_expr_dot_tuple(ck, e, object_type);
    }

    let name = match e.rhs.to_ident() {
        Some(ident) => ident.name.clone(),

        None => {
            let msg = ErrorMessage::NameExpected;
            ck.sa.report(ck.file_id, e.op_span, msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    };

    let interned_name = ck.sa.interner.intern(&name);

    if let Some(struct_id) = object_type.struct_id() {
        let struct_ = &ck.sa.structs[struct_id];
        if let Some(&field_id) = struct_.field_names.get(&interned_name) {
            let ident_type = IdentType::StructField(object_type.clone(), field_id);
            ck.analysis.map_idents.insert_or_replace(e.id, ident_type);

            let field = &struct_.fields[field_id.to_usize()];
            let struct_type_params = object_type.type_params();
            let fty = replace_type_param(ck.sa, field.ty(), &struct_type_params, None);

            if !struct_field_accessible_from(ck.sa, struct_id, field_id, ck.module_id) {
                let name = ck.sa.interner.str(field.name).to_string();
                let msg = ErrorMessage::NotAccessible(name);
                ck.sa.report(ck.file_id, e.rhs.span(), msg);
            }

            ck.analysis.set_ty(e.id, fty.clone());
            return fty;
        }
    }

    if object_type.cls_id().is_some() {
        if let Some((cls_ty, field_id, _)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_name)
        {
            let ident_type = IdentType::Field(cls_ty.clone(), field_id);
            ck.analysis.map_idents.insert_or_replace(e.id, ident_type);

            let cls_id = cls_ty.cls_id().expect("no class");
            let cls = &ck.sa.classes[cls_id];
            let field = &cls.fields[field_id];
            let class_type_params = cls_ty.type_params();
            let fty = replace_type_param(ck.sa, field.ty(), &class_type_params, None);

            if !class_field_accessible_from(ck.sa, cls_id, field_id, ck.module_id) {
                let name = ck.sa.interner.str(field.name).to_string();
                let msg = ErrorMessage::NotAccessible(name);
                ck.sa.report(ck.file_id, e.rhs.span(), msg);
            }

            ck.analysis.set_ty(e.id, fty.clone());
            return fty;
        }
    }

    // field not found, report error
    if !object_type.is_error() {
        let expr_name = ck.ty_name(&object_type);
        let msg = ErrorMessage::UnknownField(name, expr_name);
        ck.sa.report(ck.file_id, e.rhs.span(), msg);
    }

    ck.analysis.set_ty(e.id, SourceType::Error);

    SourceType::Error
}

fn check_expr_dot_tuple(
    ck: &mut TypeCheck,
    e: &ast::ExprDotType,
    object_type: SourceType,
) -> SourceType {
    let index = match e.rhs.to_lit_int() {
        Some(literal) => {
            let (ty, value_i64, _) =
                check_lit_int(ck.sa, ck.file_id, literal, false, SourceType::Any);

            if ty.is_float() {
                ck.sa
                    .report(ck.file_id, literal.span, ErrorMessage::IndexExpected);
            }

            ck.analysis.set_literal_value(literal.id, value_i64, 0.0);

            value_i64 as u64
        }

        None => {
            let msg = ErrorMessage::IndexExpected;
            ck.sa.report(ck.file_id, e.rhs.span(), msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    };

    let subtypes = object_type.tuple_subtypes();

    if index >= subtypes.len() as u64 {
        let msg = ErrorMessage::IllegalTupleIndex(index, ck.ty_name(&object_type));
        ck.sa.report(ck.file_id, e.op_span, msg);

        ck.analysis.set_ty(e.id, SourceType::Error);
        return SourceType::Error;
    }

    let ty = subtypes[usize::try_from(index).unwrap()].clone();
    ck.analysis.set_ty(e.id, ty.clone());

    ty
}

pub(super) fn check_expr_this(
    ck: &mut TypeCheck,
    e: &ast::ExprSelfType,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.is_self_available {
        let msg = ErrorMessage::ThisUnavailable;
        ck.sa.report(ck.file_id, e.span, msg);
        ck.analysis.set_ty(e.id, SourceType::Error);
        return SourceType::Error;
    }

    assert!(ck.is_self_available);
    let var_id = NestedVarId(0);
    let ident = ck
        .vars
        .check_context_allocated(var_id, &mut ck.outer_context_access_in_function);
    ck.analysis.map_idents.insert(e.id, ident);

    let var = ck.vars.get_var(var_id);
    ck.analysis.set_ty(e.id, var.ty.clone());
    var.ty.clone()
}

fn check_expr_conv(
    ck: &mut TypeCheck,
    e: &ast::ExprConvType,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, &e.object, SourceType::Any);
    ck.analysis.set_ty(e.object.id(), object_type.clone());

    let check_type = ck.read_type(&e.data_type);
    ck.analysis.set_ty(e.data_type.id(), check_type.clone());

    if check_type.is_trait() {
        let implements = implements_trait(
            ck.sa,
            object_type.clone(),
            &ck.type_param_defs,
            check_type.clone(),
        );

        if !implements {
            let object_type = ck.ty_name(&object_type);
            let check_type = ck.ty_name(&check_type);

            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::TypeNotImplementingTrait(object_type, check_type),
            );
        }

        ck.analysis.set_ty(e.id, check_type.clone());
        check_type
    } else if !check_type.is_error() {
        let name = ck.ty_name(&check_type);
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::TraitExpected(name));
        let ty = SourceType::Error;
        ck.analysis.set_ty(e.id, ty.clone());
        ty
    } else {
        SourceType::Error
    }
}

pub(super) fn check_expr_lit_int(
    ck: &mut TypeCheck,
    e: &ast::ExprLitIntType,
    negate: bool,
    expected_ty: SourceType,
) -> SourceType {
    let (ty, value_i64, value_f64) = check_lit_int(ck.sa, ck.file_id, e, negate, expected_ty);

    ck.analysis.set_ty(e.id, ty.clone());
    ck.analysis.set_literal_value(e.id, value_i64, value_f64);

    ty
}

fn check_expr_lit_float(
    ck: &mut TypeCheck,
    e: &ast::ExprLitFloatType,
    negate: bool,
    _expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_float(ck.sa, ck.file_id, e, negate);

    ck.analysis.set_ty(e.id, ty.clone());
    ck.analysis.set_literal_value(e.id, 0, value);

    ty
}

fn check_expr_lit_bool(
    ck: &mut TypeCheck,
    e: &ast::ExprLitBoolType,
    _expected_ty: SourceType,
) -> SourceType {
    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_lit_char(
    ck: &mut TypeCheck,
    e: &ast::ExprLitCharType,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_char(ck.sa, ck.file_id, e);

    ck.analysis.set_ty(e.id, SourceType::Char);
    ck.analysis.set_literal_char(e.id, value);

    SourceType::Char
}

fn check_expr_lit_str(
    ck: &mut TypeCheck,
    e: &ast::ExprLitStrType,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_str(ck.sa, ck.file_id, e);

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.analysis.set_ty(e.id, str_ty.clone());
    ck.analysis.set_literal_string(e.id, value);

    str_ty
}

fn check_expr_template(
    ck: &mut TypeCheck,
    e: &ast::ExprTemplateType,
    expected_ty: SourceType,
) -> SourceType {
    let stringable_trait = ck.sa.known.traits.stringable();
    let stringable_trait_ty = SourceType::new_trait(stringable_trait);

    for (idx, part) in e.parts.iter().enumerate() {
        if idx % 2 != 0 {
            let part_expr = check_expr(ck, part, SourceType::Any);

            if part_expr.is_error() {
                continue;
            }

            if let SourceType::TypeParam(id) = part_expr {
                if ck
                    .type_param_defs
                    .implements_trait(id, stringable_trait_ty.clone())
                {
                    continue;
                }
            } else {
                let stringable_impl_id = find_impl(
                    ck.sa,
                    part_expr.clone(),
                    &ck.type_param_defs,
                    stringable_trait_ty.clone(),
                );

                if let Some(stringable_impl_id) = stringable_impl_id {
                    let impl_ = ck.sa.impls[stringable_impl_id].read();
                    let name = ck.sa.interner.intern("toString");
                    let to_string_id = impl_
                        .instance_names
                        .get(&name)
                        .cloned()
                        .expect("method toString() not found");

                    ck.analysis.map_templates.insert(part.id(), to_string_id);
                    continue;
                }
            }

            let ty = ck.ty_name(&part_expr);
            ck.sa.report(
                ck.file_id,
                part.span(),
                ErrorMessage::ExpectedStringable(ty),
            );
        } else {
            match part.as_ref() {
                ast::ExprData::LitStr(ref e) => {
                    check_expr_lit_str(ck, e, expected_ty.clone());
                }

                _ => unreachable!(),
            }
        }
    }

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.analysis.set_ty(e.id, str_ty.clone());

    str_ty
}

pub(super) fn check_expr_un(
    ck: &mut TypeCheck,
    e: &ast::ExprUnType,
    expected_ty: SourceType,
) -> SourceType {
    if e.op == ast::UnOp::Neg && e.opnd.is_lit_int() {
        let expr_type = check_expr_lit_int(ck, e.opnd.to_lit_int().unwrap(), true, expected_ty);
        ck.analysis.set_ty(e.id, expr_type.clone());
        return expr_type;
    }

    let opnd = check_expr(ck, &e.opnd, SourceType::Any);

    match e.op {
        ast::UnOp::Plus => check_expr_un_method(ck, e, e.op, "unaryPlus", opnd),
        ast::UnOp::Neg => check_expr_un_method(ck, e, e.op, "unaryMinus", opnd),
        ast::UnOp::Not => check_expr_un_method(ck, e, e.op, "not", opnd),
    }
}

fn check_expr_un_method(
    ck: &mut TypeCheck,
    e: &ast::ExprUnType,
    op: ast::UnOp,
    name: &str,
    ty: SourceType,
) -> SourceType {
    let name = ck.sa.interner.intern(name);
    let call_types = [];

    if !ty.is_error() {
        if let Some(descriptor) = lookup_method(
            ck.sa,
            ty.clone(),
            ck.type_param_defs,
            false,
            name,
            &call_types,
            &SourceTypeArray::empty(),
        ) {
            let call_type = CallType::Method(ty.clone(), descriptor.fct_id, descriptor.type_params);
            ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

            ck.analysis.set_ty(e.id, descriptor.return_type.clone());
            return descriptor.return_type;
        }

        let ty = ck.ty_name(&ty);
        let msg = ErrorMessage::UnOpType(op.as_str().into(), ty);

        ck.sa.report(ck.file_id, e.span, msg);
    }

    ck.analysis.set_ty(e.id, SourceType::Error);

    SourceType::Error
}

pub(super) fn check_expr_bin(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    _expected_ty: SourceType,
) -> SourceType {
    if e.op.is_any_assign() {
        check_expr_assign(ck, e);
        return SourceType::Unit;
    }

    let lhs_type = check_expr(ck, &e.lhs, SourceType::Any);
    let rhs_type = check_expr(ck, &e.rhs, SourceType::Any);

    if lhs_type.is_error() || rhs_type.is_error() {
        ck.analysis.set_ty(e.id, SourceType::Error);
        return SourceType::Error;
    }

    match e.op {
        ast::BinOp::Or | ast::BinOp::And => check_expr_bin_bool(ck, e, e.op, lhs_type, rhs_type),
        ast::BinOp::Cmp(cmp) => check_expr_bin_cmp(ck, e, cmp, lhs_type, rhs_type),
        ast::BinOp::Add => check_expr_bin_method(ck, e, e.op, "plus", lhs_type, rhs_type),
        ast::BinOp::Sub => check_expr_bin_method(ck, e, e.op, "minus", lhs_type, rhs_type),
        ast::BinOp::Mul => check_expr_bin_method(ck, e, e.op, "times", lhs_type, rhs_type),
        ast::BinOp::Div => check_expr_bin_method(ck, e, e.op, "div", lhs_type, rhs_type),
        ast::BinOp::Mod => check_expr_bin_method(ck, e, e.op, "modulo", lhs_type, rhs_type),
        ast::BinOp::BitOr => check_expr_bin_method(ck, e, e.op, "bitwiseOr", lhs_type, rhs_type),
        ast::BinOp::BitAnd => check_expr_bin_method(ck, e, e.op, "bitwiseAnd", lhs_type, rhs_type),
        ast::BinOp::BitXor => check_expr_bin_method(ck, e, e.op, "bitwiseXor", lhs_type, rhs_type),
        ast::BinOp::ShiftL => check_expr_bin_method(ck, e, e.op, "shiftLeft", lhs_type, rhs_type),
        ast::BinOp::ArithShiftR => {
            check_expr_bin_method(ck, e, e.op, "shiftRightSigned", lhs_type, rhs_type)
        }
        ast::BinOp::LogicalShiftR => {
            check_expr_bin_method(ck, e, e.op, "shiftRight", lhs_type, rhs_type)
        }
        ast::BinOp::Assign => unreachable!(),
    }
}

fn check_expr_bin_bool(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    check_type(ck, e, op, lhs_type, rhs_type, SourceType::Bool);
    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_bin_method(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    name: &str,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    let name = ck.sa.interner.intern(name);
    let call_types = [rhs_type.clone()];

    if let Some(descriptor) = lookup_method(
        ck.sa,
        lhs_type.clone(),
        ck.type_param_defs,
        false,
        name,
        &call_types,
        &SourceTypeArray::empty(),
    ) {
        let call_type =
            CallType::Method(lhs_type.clone(), descriptor.fct_id, descriptor.type_params);
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));

        ck.analysis.set_ty(e.id, descriptor.return_type.clone());

        descriptor.return_type
    } else {
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

        ck.sa.report(ck.file_id, e.span, msg);

        ck.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }
}

fn check_expr_bin_cmp(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    cmp: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    match cmp {
        ast::CmpOp::Is | ast::CmpOp::IsNot => {
            if !lhs_type.allows(ck.sa, rhs_type.clone())
                && !rhs_type.allows(ck.sa, lhs_type.clone())
            {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.sa.report(
                    ck.file_id,
                    e.span,
                    ErrorMessage::TypesIncompatible(lhs_type, rhs_type),
                );
            }

            ck.analysis.set_ty(e.id, SourceType::Bool);
            return SourceType::Bool;
        }

        ast::CmpOp::Eq | ast::CmpOp::Ne => {
            if is_simple_enum(ck.sa, lhs_type.clone()) {
                check_expr_cmp_enum(ck, e, cmp, lhs_type, rhs_type)
            } else {
                check_expr_bin_method(ck, e, e.op, "equals", lhs_type, rhs_type);
            }
        }

        _ => {
            check_expr_bin_method(ck, e, e.op, "compareTo", lhs_type, rhs_type);
        }
    }

    ck.analysis.set_ty(e.id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_cmp_enum(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) {
    if lhs_type.allows(ck.sa, rhs_type.clone()) {
        let intrinsic = match op {
            ast::CmpOp::Eq => Intrinsic::EnumEq,
            ast::CmpOp::Ne => Intrinsic::EnumNe,
            _ => unreachable!(),
        };
        let call_type = CallType::Intrinsic(intrinsic);
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));

        ck.analysis.set_ty(e.id, SourceType::Bool);
    } else {
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType("equals".into(), lhs_type, rhs_type);

        ck.sa.report(ck.file_id, e.span, msg);

        ck.analysis.set_ty(e.id, SourceType::Error);
    }
}

fn check_expr_lambda(
    ck: &mut TypeCheck,
    node: &Arc<ast::Function>,
    _expected_ty: SourceType,
) -> SourceType {
    let ret = if let Some(ref ret_type) = node.return_type {
        ck.read_type(ret_type)
    } else {
        SourceType::Unit
    };

    ck.contains_lambda = true;

    let mut params = Vec::new();

    for param in &node.params {
        params.push(ck.read_type(&param.data_type));
    }

    let ty = SourceType::Lambda(SourceTypeArray::with(params.clone()), Box::new(ret.clone()));

    let mut params_with_ctxt = vec![SourceType::Ptr];
    params_with_ctxt.append(&mut params);

    let name = ck.sa.interner.intern("<closure>");

    let mut lambda = FctDefinition::new(
        ck.package_id,
        ck.module_id,
        ck.file_id,
        node,
        ParsedModifierList::default(),
        name,
        FctParent::Function,
    );
    lambda.param_types = params_with_ctxt;
    lambda.return_type = ret;
    lambda.type_params = ck.type_param_defs.clone();
    let lambda_fct_id = ck.sa.add_fct(lambda);
    ck.analysis.map_lambdas.insert(node.id, lambda_fct_id);

    {
        let lambda = ck.sa.fcts.idx(lambda_fct_id);

        let mut analysis = AnalysisData::new();
        analysis.outer_context_infos = ck.outer_context_classes.clone();

        {
            let lambda = lambda.read();

            let mut typeck = TypeCheck {
                sa: ck.sa,
                type_param_defs: &lambda.type_params,
                package_id: ck.package_id,
                module_id: ck.module_id,
                file_id: ck.file_id,
                analysis: &mut analysis,
                symtable: &mut ck.symtable,
                in_loop: false,
                is_lambda: true,
                param_types: lambda.param_types.clone(),
                return_type: Some(lambda.return_type.clone()),
                has_hidden_self_argument: true,
                is_self_available: ck.is_self_available,
                vars: ck.vars,
                contains_lambda: false,
                outer_context_classes: ck.outer_context_classes,
                outer_context_access_in_function: false,
                outer_context_access_from_lambda: false,
            };

            typeck.check_fct(&*lambda, &node);
        }

        if analysis.outer_context_access() {
            ck.outer_context_access_from_lambda = true
        }

        lambda.write().analysis = Some(analysis);
    }

    ck.analysis.set_ty(node.id, ty.clone());

    ty
}

pub(super) fn check_enum_value_with_args(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
    arg_types: &[SourceType],
) -> SourceType {
    let enum_ = ck.sa.enums.idx(enum_id);
    let enum_ = enum_.read();
    let variant = &enum_.variants[variant_idx as usize];

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible(enum_.name(ck.sa));
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let type_params_ok = typeparamck::check_enum(
        ck.sa,
        ck.type_param_defs,
        enum_id,
        &type_params,
        ErrorReporting::Yes(ck.file_id, e.span),
    );

    if !type_params_ok {
        ck.analysis.set_ty(e.id, SourceType::Error);
        return SourceType::Error;
    }

    if !check_expr_call_enum_args(ck, enum_id, type_params.clone(), variant, arg_types) {
        let enum_name = ck.sa.interner.str(enum_.name).to_string();
        let variant_name = ck.sa.interner.str(variant.name).to_string();
        let variant_types = variant
            .types
            .iter()
            .map(|a| a.name_enum(ck.sa, &*enum_))
            .collect::<Vec<_>>();
        let arg_types = arg_types.iter().map(|a| ck.ty_name(a)).collect::<Vec<_>>();
        let msg =
            ErrorMessage::EnumArgsIncompatible(enum_name, variant_name, variant_types, arg_types);
        ck.sa.report(ck.file_id, e.span, msg);
    } else if variant.types.is_empty() {
        let enum_name = ck.sa.interner.str(enum_.name).to_string();
        let variant_name = ck.sa.interner.str(variant.name).to_string();
        let msg = ErrorMessage::EnumArgsNoParens(enum_name, variant_name);
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let ty = SourceType::Enum(enum_id, type_params);

    ck.analysis
        .map_calls
        .insert(e.id, Arc::new(CallType::Enum(ty.clone(), variant_idx)));

    ck.analysis.set_ty(e.id, ty.clone());
    ty
}

pub(super) fn check_expr_path(
    ck: &mut TypeCheck,
    e: &ast::ExprPathType,
    expected_ty: SourceType,
) -> SourceType {
    let (container_expr, type_params) = if let Some(expr_type_params) = e.lhs.to_type_param() {
        let type_params: Vec<SourceType> = expr_type_params
            .args
            .iter()
            .map(|p| ck.read_type(p))
            .collect();
        let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

        (&expr_type_params.callee, type_params)
    } else {
        (&e.lhs, SourceTypeArray::empty())
    };

    let sym = match read_path_expr(ck, container_expr) {
        Ok(sym) => sym,
        Err(()) => {
            ck.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    };

    let element_name = if let Some(ident) = e.rhs.to_ident() {
        ident.name.clone()
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, e.rhs.span(), msg);
        return SourceType::Error;
    };

    match sym {
        Some(SymbolKind::Enum(id)) => check_enum_value_without_args(
            ck,
            e.id,
            e.op_span,
            expected_ty,
            id,
            type_params,
            element_name,
        ),

        Some(SymbolKind::Module(module_id)) => {
            check_expr_path_module(ck, e, expected_ty, module_id, element_name)
        }

        _ => {
            let msg = ErrorMessage::InvalidLeftSideOfSeparator;
            ck.sa.report(ck.file_id, e.lhs.span(), msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            SourceType::Error
        }
    }
}

pub(super) fn read_path_expr(
    ck: &mut TypeCheck,
    expr: &ast::ExprData,
) -> Result<Option<SymbolKind>, ()> {
    if let Some(expr_path) = expr.to_path() {
        let sym = read_path_expr(ck, &expr_path.lhs)?;

        let element_name = if let Some(ident) = expr_path.rhs.to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, expr_path.rhs.span(), msg);
            return Err(());
        };

        let interned_element_name = ck.sa.interner.intern(&element_name);

        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = &ck.sa.modules[module_id].read();
                let symtable = module.table.read();
                let sym = symtable.get(interned_element_name);

                Ok(sym)
            }

            _ => {
                let msg = ErrorMessage::ExpectedModule;
                ck.sa.report(ck.file_id, expr.span(), msg);
                Err(())
            }
        }
    } else if let Some(expr_ident) = expr.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        Ok(sym)
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, expr.span(), msg);
        Err(())
    }
}

fn check_enum_value_without_args(
    ck: &mut TypeCheck,
    expr_id: ast::NodeId,
    expr_span: Span,
    _expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    name: String,
) -> SourceType {
    let enum_ = ck.sa.enums[enum_id].read();

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible(enum_.name(ck.sa));
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    let type_params_ok = typeparamck::check_enum(
        ck.sa,
        ck.type_param_defs,
        enum_id,
        &type_params,
        ErrorReporting::Yes(ck.file_id, expr_span),
    );

    let interned_name = ck.sa.interner.intern(&name);

    if let Some(&value) = enum_.name_to_value.get(&interned_name) {
        let variant = &enum_.variants[value as usize];

        if !variant.types.is_empty() {
            let enum_name = ck.sa.interner.str(enum_.name).to_string();
            let variant_name = ck.sa.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name_enum(ck.sa, &*enum_))
                .collect::<Vec<_>>();
            let arg_types = Vec::new();
            let msg = ErrorMessage::EnumArgsIncompatible(
                enum_name,
                variant_name,
                variant_types,
                arg_types,
            );
            ck.sa.report(ck.file_id, expr_span, msg);
        }

        ck.analysis.map_idents.insert(
            expr_id,
            IdentType::EnumValue(enum_id, type_params.clone(), value),
        );
    } else {
        ck.sa.report(
            ck.file_id,
            expr_span,
            ErrorMessage::UnknownEnumVariant(name),
        );
    }

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.analysis.set_ty(expr_id, ty.clone());
        ty
    } else {
        ck.analysis.set_ty(expr_id, SourceType::Error);
        SourceType::Error
    }
}

pub(super) fn check_expr_type_param(
    ck: &mut TypeCheck,
    e: &ast::ExprTypeParamType,
    expected_ty: SourceType,
) -> SourceType {
    let type_params: Vec<SourceType> = e.args.iter().map(|p| ck.read_type(p)).collect();
    let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

    if let Some(ident) = e.callee.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &ident.name);

        match sym {
            Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => {
                check_enum_value_without_args_id(
                    ck,
                    e.id,
                    e.op_span,
                    expected_ty,
                    enum_id,
                    type_params,
                    variant_idx,
                )
            }

            _ => {
                ck.sa
                    .report(ck.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);

                ck.analysis.set_ty(e.id, SourceType::Error);
                SourceType::Error
            }
        }
    } else if let Some(path) = e.callee.to_path() {
        let container_name = if let Some(container_expr) = path.lhs.to_ident() {
            container_expr.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, path.lhs.span(), msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        };

        let method_name = if let Some(ident) = path.rhs.to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, path.rhs.span(), msg);

            ck.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        };

        let sym = ck.symtable.get_string(ck.sa, &container_name);

        match sym {
            Some(SymbolKind::Enum(enum_id)) => check_enum_value_without_args(
                ck,
                e.id,
                e.op_span,
                expected_ty,
                enum_id,
                type_params,
                method_name,
            ),

            _ => {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, e.op_span, msg);

                ck.analysis.set_ty(e.id, SourceType::Error);
                SourceType::Error
            }
        }
    } else {
        ck.sa
            .report(ck.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);
        ck.analysis.set_ty(e.id, SourceType::Error);
        return SourceType::Error;
    }
}

pub(super) fn check_enum_value_without_args_id(
    ck: &mut TypeCheck,
    expr_id: ast::NodeId,
    expr_span: Span,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
) -> SourceType {
    let enum_ = ck.sa.enums[enum_id].read();

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible(enum_.name(ck.sa));
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let type_params_ok = typeparamck::check_enum(
        ck.sa,
        ck.type_param_defs,
        enum_id,
        &type_params,
        ErrorReporting::Yes(ck.file_id, expr_span),
    );

    let variant = &enum_.variants[variant_idx as usize];

    if !variant.types.is_empty() {
        let enum_name = ck.sa.interner.str(enum_.name).to_string();
        let variant_name = ck.sa.interner.str(variant.name).to_string();
        let variant_types = variant
            .types
            .iter()
            .map(|a| a.name_enum(ck.sa, &*enum_))
            .collect::<Vec<_>>();
        let arg_types = Vec::new();
        let msg =
            ErrorMessage::EnumArgsIncompatible(enum_name, variant_name, variant_types, arg_types);
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    ck.analysis.map_idents.insert(
        expr_id,
        IdentType::EnumValue(enum_id, type_params.clone(), variant_idx),
    );

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.analysis.set_ty(expr_id, ty.clone());
        ty
    } else {
        ck.analysis.set_ty(expr_id, SourceType::Error);
        SourceType::Error
    }
}

fn check_expr_path_module(
    ck: &mut TypeCheck,
    e: &ast::ExprPathType,
    expected_ty: SourceType,
    module_id: ModuleDefinitionId,
    element_name: String,
) -> SourceType {
    let module = &ck.sa.modules.idx(module_id);
    let module = module.read();
    let table = module.table.read();

    let interned_element_name = ck.sa.interner.intern(&element_name);

    let sym = table.get(interned_element_name);

    match sym {
        Some(SymbolKind::Global(global_id)) => {
            if !global_accessible_from(ck.sa, global_id, ck.module_id) {
                let global = &ck.sa.globals.idx(global_id);
                let global = global.read();
                let msg = ErrorMessage::NotAccessible(global.name(ck.sa));
                ck.sa.report(ck.file_id, e.op_span, msg);
            }

            let global_var = ck.sa.globals.idx(global_id);
            let ty = global_var.read().ty.clone();
            ck.analysis.set_ty(e.id, ty.clone());

            ck.analysis
                .map_idents
                .insert(e.id, IdentType::Global(global_id));

            ty
        }

        Some(SymbolKind::Const(const_id)) => {
            if !const_accessible_from(ck.sa, const_id, ck.module_id) {
                let const_ = &ck.sa.consts[const_id];
                let msg = ErrorMessage::NotAccessible(const_.name(ck.sa));
                ck.sa.report(ck.file_id, e.op_span, msg);
            }

            let const_ = &ck.sa.consts[const_id];
            ck.analysis.set_ty(e.id, const_.ty());

            ck.analysis
                .map_idents
                .insert(e.id, IdentType::Const(const_id));

            const_.ty()
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_value_without_args_id(
            ck,
            e.id,
            e.op_span,
            expected_ty,
            enum_id,
            SourceTypeArray::empty(),
            variant_idx,
        ),

        None => {
            let module = module.name(ck.sa);
            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::UnknownIdentifierInModule(module, element_name),
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

pub(super) fn check_type(
    ck: &mut TypeCheck,
    e: &ast::ExprBinType,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
    expected_type: SourceType,
) {
    if !expected_type.allows(ck.sa, lhs_type.clone())
        || !expected_type.allows(ck.sa, rhs_type.clone())
    {
        let op = op.as_str().into();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

        ck.sa.report(ck.file_id, e.span, msg);
    }
}

pub(super) fn read_path(ck: &mut TypeCheck, path: &ast::PathData) -> Result<SymbolKind, ()> {
    let names = &path.names;
    let mut sym = ck.symtable.get_string(ck.sa, &names[0].name_as_string);

    for ident in &names[1..] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                if !module_accessible_from(ck.sa, module_id, ck.module_id) {
                    let module = &ck.sa.modules[module_id].read();
                    let msg = ErrorMessage::NotAccessible(module.name(ck.sa));
                    ck.sa.report(ck.file_id, path.span, msg);
                }

                let iname = ck.sa.interner.intern(&ident.name_as_string);

                let module = &ck.sa.modules[module_id].read();
                let symtable = module.table.read();
                sym = symtable.get(iname);
            }

            Some(SymbolKind::Enum(enum_id)) => {
                let enum_ = ck.sa.enums[enum_id].read();

                if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible(enum_.name(ck.sa));
                    ck.sa.report(ck.file_id, path.span, msg);
                }

                let iname = ck.sa.interner.intern(&ident.name_as_string);

                if let Some(&variant_idx) = enum_.name_to_value.get(&iname) {
                    sym = Some(SymbolKind::EnumVariant(enum_id, variant_idx));
                } else {
                    let name = ident.name_as_string.clone();
                    ck.sa.report(
                        ck.file_id.into(),
                        path.span,
                        ErrorMessage::UnknownEnumVariant(name),
                    );
                    return Err(());
                }
            }

            Some(_) => {
                let msg = ErrorMessage::ExpectedModule;
                ck.sa.report(ck.file_id, path.span, msg);
                return Err(());
            }

            None => {
                let name = names[0].name_as_string.clone();
                let msg = ErrorMessage::UnknownIdentifier(name);
                ck.sa.report(ck.file_id, path.span, msg);
                return Err(());
            }
        }
    }

    if let Some(sym) = sym {
        Ok(sym)
    } else {
        let name = names[0].name_as_string.clone();
        let msg = ErrorMessage::UnknownIdentifier(name);
        ck.sa.report(ck.file_id, path.span, msg);

        Err(())
    }
}
