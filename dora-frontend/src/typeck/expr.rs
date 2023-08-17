use std::sync::Arc;

use dora_parser::ast;
use fixedbitset::FixedBitSet;

use std::collections::HashMap;

use crate::access::{class_field_accessible_from, struct_field_accessible_from};
use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{
    create_tuple, find_field_in_class, find_impl, implements_trait, CallType, EnumDefinitionId,
    IdentType, NestedVarId,
};
use crate::specialize::replace_type_param;
use crate::sym::SymbolKind;
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeck::{
    check_expr_call, check_expr_for, check_expr_if, check_expr_return, check_expr_while,
    check_lit_char, check_lit_float, check_lit_int, check_lit_str, TypeCheck,
};

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
        ast::ExprData::Un(ref expr) => ck.check_expr_un(expr, expected_ty),
        ast::ExprData::Bin(ref expr) => ck.check_expr_bin(expr, expected_ty),
        ast::ExprData::Call(ref expr) => check_expr_call(ck, expr, expected_ty),
        ast::ExprData::TypeParam(ref expr) => ck.check_expr_type_param(expr, expected_ty),
        ast::ExprData::Path(ref expr) => ck.check_expr_path(expr, expected_ty),
        ast::ExprData::Dot(ref expr) => check_expr_dot(ck, expr, expected_ty),
        ast::ExprData::This(ref expr) => check_expr_this(ck, expr, expected_ty),
        ast::ExprData::Conv(ref expr) => check_expr_conv(ck, expr, expected_ty),
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
