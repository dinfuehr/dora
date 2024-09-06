use dora_parser::ast;

use crate::access::enum_accessible_from;
use crate::error::msg::ErrorMessage;
use crate::sema::{EnumDefinitionId, IdentType};
use crate::ty::SourceType;
use crate::typeck::{add_local, check_expr, class_or_struct_or_enum_params, read_path, TypeCheck};
use crate::{specialize_type, SourceTypeArray, SymbolKind};

pub(super) fn check_stmt(ck: &mut TypeCheck, s: &ast::StmtData) {
    match *s {
        ast::StmtData::Let(ref stmt) => check_stmt_let(ck, stmt),

        ast::StmtData::Expr(ref stmt) => {
            check_expr(ck, &stmt.expr, SourceType::Any);
        }
    }
}

fn check_stmt_let(ck: &mut TypeCheck, s: &ast::StmtLetType) {
    let defined_type = if let Some(ref data_type) = s.data_type {
        ck.read_type(data_type)
    } else {
        SourceType::Any
    };

    let expr_type = s
        .expr
        .as_ref()
        .map(|expr| check_expr(ck, &expr, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if s.data_type.is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::VarNeedsTypeOrExpression);

        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_pattern(ck, &s.pattern, defined_type.clone());

    if s.expr.is_some() {
        if !expr_type.is_error()
            && !defined_type.is_error()
            && !defined_type.allows(ck.sa, expr_type.clone())
        {
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(defined_type, expr_type);
            ck.sa.report(ck.file_id, s.span, msg);
        }

    // let variable binding needs to be assigned
    } else {
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::LetMissingInitialization);
    }
}

pub(super) fn check_pattern(ck: &mut TypeCheck, pattern: &ast::Pattern, ty: SourceType) {
    match pattern {
        ast::Pattern::Ident(ref ident) => {
            let sym = ck.symtable.get_string(ck.sa, &ident.name.name_as_string);

            match sym {
                Some(SymbolKind::EnumVariant(enum_id, variant_id)) => {
                    check_pattern_enum(ck, pattern, ty, enum_id, variant_id);
                }

                _ => {
                    let name = ck.sa.interner.intern(&ident.name.name_as_string);
                    let var_id = ck.vars.add_var(name, ty, ident.mutable);

                    add_local(ck.sa, ck.symtable, ck.vars, var_id, ck.file_id, ident.span);
                    ck.analysis
                        .map_idents
                        .insert(ident.id, IdentType::Var(ck.vars.local_var_id(var_id)));
                }
            }
        }

        ast::Pattern::LitBool(ref p) => {
            if !ty.is_bool() && !ty.is_error() {
                let ty_name = ck.ty_name(&ty);
                ck.sa.report(
                    ck.file_id,
                    p.span,
                    ErrorMessage::WrongType("Bool".into(), ty_name),
                );
            }
        }

        ast::Pattern::Underscore(_) => {
            // nothing to do
        }

        ast::Pattern::ClassOrStructOrEnum(ref p) => {
            let sym = read_path(ck, &p.path);

            match sym {
                Ok(SymbolKind::EnumVariant(enum_id, variant_id)) => {
                    check_pattern_enum(ck, pattern, ty, enum_id, variant_id);
                }

                Ok(..) => {
                    let msg = ErrorMessage::EnumVariantExpected;
                    ck.sa.report(ck.file_id, p.path.span, msg);
                }

                Err(..) => {}
            }
        }

        ast::Pattern::Tuple(ref tuple) => {
            check_pattern_tuple(ck, tuple, ty);
        }
    }
}

fn check_pattern_enum(
    ck: &mut TypeCheck,
    pattern: &ast::Pattern,
    ty: SourceType,
    enum_id: EnumDefinitionId,
    variant_id: u32,
) {
    let enum_ = ck.sa.enum_(enum_id);
    let variant = &enum_.variants[variant_id as usize];

    let params = class_or_struct_or_enum_params(pattern);
    let given_params = params.as_ref().map(|p| p.len()).unwrap_or(0);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible(enum_.name(ck.sa));
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(enum_id) == ty.enum_id() {
        let value_type_params = ty.type_params();

        ck.analysis.map_idents.insert(
            pattern.id(),
            IdentType::EnumVariant(enum_id, value_type_params.clone(), variant_id),
        );

        let expected_params = variant.types().len();

        if given_params != expected_params {
            let msg = ErrorMessage::PatternWrongNumberOfParams(given_params, expected_params);
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        if let Some(ref params) = params {
            for (idx, param) in params.iter().enumerate() {
                let param_ty = variant
                    .types()
                    .get(idx)
                    .cloned()
                    .unwrap_or(SourceType::Error);
                let param_ty = specialize_type(ck.sa, param_ty, &value_type_params);
                check_pattern(ck, param.as_ref(), param_ty);
            }
        }
    } else if !ty.is_error() {
        let ty = ty.name(ck.sa);
        let msg = ErrorMessage::PatternTypeMismatch(ty);
        ck.sa.report(ck.file_id, pattern.span(), msg);

        if let Some(params) = params {
            for param in params.iter() {
                let param_ty = SourceType::Error;
                check_pattern(ck, param.as_ref(), param_ty);
            }
        }
    }
}

fn check_pattern_tuple(ck: &mut TypeCheck, pattern: &ast::PatternTuple, ty: SourceType) {
    if !ty.is_tuple_or_unit() {
        if !ty.is_error() {
            let ty_name = ck.ty_name(&ty);
            ck.sa.report(
                ck.file_id,
                pattern.span,
                ErrorMessage::PatternTupleExpected(ty_name),
            );
        }

        for param in &pattern.params {
            check_pattern(ck, param, SourceType::Error);
        }

        return;
    }

    let subtypes = if ty.is_unit() {
        SourceTypeArray::empty()
    } else {
        ty.tuple_subtypes()
    };

    if subtypes.len() != pattern.params.len() {
        let ty_name = ck.ty_name(&ty);
        ck.sa.report(
            ck.file_id,
            pattern.span,
            ErrorMessage::PatternTupleLengthMismatch(ty_name, subtypes.len(), pattern.params.len()),
        );
    }

    for (idx, param) in pattern.params.iter().enumerate() {
        let subty = subtypes
            .types()
            .get(idx)
            .cloned()
            .unwrap_or(SourceType::Error);
        check_pattern(ck, param.as_ref(), subty.clone());
    }
}
