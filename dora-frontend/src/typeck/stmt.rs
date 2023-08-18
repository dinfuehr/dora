use dora_parser::ast;

use crate::error::msg::ErrorMessage;
use crate::ty::SourceType;
use crate::typeck::{check_expr, TypeCheck};

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
        let tyname = s.pattern.to_name().unwrap();
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::VarNeedsTypeInfo(tyname));

        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_let_pattern(ck, &s.pattern, defined_type.clone());

    if s.expr.is_some() {
        if !expr_type.is_error()
            && !defined_type.is_error()
            && !defined_type.allows(ck.sa, expr_type.clone())
        {
            let name = s.pattern.to_name().unwrap();
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(name, defined_type, expr_type);
            ck.sa.report(ck.file_id, s.span, msg);
        }

    // let variable binding needs to be assigned
    } else {
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::LetMissingInitialization);
    }
}

pub(super) fn check_let_pattern(ck: &mut TypeCheck, pattern: &ast::LetPattern, ty: SourceType) {
    match pattern {
        ast::LetPattern::Ident(ref ident) => {
            let name = ck
                .sa
                .interner
                .intern(&ident.name.as_ref().expect("missing name").name_as_string);
            let var_id = ck.vars.add_var(name, ty, ident.mutable);

            ck.add_local(var_id, ident.span);
            ck.analysis
                .map_vars
                .insert(ident.id, ck.vars.local_var_id(var_id));
        }

        ast::LetPattern::Underscore(_) => {
            // nothing to do
        }

        ast::LetPattern::Tuple(ref tuple) => {
            if !ty.is_tuple_or_unit() && !ty.is_error() {
                let ty_name = ck.ty_name(&ty);
                ck.sa.report(
                    ck.file_id,
                    tuple.span,
                    ErrorMessage::LetPatternExpectedTuple(ty_name),
                );
                return;
            }

            if ty.is_unit() {
                // () doesn't have any subparts
                if tuple.parts.len() != 0 {
                    ck.sa
                        .report(ck.file_id, tuple.span, ErrorMessage::LetPatternShouldBeUnit);
                }
                return;
            }

            if ty.is_error() {
                for part in &tuple.parts {
                    check_let_pattern(ck, part, SourceType::Error);
                }
                return;
            }

            let subtypes = ty.tuple_subtypes();

            if subtypes.len() != tuple.parts.len() {
                let ty_name = ck.ty_name(&ty);
                ck.sa.report(
                    ck.file_id,
                    tuple.span,
                    ErrorMessage::LetPatternExpectedTupleWithLength(
                        ty_name,
                        subtypes.len(),
                        tuple.parts.len(),
                    ),
                );
                return;
            }

            for (part, subtype) in tuple.parts.iter().zip(subtypes.iter()) {
                check_let_pattern(ck, &*part, subtype.clone());
            }
        }
    }
}
