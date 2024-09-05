use dora_parser::ast;

use crate::error::msg::ErrorMessage;
use crate::ty::SourceType;
use crate::typeck::{add_local, check_expr, TypeCheck};

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
            let name = ck.sa.interner.intern(&ident.name.name_as_string);
            let var_id = ck.vars.add_var(name, ty, ident.mutable);

            add_local(ck.sa, ck.symtable, ck.vars, var_id, ck.file_id, ident.span);
            ck.analysis
                .map_vars
                .insert(ident.id, ck.vars.local_var_id(var_id));
        }

        ast::Pattern::LitBool(ref p) => {
            if !ty.is_bool() && !ty.is_error() {
                let ty_name = ck.ty_name(&ty);
                ck.sa.report(
                    ck.file_id,
                    p.span,
                    ErrorMessage::LetPatternExpectedType("Bool".into(), ty_name),
                );
            }
        }

        ast::Pattern::Underscore(_) => {
            // nothing to do
        }

        ast::Pattern::ClassOrStructOrEnum(..) => unreachable!(),

        ast::Pattern::Tuple(ref tuple) => {
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
                if tuple.params.len() != 0 {
                    ck.sa
                        .report(ck.file_id, tuple.span, ErrorMessage::LetPatternShouldBeUnit);
                }
                return;
            }

            if ty.is_error() {
                for param in &tuple.params {
                    check_pattern(ck, param, SourceType::Error);
                }
                return;
            }

            let subtypes = ty.tuple_subtypes();

            if subtypes.len() != tuple.params.len() {
                let ty_name = ck.ty_name(&ty);
                ck.sa.report(
                    ck.file_id,
                    tuple.span,
                    ErrorMessage::LetPatternExpectedTupleWithLength(
                        ty_name,
                        subtypes.len(),
                        tuple.params.len(),
                    ),
                );
                return;
            }

            for (param, subtype) in tuple.params.iter().zip(subtypes.iter()) {
                check_pattern(ck, param.as_ref(), subtype.clone());
            }
        }
    }
}
