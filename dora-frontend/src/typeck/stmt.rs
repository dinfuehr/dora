use dora_parser::ast;
use dora_parser::ast::SyntaxNodeBase;

use crate::error::msg::ErrorMessage;
use crate::ty::SourceType;
use crate::typeck::{TypeCheck, check_expr, check_pattern};

pub(super) fn check_stmt(ck: &mut TypeCheck, node: ast::AstStmt) {
    match node {
        ast::AstStmt::Let(stmt) => check_stmt_let(ck, stmt),

        ast::AstStmt::ExprStmt(stmt) => {
            check_expr(ck, stmt.expr(), SourceType::Any);
        }

        ast::AstStmt::Error(_) => {}
    }
}

fn check_stmt_let(ck: &mut TypeCheck, s: ast::AstLet) {
    let defined_type = if let Some(data_type) = s.data_type() {
        ck.read_type(ck.file_id, data_type)
    } else {
        SourceType::Any
    };

    let expr_type = s
        .expr()
        .map(|expr| check_expr(ck, expr, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if s.data_type().is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.sa
            .report(ck.file_id, s.span(), ErrorMessage::VarNeedsTypeOrExpression);

        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    let pattern = s.pattern();
    check_pattern(ck, pattern, defined_type.clone());

    if s.expr().is_some() {
        if !expr_type.is_error()
            && !defined_type.is_error()
            && !defined_type.allows(ck.sa, expr_type.clone())
        {
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(defined_type, expr_type);
            ck.report(s.span(), msg);
        }

    // let variable binding needs to be assigned
    } else {
        ck.sa
            .report(ck.file_id, s.span(), ErrorMessage::LetMissingInitialization);
    }
}
