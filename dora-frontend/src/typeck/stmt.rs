use dora_parser::ast;
use dora_parser::ast::SyntaxNodeBase;

use crate::args;
use crate::error::diagnostics::{
    ASSIGN_TYPE, LET_MISSING_INITIALIZATION, VAR_NEEDS_TYPE_OR_EXPRESSION,
};
use crate::sema::StmtId;
use crate::ty::SourceType;
use crate::typeck::{TypeCheck, check_expr, check_expr_id, check_pattern};

pub(super) fn check_stmt_id(ck: &mut TypeCheck, id: StmtId) {
    let stmt = ck.syntax_by_stmt_id::<ast::AstStmt>(id);
    check_stmt(ck, stmt);
}

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
    let stmt_id = ck.body.to_stmt_id(s.id());
    let stmt = ck.body.stmt(stmt_id).as_let();

    let defined_type = if let Some(data_type) = s.data_type() {
        ck.read_type(data_type)
    } else {
        SourceType::Any
    };

    let expr_type = stmt
        .expr
        .map(|expr_id| check_expr_id(ck, expr_id, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if s.data_type().is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.report_stmt_id(stmt_id, &VAR_NEEDS_TYPE_OR_EXPRESSION, args!());
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
            ck.report_stmt_id(stmt_id, &ASSIGN_TYPE, args!(defined_type, expr_type));
        }

    // let variable binding needs to be assigned
    } else {
        ck.report_stmt_id(stmt_id, &LET_MISSING_INITIALIZATION, args!());
    }
}
