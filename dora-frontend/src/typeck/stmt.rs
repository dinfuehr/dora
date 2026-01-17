use crate::args;
use crate::error::diagnostics::{
    ASSIGN_TYPE, LET_MISSING_INITIALIZATION, VAR_NEEDS_TYPE_OR_EXPRESSION,
};
use crate::sema::{LetStmt, Stmt, StmtId};
use crate::ty::SourceType;
use crate::typeck::{TypeCheck, check_expr, check_pattern};

pub(super) fn check_stmt(ck: &mut TypeCheck, stmt_id: StmtId) {
    let stmt = ck.body.stmt(stmt_id);

    match stmt {
        Stmt::Let(let_stmt) => check_stmt_let(ck, stmt_id, let_stmt),
        Stmt::Expr(expr_id) => {
            check_expr(ck, *expr_id, SourceType::Any);
        }
        Stmt::Error => {}
    }
}

fn check_stmt_let(ck: &mut TypeCheck, stmt_id: StmtId, stmt: &LetStmt) {
    let defined_type = if let Some(type_ref_id) = stmt.data_type {
        ck.read_type_id(type_ref_id)
    } else {
        SourceType::Any
    };

    let expr_type = stmt
        .expr
        .map(|expr_id| check_expr(ck, expr_id, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if stmt.data_type.is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.report_stmt_id(stmt_id, &VAR_NEEDS_TYPE_OR_EXPRESSION, args!());
        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_pattern(ck, stmt.pattern, defined_type.clone());

    if stmt.expr.is_some() {
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
