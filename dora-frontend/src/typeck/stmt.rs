use crate::args;
use crate::error::diagnostics::{ASSIGN_TYPE, LET_ELSE_NOT_DIVERGING, LET_MISSING_INITIALIZATION};
use crate::expr_always_exits;
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
    let defined_type = stmt.data_type.map(|type_ref_id| ck.read_type(type_ref_id));

    let expr_type = if let Some(expr_id) = stmt.expr {
        check_expr(ck, expr_id, defined_type.clone().unwrap_or(SourceType::Any))
    } else {
        ck.report_stmt_id(stmt_id, &LET_MISSING_INITIALIZATION, args!());
        SourceType::Error
    };

    let defined_type = defined_type.unwrap_or(expr_type.clone());

    if let Some(else_expr) = stmt.else_expr {
        check_expr(ck, else_expr, SourceType::Any);

        if !expr_always_exits(ck.sa, ck.body, else_expr) {
            ck.report(ck.expr_span(else_expr), &LET_ELSE_NOT_DIVERGING, args!());
        }
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_pattern(ck, stmt.pattern, defined_type.clone());

    if !defined_type.allows(ck.sa, expr_type.clone()) {
        let defined_type = ck.ty_name(&defined_type);
        let expr_type = ck.ty_name(&expr_type);
        ck.report_stmt_id(stmt_id, &ASSIGN_TYPE, args!(defined_type, expr_type));
    }
}
