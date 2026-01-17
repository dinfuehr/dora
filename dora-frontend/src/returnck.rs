use crate::sema::{BlockExpr, Body, Expr, ExprId, IfExpr, MatchExpr, Stmt, StmtId};

pub fn stmt_returns_value(body: &Body, stmt_id: StmtId) -> bool {
    let stmt = body.stmt(stmt_id);
    match stmt {
        Stmt::Let(_) => false,
        Stmt::Expr(expr_id) => expr_returns_value(body, *expr_id),
        Stmt::Error => true,
    }
}

pub fn expr_returns_value(body: &Body, expr_id: ExprId) -> bool {
    let expr = body.expr(expr_id);
    match expr {
        Expr::Block(e) => expr_block_returns_value(body, e),
        Expr::If(e) => expr_if_returns_value(body, e),
        Expr::Match(e) => expr_match_returns_value(body, e),
        Expr::Paren(expr_id) => expr_returns_value(body, *expr_id),
        Expr::For(_) => false,
        Expr::While(_) => false,
        Expr::Break => false,
        Expr::Continue => false,
        Expr::Return(_) => true,
        Expr::Error => true,
        _ => false,
    }
}

pub fn expr_block_returns_value(body: &Body, e: &BlockExpr) -> bool {
    for &stmt_id in &e.stmts {
        if stmt_returns_value(body, stmt_id) {
            return true;
        }
    }

    if let Some(tail_expr_id) = e.expr {
        expr_returns_value(body, tail_expr_id)
    } else {
        false
    }
}

fn expr_if_returns_value(body: &Body, e: &IfExpr) -> bool {
    if !expr_returns_value(body, e.then_expr) {
        return false;
    }

    match e.else_expr {
        Some(else_expr_id) => expr_returns_value(body, else_expr_id),
        None => false,
    }
}

fn expr_match_returns_value(body: &Body, e: &MatchExpr) -> bool {
    e.arms.iter().all(|arm| expr_returns_value(body, arm.value))
}

#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::RETURN_TYPE;
    use crate::tests::*;

    #[test]
    fn returns_unit() {
        ok("fn f() {}");
        ok("fn f() { if true { return; } }");
        ok("fn f() { while true { return; } }");
    }

    #[test]
    fn returns_int() {
        err(
            "fn f(): Int32 { }",
            (1, 15),
            3,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int32", "()"),
        );
        err(
            "fn f(): Int32 { if true { return 1; } }",
            (1, 15),
            25,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int32", "()"),
        );
        err(
            "fn f(): Int32 { if true { } else { return 1; } }",
            (1, 15),
            34,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int32", "()"),
        );
        err(
            "fn f(): Int32 { while true { return 1; } }",
            (1, 15),
            28,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int32", "()"),
        );
        ok("fn f(): Int32 { if true { return 1; } else { return 2; } }");
        ok("fn f(): Int32 { return 1; 1+2; }");
    }
}
