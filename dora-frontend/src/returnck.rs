use dora_parser::Span;
use dora_parser::ast::*;
use dora_parser::ast::{self, AstExpr};

pub fn returns_value(f: &File, s: ast::AstStmt) -> Result<(), Span> {
    match s {
        AstStmt::Let(stmt) => Err(stmt.span()),
        AstStmt::ExprStmt(stmt) => expr_returns_value(f, stmt.expr()),
        AstStmt::ErrorStmt(_) => Ok(()),
    }
}

pub fn expr_returns_value(f: &File, expr: ast::AstExpr) -> Result<(), Span> {
    match expr {
        AstExpr::Block(block) => expr_block_returns_value(f, block),
        AstExpr::If(expr) => expr_if_returns_value(f, expr),
        AstExpr::For(expr) => Err(expr.span()),
        AstExpr::While(expr) => Err(expr.span()),
        AstExpr::Break(stmt) => Err(stmt.span()),
        AstExpr::Continue(stmt) => Err(stmt.span()),
        AstExpr::Return(..) => Ok(()),
        _ => Err(expr.span()),
    }
}

pub fn expr_block_returns_value(f: &File, e: ast::AstBlock) -> Result<(), Span> {
    let mut span = e.span();

    for stmt in e.stmts() {
        match returns_value(f, stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => span = err_pos,
        }
    }

    if let Some(expr) = e.expr() {
        expr_returns_value(f, expr)
    } else {
        Err(span)
    }
}

fn expr_if_returns_value(f: &File, e: ast::AstIf) -> Result<(), Span> {
    expr_returns_value(f, e.then_block())?;

    match e.else_block() {
        Some(block) => expr_returns_value(f, block),
        None => Err(e.span()),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
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
            ErrorMessage::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fn f(): Int32 { if true { return 1; } }",
            (1, 15),
            ErrorMessage::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fn f(): Int32 { if true { } else { return 1; } }",
            (1, 15),
            ErrorMessage::ReturnType("Int32".into(), "()".into()),
        );
        err(
            "fn f(): Int32 { while true { return 1; } }",
            (1, 15),
            ErrorMessage::ReturnType("Int32".into(), "()".into()),
        );
        ok("fn f(): Int32 { if true { return 1; } else { return 2; } }");
        ok("fn f(): Int32 { return 1; 1+2; }");
    }
}
