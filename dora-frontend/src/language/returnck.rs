use dora_parser::ast::*;
use dora_parser::Span;

pub fn returns_value(s: &Stmt) -> Result<(), Span> {
    match *s {
        Stmt::Return(_) => Ok(()),
        Stmt::For(ref stmt) => Err(stmt.span),
        Stmt::While(ref stmt) => Err(stmt.span),
        Stmt::Break(ref stmt) => Err(stmt.span),
        Stmt::Continue(ref stmt) => Err(stmt.span),
        Stmt::Let(ref stmt) => Err(stmt.span),
        Stmt::Expr(ref stmt) => expr_returns_value(&stmt.expr),
    }
}

pub fn expr_returns_value(e: &Expr) -> Result<(), Span> {
    match *e {
        Expr::Block(ref block) => expr_block_returns_value(block),
        Expr::If(ref expr) => expr_if_returns_value(expr),
        _ => Err(e.span()),
    }
}

pub fn expr_block_returns_value(e: &ExprBlockType) -> Result<(), Span> {
    let mut span = e.span;

    for stmt in &e.stmts {
        match returns_value(stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => span = err_pos,
        }
    }

    if let Some(ref expr) = e.expr {
        expr_returns_value(expr)
    } else {
        Err(span)
    }
}

fn expr_if_returns_value(e: &ExprIfType) -> Result<(), Span> {
    expr_returns_value(&e.then_block)?;

    match e.else_block {
        Some(ref block) => expr_returns_value(block),
        None => Err(e.span),
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

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
