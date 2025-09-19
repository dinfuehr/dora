use dora_parser::ast::*;
use dora_parser::Span;

pub fn returns_value(f: &File, s: &Ast) -> Result<(), Span> {
    match *s {
        Ast::LetStmt(ref stmt) => Err(stmt.span),
        Ast::ExprStmt(ref stmt) => expr_returns_value(f, &stmt.expr),
        _ => unreachable!(),
    }
}

pub fn expr_returns_value(f: &File, e: &ExprData) -> Result<(), Span> {
    match *e {
        ExprData::Block(ref block) => expr_block_returns_value(f, block),
        ExprData::If(ref expr) => expr_if_returns_value(f, expr),
        ExprData::For(ref expr) => Err(expr.span),
        ExprData::While(ref expr) => Err(expr.span),
        ExprData::Break(ref stmt) => Err(stmt.span),
        ExprData::Continue(ref stmt) => Err(stmt.span),
        ExprData::Return(..) => Ok(()),
        _ => Err(e.span()),
    }
}

pub fn expr_block_returns_value(f: &File, e: &ExprBlockType) -> Result<(), Span> {
    let mut span = e.span;

    for &stmt_id in &e.stmts {
        let stmt = f.node(stmt_id);
        match returns_value(f, stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => span = err_pos,
        }
    }

    if let Some(ref expr) = e.expr {
        expr_returns_value(f, expr)
    } else {
        Err(span)
    }
}

fn expr_if_returns_value(f: &File, e: &ExprIfType) -> Result<(), Span> {
    expr_returns_value(f, &e.then_block)?;

    match e.else_block {
        Some(ref block) => expr_returns_value(f, block),
        None => Err(e.span),
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
