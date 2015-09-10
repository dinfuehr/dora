use parser::ast::ctxt::Context;
use error::msg::Msg;

use parser::ast::*;
use parser::ast::Stmt::*;
use parser::ast::visit::*;
use parser::lexer::position::Position;

use sym::*;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    ReturnCheck::new(ctxt).visit_ast(ast);
}

struct ReturnCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
}

impl<'a, 'ast> ReturnCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> ReturnCheck<'a, 'ast> {
        ReturnCheck {
            ctxt: ctxt,
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for ReturnCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let return_type = self.ctxt.function(f.id, |fct| fct.return_type);

        if return_type != BuiltinType::Unit {
            if let Err(pos) = returns_value(&f.block) {
                self.ctxt.diag.borrow_mut().report(pos, Msg::NoReturnValue);
            }
        }
    }
}

fn returns_value(s: &Stmt) -> Result<(), Position> {
    match *s {
        StmtReturn(_) => Ok(()),
        StmtBlock(ref stmt) => block_returns_value(stmt),
        StmtIf(ref stmt) => if_returns_value(stmt),
        StmtLoop(ref stmt) => returns_value(&stmt.block),

        StmtWhile(ref stmt) => Err(stmt.pos),
        StmtBreak(ref stmt) => Err(stmt.pos),
        StmtContinue(ref stmt) => Err(stmt.pos),
        StmtVar(ref stmt) => Err(stmt.pos),
        StmtExpr(ref stmt) => Err(stmt.pos),
    }
}

fn if_returns_value(s: &StmtIfType) -> Result<(), Position> {
    try!(returns_value(&s.then_block));

    match s.else_block {
        Some(ref block) => returns_value(block),
        None => Err(s.pos)
    }
}

fn block_returns_value(s: &StmtBlockType) -> Result<(), Position> {
    let mut pos = s.pos;

    for stmt in &s.stmts {
        match returns_value(stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => pos = err_pos
        }
    }

    Err(pos)
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn returns_unit() {
        ok("fn f() {}");
        ok("fn f() { if true { return; } }");
        ok("fn f() { while true { return; } }");
        ok("fn f() { loop { return; } }");
    }

    #[test]
    fn returns_int() {
        err("fn f() -> int { }", pos(1, 15), Msg::NoReturnValue);
        err("fn f() -> int { if true { return 1; } }", pos(1, 17), Msg::NoReturnValue);
        err("fn f() -> int { if true { } else { return 1; } }", pos(1, 25), Msg::NoReturnValue);
        err("fn f() -> int { while true { return 1; } }", pos(1, 17), Msg::NoReturnValue);
        ok("fn f() -> int { loop { return 1; } }");
        ok("fn f() -> int { if true { return 1; } else { return 2; } }");
        ok("fn f() -> int { return 1; 1+2; }");
    }
}
