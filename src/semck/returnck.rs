use ast::ctxt::Context;
use error::msg::Msg;

use ast::*;
use ast::Stmt::*;
use ast::visit::*;
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
        let returns = returns_value(&f.block);

        if let Err(pos) = returns {
            let return_type = self.ctxt.function(f.id, |fct| fct.return_type);

            // only report error for functions that do not just return ()
            if return_type != BuiltinType::Unit {
                self.ctxt.diag.borrow_mut().report(pos, Msg::NoReturnValue);
            }

        } else {
            // otherwise the function is always finished with a return statement
            // save this information for the function, this information is useful
            // for code generation

            self.ctxt.function(f.id, |fct| fct.always_returns = true);
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
    use test::parse;

    fn test_always_returns(code: &'static str, value: bool) {
        parse(code, |ctxt| {
            let name = ctxt.interner.intern("f");
            let fct_id = ctxt.sym.borrow().get_function(name).unwrap();

            let fct_infos = ctxt.fct_infos.borrow();
            let fct = &fct_infos[fct_id.0];

            assert_eq!(value, fct.always_returns);
        });
    }

    #[test]
    fn returns_unit() {
        ok("fn f() {}");
        ok("fn f() { if true { return; } }");
        ok("fn f() { while true { return; } }");
        ok("fn f() { loop { return; } }");
    }

    #[test]
    fn always_returns() {
        test_always_returns("fn f() {}", false);
        test_always_returns("fn f() { return; }", true);
        test_always_returns("fn f() -> int { return 1; }", true);
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
