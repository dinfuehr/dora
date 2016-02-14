use ctxt::{Context, Fct};
use error::msg::Msg;

use ast::*;
use ast::Stmt::*;
use ast::visit::*;
use lexer::position::Position;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if fct.kind.is_src() {
            let ast = fct.ast();
            let mut returnck = ReturnCheck {
                ctxt: ctxt,
                fct: &mut fct,
                ast: ast,
            };

            returnck.check();
        }
    }
}

struct ReturnCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
}

impl<'a, 'ast> ReturnCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }
}

impl<'a, 'ast> Visitor<'ast> for ReturnCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let returns = returns_value(&f.block);

        if let Err(pos) = returns {
            let return_type = self.fct.return_type;

            // only report error for functions that do not just return ()
            if return_type != BuiltinType::Unit {
                self.ctxt.diag.borrow_mut().report(pos, Msg::NoReturnValue);
            }

        } else {
            // otherwise the function is always finished with a return statement
            // save this information for the function, this information is useful
            // for code generation

            self.fct.src_mut().always_returns = true;
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

            assert_eq!(value, ctxt.fct_by_id(fct_id, |fct| fct.src().always_returns));
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
