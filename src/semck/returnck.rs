use ctxt::{Fct, FctSrc, SemContext};
use dora_parser::error::msg::Msg;

use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &SemContext<'ast>) {
    for fct in ctxt.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();
        let ast = fct.ast;

        let mut returnck = ReturnCheck {
            ctxt: ctxt,
            fct: &fct,
            src: &mut src,
            ast: ast,
        };

        returnck.check();
    }
}

struct ReturnCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
}

impl<'a, 'ast> ReturnCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }
}

impl<'a, 'ast> Visitor<'ast> for ReturnCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let returns = returns_value(f.block());

        if let Err(pos) = returns {
            let return_type = self.fct.return_type;

            // only report error for functions that do not just return ()
            if return_type != BuiltinType::Unit {
                self.ctxt.diag.lock().report_without_path(pos, Msg::NoReturnValue);
            }
        } else {
            // otherwise the function is always finished with a return statement
            // save this information for the function, this information is useful
            // for code generation

            self.src.always_returns = true;
        }
    }
}

pub fn returns_value(s: &Stmt) -> Result<(), Position> {
    match *s {
        StmtReturn(_) => Ok(()),
        StmtBlock(ref stmt) => block_returns_value(stmt),
        StmtIf(ref stmt) => if_returns_value(stmt),
        StmtLoop(ref stmt) => returns_value(&stmt.block),
        StmtFor(ref stmt) => Err(stmt.pos),
        StmtWhile(ref stmt) => Err(stmt.pos),
        StmtBreak(ref stmt) => Err(stmt.pos),
        StmtContinue(ref stmt) => Err(stmt.pos),
        StmtVar(ref stmt) => Err(stmt.pos),
        StmtExpr(ref stmt) => Err(stmt.pos),
        StmtSpawn(ref stmt) => Err(stmt.pos),
        StmtThrow(_) => Ok(()),
        StmtDefer(ref stmt) => Err(stmt.pos),
        StmtDo(ref stmt) => do_returns_value(stmt),
    }
}

fn if_returns_value(s: &StmtIfType) -> Result<(), Position> {
    try!(returns_value(&s.then_block));

    match s.else_block {
        Some(ref block) => returns_value(block),
        None => Err(s.pos),
    }
}

fn block_returns_value(s: &StmtBlockType) -> Result<(), Position> {
    let mut pos = s.pos;

    for stmt in &s.stmts {
        match returns_value(stmt) {
            Ok(_) => return Ok(()),
            Err(err_pos) => pos = err_pos,
        }
    }

    Err(pos)
}

fn do_returns_value(s: &StmtDoType) -> Result<(), Position> {
    // return in finally-block is good enough
    if let Some(ref finally_block) = s.finally_block {
        if returns_value(&finally_block.block).is_ok() {
            return Ok(());
        }
    }

    // if no finally block given or finally does not return,
    // do and all catch-blocks need to return
    try!(returns_value(&s.do_block));

    for catch in &s.catch_blocks {
        try!(returns_value(&catch.block));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;
    use test::parse;

    fn test_always_returns(code: &'static str, value: bool) {
        parse(code, |ctxt| {
            let name = ctxt.interner.intern("f");
            let fct_id = ctxt.sym.lock().get_fct(name).unwrap();

            let fct = ctxt.fcts.idx(fct_id);
            let fct = fct.read();
            let src = fct.src();
            let src = src.read();

            assert_eq!(value, src.always_returns);
        });
    }

    #[test]
    fn returns_unit() {
        ok("fun f() {}");
        ok("fun f() { if true { return; } }");
        ok("fun f() { while true { return; } }");
        ok("fun f() { loop { return; } }");
    }

    #[test]
    fn always_returns() {
        test_always_returns("fun f() {}", false);
        test_always_returns("fun f() { return; }", true);
        test_always_returns("fun f() -> Int { return 1; }", true);
        test_always_returns("fun f() -> Int { throw \"abc\"; }", true);
    }

    #[test]
    fn returns_int() {
        err("fun f() -> Int { }", pos(1, 16), Msg::NoReturnValue);
        err(
            "fun f() -> Int { if true { return 1; } }",
            pos(1, 18),
            Msg::NoReturnValue,
        );
        err(
            "fun f() -> Int { if true { } else { return 1; } }",
            pos(1, 26),
            Msg::NoReturnValue,
        );
        err(
            "fun f() -> Int { while true { return 1; } }",
            pos(1, 18),
            Msg::NoReturnValue,
        );
        ok("fun f() -> Int { loop { return 1; } }");
        ok("fun f() -> Int { if true { return 1; } else { return 2; } }");
        ok("fun f() -> Int { return 1; 1+2; }");
        ok("fun f(x: Int) -> Int { if x == 0 { throw \"abc\"; } else { return -x; } }");
    }

    #[test]
    fn do_returns() {
        ok("fun f() -> Int { do { return 1; } catch x: String { return 2; } }");
        ok("fun f() -> Int { do { } catch x: String { return 2; } return 1; }");
        ok("fun f() -> Int { do { return 2; } catch x: String { } return 1; }");
        ok("fun f() -> Int { do { } catch x: String { } return 1; }");
        ok("fun f() -> Int { do { } catch x: String { } finally { return 1; } }");
        err(
            "fun f() -> Int { do { return 1; } catch x: String { } }",
            pos(1, 51),
            Msg::NoReturnValue,
        );
        err(
            "fun f() -> Int { do { } catch x: String { return 1; } }",
            pos(1, 21),
            Msg::NoReturnValue,
        );
    }
}
