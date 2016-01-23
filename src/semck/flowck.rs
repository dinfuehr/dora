use ctxt::Context;
use error::msg::Msg;

use ast::*;
use ast::Stmt::*;
use ast::visit::*;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    FlowCheck::new(ctxt).visit_ast(ast);
}

struct FlowCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    in_loop: bool,
}

impl<'a, 'ast> FlowCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> FlowCheck<'a, 'ast> {
        FlowCheck {
            ctxt: ctxt,
            in_loop: false
        }
    }

    fn handle_loop(&mut self, block: &'ast Stmt) {
        let old_in_loop = self.in_loop;

        self.in_loop = true;
        visit::walk_stmt(self, block);
        self.in_loop = old_in_loop;
    }

    fn handle_flow(&mut self, s: &'ast Stmt) {
        if !self.in_loop {
            self.ctxt.diag.borrow_mut().report(s.pos(), Msg::OutsideLoop);
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for FlowCheck<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtLoop(_) => self.handle_loop(s),
            StmtWhile(_) => self.handle_loop(s),
            StmtBreak(_) => self.handle_flow(s),
            StmtContinue(_) => self.handle_flow(s),

            _ => visit::walk_stmt(self, s)
        }
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;
    use ty::BuiltinType;

    #[test]
    fn flowck_break() {
        ok("fn a() { while true { break; } }");
        ok("fn a() { while true { if true { break; } } }");
        ok("fn a() { loop { break; } }");
        ok("fn a() { loop { if true { break; } } }");
        err("fn a() { break; }", pos(1, 10), Msg::OutsideLoop);
        err("fn a() { loop { } break; }", pos(1, 19), Msg::OutsideLoop);
        err("fn a() { while true { } break; }", pos(1, 25), Msg::OutsideLoop);
        err("fn a() { if true { } break; }", pos(1, 22), Msg::OutsideLoop);
    }

    #[test]
    fn flowck_continue() {
        ok("fn a() { while true { continue; } }");
        ok("fn a() { while true { if true { continue; } } }");
        ok("fn a() { loop { continue; } }");
        ok("fn a() { loop { if true { continue; } } }");
        err("fn a() { continue; }", pos(1, 10), Msg::OutsideLoop);
        err("fn a() { loop { } continue; }", pos(1, 19), Msg::OutsideLoop);
        err("fn a() { while true { } continue; }", pos(1, 25), Msg::OutsideLoop);
        err("fn a() { if true { } continue; }", pos(1, 22), Msg::OutsideLoop);
    }
}
