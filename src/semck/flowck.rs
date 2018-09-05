use ctxt::{Fct, FctSrc, SemContext};
use dora_parser::error::msg::Msg;

use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

pub fn check<'ast>(ctxt: &SemContext<'ast>) {
    for fct in ctxt.fcts.iter() {
        let fct = fct.borrow();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.borrow_mut();
        let ast = fct.ast;

        let mut flowck = FlowCheck {
            ctxt: ctxt,
            fct: &fct,
            src: &mut src,
            ast: ast,
            in_loop: false,
        };

        flowck.check();
    }
}

struct FlowCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    in_loop: bool,
}

impl<'a, 'ast> FlowCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    fn handle_loop(&mut self, block: &'ast Stmt) {
        let old_in_loop = self.in_loop;

        self.in_loop = true;
        visit::walk_stmt(self, block);
        self.in_loop = old_in_loop;
    }

    fn handle_flow(&mut self, s: &'ast Stmt) {
        if !self.in_loop {
            self.ctxt
                .diag
                .borrow_mut()
                .report(s.pos(), Msg::OutsideLoop);
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for FlowCheck<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtLoop(_) => self.handle_loop(s),
            StmtWhile(_) => self.handle_loop(s),
            StmtFor(_) => self.handle_loop(s),
            StmtBreak(_) => self.handle_flow(s),
            StmtContinue(_) => self.handle_flow(s),

            _ => visit::walk_stmt(self, s),
        }
    }
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn flowck_break() {
        ok("fun a() { while true { break; } }");
        ok("fun a() { while true { if true { break; } } }");
        ok("fun a() { loop { break; } }");
        ok("fun a() { loop { if true { break; } } }");
        err("fun a() { break; }", pos(1, 11), Msg::OutsideLoop);
        err("fun a() { loop { } break; }", pos(1, 20), Msg::OutsideLoop);
        err(
            "fun a() { while true { } break; }",
            pos(1, 26),
            Msg::OutsideLoop,
        );
        err(
            "fun a() { if true { } break; }",
            pos(1, 23),
            Msg::OutsideLoop,
        );
    }

    #[test]
    fn flowck_continue() {
        ok("fun a() { while true { continue; } }");
        ok("fun a() { while true { if true { continue; } } }");
        ok("fun a() { loop { continue; } }");
        ok("fun a() { loop { if true { continue; } } }");
        err("fun a() { continue; }", pos(1, 11), Msg::OutsideLoop);
        err(
            "fun a() { loop { } continue; }",
            pos(1, 20),
            Msg::OutsideLoop,
        );
        err(
            "fun a() { while true { } continue; }",
            pos(1, 26),
            Msg::OutsideLoop,
        );
        err(
            "fun a() { if true { } continue; }",
            pos(1, 23),
            Msg::OutsideLoop,
        );
    }
}
