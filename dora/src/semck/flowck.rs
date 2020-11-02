use crate::error::msg::SemError;
use crate::vm::{Fct, FctSrc, VM};

use dora_parser::ast::visit::*;
use dora_parser::ast::*;

pub fn check(vm: &VM) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();

        let mut flowck = FlowCheck {
            vm,
            fct: &fct,
            src: &mut src,
            in_loop: false,
        };

        flowck.check();
    }
}

struct FlowCheck<'a> {
    vm: &'a VM,
    fct: &'a Fct,
    src: &'a mut FctSrc,
    in_loop: bool,
}

impl<'a> FlowCheck<'a> {
    fn check(&mut self) {
        self.visit_fct(&self.fct.ast);
    }

    fn handle_loop(&mut self, block: &Stmt) {
        let old_in_loop = self.in_loop;

        self.in_loop = true;
        visit::walk_stmt(self, block);
        self.in_loop = old_in_loop;
    }

    fn handle_flow(&mut self, s: &Stmt) {
        if !self.in_loop {
            self.vm
                .diag
                .lock()
                .report(self.fct.file_id, s.pos(), SemError::OutsideLoop);
        }
    }
}

impl<'a> Visitor for FlowCheck<'a> {
    fn visit_stmt(&mut self, s: &Stmt) {
        match *s {
            Stmt::While(_) => self.handle_loop(s),
            Stmt::For(_) => self.handle_loop(s),
            Stmt::Break(_) => self.handle_flow(s),
            Stmt::Continue(_) => self.handle_flow(s),

            _ => visit::walk_stmt(self, s),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn flowck_break() {
        ok("fun a() { while true { break; } }");
        ok("fun a() { while true { if true { break; } } }");
        err("fun a() { break; }", pos(1, 11), SemError::OutsideLoop);
        err(
            "fun a() { while true { } break; }",
            pos(1, 26),
            SemError::OutsideLoop,
        );
        err(
            "fun a() { if true { } break; }",
            pos(1, 23),
            SemError::OutsideLoop,
        );
    }

    #[test]
    fn flowck_continue() {
        ok("fun a() { while true { continue; } }");
        ok("fun a() { while true { if true { continue; } } }");
        err("fun a() { continue; }", pos(1, 11), SemError::OutsideLoop);
        err(
            "fun a() { while true { } continue; }",
            pos(1, 26),
            SemError::OutsideLoop,
        );
        err(
            "fun a() { if true { } continue; }",
            pos(1, 23),
            SemError::OutsideLoop,
        );
    }
}
