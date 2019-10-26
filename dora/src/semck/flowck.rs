use crate::vm::{Fct, FctSrc, VM};

use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

pub fn check<'ast>(vm: &VM<'ast>) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();
        let ast = fct.ast;

        let mut flowck = FlowCheck {
            vm,
            fct: &fct,
            src: &mut src,
            ast,
            in_loop: false,
        };

        flowck.check();
    }
}

struct FlowCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
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
}

impl<'a, 'ast> Visitor<'ast> for FlowCheck<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtWhile(_) => self.handle_loop(s),
            StmtFor(_) => self.handle_loop(s),

            _ => visit::walk_stmt(self, s),
        }
    }
}
