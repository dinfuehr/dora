use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::{Ast, Function, Param, Type};
use parser::ast::Type::*;
use parser::ast::visit;
use parser::ast::visit::Visitor;

use sym::Sym::*;
use sym::BuiltinType;

pub fn check(ctxt: &Context, ast: &Ast) {
    NameCheck::new(ctxt).visit_ast(ast);
}

pub struct NameCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> NameCheck<'a, 'ast> {
    pub fn new(ctxt: &'a Context<'a, 'ast>) -> NameCheck<'a, 'ast> {
        NameCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let found = self.ctxt.sym.borrow().get(f.name).is_some();

        if found {
            let fname = self.ctxt.interner.str(f.name).clone_string();
            let msg = Msg::IdentifierExists(fname);

            self.ctxt.diag.borrow_mut().report(f.pos, msg);
        } else {
            self.ctxt.sym.borrow_mut().insert(f.name, SymFunction(f.id));
        }

        self.ctxt.sym.borrow_mut().push_level();

        for p in &f.params { self.visit_param(p); }
        self.visit_stmt(&f.block);

        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn visit_param(&mut self, p: &'ast Param) {
        let found = self.ctxt.sym.borrow().get_last(p.name).is_some();

        if found {
            let fname = self.ctxt.interner.str(p.name).clone_string();
            let msg = Msg::IdentifierExists(fname);

            self.ctxt.diag.borrow_mut().report(p.pos, msg);
        } else {
            self.ctxt.sym.borrow_mut().insert(p.name, SymVar(p.id));
        }
    }
}
