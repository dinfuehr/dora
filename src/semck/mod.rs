use driver::ctxt::Context;
use parser::ast::{Function, Type};
use parser::ast::Type::*;
use parser::ast::visit;
use parser::ast::visit::Visitor;

use sym::Sym::*;
use sym::BuiltinType;

pub fn check(ctxt: &Context) {
    add_builtin_types(ctxt);

    SemCheck::new(ctxt).visit_ast(ctxt.ast);
}

fn add_builtin_types(ctxt: &Context) {
    let mut sym = ctxt.sym.borrow_mut();

    let name = ctxt.interner.intern("int".into());
    assert!(sym.insert(name, SymType(BuiltinType::Int)).is_none());

    let name = ctxt.interner.intern("bool".into());
    assert!(sym.insert(name, SymType(BuiltinType::Bool)).is_none());

    let name = ctxt.interner.intern("str".into());
    assert!(sym.insert(name, SymType(BuiltinType::Str)).is_none());
}

struct SemCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> SemCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> SemCheck<'a, 'ast> {
        SemCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for SemCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        println!("{}", self.ctxt.interner.str(f.name));

        visit::walk_fct(self, f);
    }
}
