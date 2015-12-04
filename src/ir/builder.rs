use ctxt::Context;

use ast::*;
use ast::visit::*;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    Builder::new(ctxt).visit_ast(ast);
}

struct Builder<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
}

impl<'a, 'ast> Builder<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> Builder<'a, 'ast> {
        Builder {
            ctxt: ctxt,
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for Builder<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        println!("Hallo!");
    }
}
