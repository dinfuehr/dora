use driver::ctxt::Context;

use parser::ast::*;
use parser::ast::visit::*;

pub fn check(ctxt: &Context, ast: &Ast) {
    FlowCheck::new(ctxt).visit_ast(ast);
}

struct FlowCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> FlowCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> FlowCheck<'a, 'ast> {
        FlowCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for FlowCheck<'a, 'ast> {
}
