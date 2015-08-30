use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::{Ast, Function, Param, Type};
use parser::ast::Type::*;
use parser::ast::visit;
use parser::ast::visit::Visitor;

use sym::BuiltinType;
use sym::Sym::*;

pub fn check(ctxt: &Context, ast: &Ast) {
    TypeCheck::new(ctxt).visit_ast(ast);
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> TypeCheck<'a, 'ast> {
        TypeCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_type(&mut self, t: &'ast Type) {
        match *t {
            TypeBasic(ref basic) => {
                if let Some(builtin) = self.ctxt.sym.borrow().get_type(basic.name) {
                    self.ctxt.types.borrow_mut().insert(basic.id, builtin);
                } else {
                    let tyname = self.ctxt.interner.str(basic.name).clone_string();
                    let msg = Msg::UnknownType(tyname);
                    self.ctxt.diag.borrow_mut().report(basic.pos, msg);
                }
            }

            _ => self.ctxt.diag.borrow_mut().report_unimplemented(t.pos())
        }
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn type_def_for_return_type() {
        ok("fn a() -> int {}");
        err("fn a() -> unknown {}", pos(1, 11), Msg::UnknownType("unknown".into()));
    }

    #[test]
    fn type_def_for_param() {
        ok("fn a(b: int) {}");
        err("fn a(b: foo) {}", pos(1, 9), Msg::UnknownType("foo".into()));
    }

    #[test]
    fn type_def_for_var() {
        ok("fn a() { var a : int = 1; }");
        err("fn a() { var a : test = 1; }", pos(1, 18), Msg::UnknownType("test".into()));
    }
}
