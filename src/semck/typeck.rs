use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::{Ast, Function, Param, Type};
use parser::ast::Type::*;
use parser::ast::visit;
use parser::ast::visit::Visitor;

use sym::BuiltinType;
use sym::Sym::*;

pub fn check(ctxt: &Context, ast: &Ast) {
    add_builtin_types(ctxt);

    TypeCheck::new(ctxt).visit_ast(ast);
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
