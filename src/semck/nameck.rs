use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::{Ast, Function, Param, Type};
use parser::ast::Expr;
use parser::ast::Expr::*;
use parser::ast::Stmt;
use parser::ast::Stmt::*;
use parser::ast::Type::*;
use parser::ast::visit;
use parser::ast::visit::Visitor;
use parser::interner::Name;
use parser::lexer::position::Position;

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

    fn report(&self, pos: Position, msg: Msg) {
        self.ctxt.diag.borrow_mut().report(pos, msg);
    }

    fn str(&self, name: Name) -> String {
        self.ctxt.interner.str(name).clone_string()
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let found = self.ctxt.sym.borrow().get(f.name).is_some();

        if found {
            self.report(f.pos, Msg::IdentifierExists(self.str(f.name)));
        } else {
            self.ctxt.sym.borrow_mut().insert(f.name, SymFunction(f.id));
        }

        self.ctxt.sym.borrow_mut().push_level();

        for p in &f.params { self.visit_param(p); }
        self.visit_stmt(&f.block);

        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn visit_param(&mut self, p: &'ast Param) {
        // params are only allowed to replace functions,
        // types and vars cannot be replaced
        let redefinable = self.ctxt.sym.borrow().get(p.name)
            .map_or(true, |sym| sym.is_function());

        if redefinable {
            self.ctxt.sym.borrow_mut().insert(p.name, SymVar(p.id));
        } else {
            self.report(p.pos, Msg::IdentifierExists(self.str(p.name)));
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => {
                let redefinable = self.ctxt.sym.borrow().get(var.name)
                    .map_or(true, |sym| !sym.is_type());

                if redefinable {
                    self.ctxt.sym.borrow_mut().insert(var.name, SymVar(var.id));
                } else {
                    self.report(var.pos, Msg::ShadowType(self.str(var.name)));
                }
            }

            // no need to handle rest of statements
            _ => visit::walk_stmt(self, s)
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprIdent(ref ident) => {
                if let Some(id) = self.ctxt.sym.borrow().get_var(ident.name) {
                    self.ctxt.var_uses.borrow_mut().insert(ident.id, id);
                } else {
                    self.report(ident.pos, Msg::UnknownIdentifier(self.str(ident.name)));
                }
            }

            // no need to handle rest of expressions
            _ => visit::walk_expr(self, e)
        }
    }
}
