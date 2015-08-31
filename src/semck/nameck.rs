use driver::ctxt::Context;
use error::msg::Msg;

use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::Type::*;
use parser::ast::visit::*;
use parser::interner::Name;
use parser::lexer::position::Position;

use sym::Sym::*;
use sym::BuiltinType;

pub fn check(ctxt: &Context, ast: &Ast) {
    GlobalDef::new(ctxt).visit_ast(ast);
    NameCheck::new(ctxt).visit_ast(ast);
}

struct GlobalDef<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> GlobalDef<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> GlobalDef<'a, 'ast> {
        GlobalDef {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for GlobalDef<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let entry_type = self.ctxt.sym.borrow().get_entry_type(f.name);

        if entry_type.is_empty() {
            self.ctxt.sym.borrow_mut().insert(f.name, SymFunction(f.id));
        } else {
            let name = self.ctxt.interner.str(f.name).to_string();
            let msg = if entry_type.is_type() {
                Msg::ShadowType(name)
            } else {
                Msg::ShadowFunction(name)
            };

            report(self.ctxt, f.pos, msg);
        }
    }
}

struct NameCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>
}

impl<'a, 'ast> NameCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> NameCheck<'a, 'ast> {
        NameCheck {
            ctxt: ctxt
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.ctxt.sym.borrow_mut().push_level();

        for p in &f.params { self.visit_param(p); }
        self.visit_stmt(&f.block);

        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn visit_param(&mut self, p: &'ast Param) {
        // params are only allowed to replace functions,
        // types and vars cannot be replaced
        let entry_type = self.ctxt.sym.borrow().get_entry_type(p.name);

        if entry_type.is_empty() || entry_type.is_function() {
            self.ctxt.sym.borrow_mut().insert(p.name, SymVar(p.id));
        } else {
            let name = str(self.ctxt, p.name);
            let msg = if entry_type.is_type() {
                Msg::ShadowType(name)
            } else {
                Msg::ShadowParam(name)
            };

            report(self.ctxt, p.pos, msg);
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => {
                let entry_type = self.ctxt.sym.borrow().get_entry_type(var.name);

                if entry_type.is_type() {
                    let name = str(self.ctxt, var.name);
                    report(self.ctxt, var.pos, Msg::ShadowType(name));
                } else {
                    self.ctxt.sym.borrow_mut().insert(var.name, SymVar(var.id));
                }

                if let Some(ref expr) = var.expr {
                    self.visit_expr(expr);
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
                    self.ctxt.defs.borrow_mut().insert(ident.id, id);
                } else {
                    let name = str(self.ctxt, ident.name);
                    report(self.ctxt, ident.pos, Msg::UnknownIdentifier(name));
                }
            }

            ExprCall(ref call) => {
                if let Some(id) = self.ctxt.sym.borrow().get_function(call.name) {
                    self.ctxt.defs.borrow_mut().insert(call.id, id);
                } else {
                    let name = str(self.ctxt, call.name);
                    report(self.ctxt, call.pos, Msg::UnknownFunction(name));
                }
            }

            // no need to handle rest of expressions
            _ => visit::walk_expr(self, e)
        }
    }
}

fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

fn str(ctxt: &Context, name: Name) -> String {
    ctxt.interner.str(name).to_string()
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn multiple_functions() {
        ok("fn f() {}\nfn g() {}");
    }

    #[test]
    fn redefine_function() {
        err("fn f() {}\nfn f() {}", pos(2, 1),
            Msg::ShadowFunction("f".into()));
    }

    #[test]
    fn shadow_type_with_function() {
        err("fn int() {}", pos(1, 1),
            Msg::ShadowType("int".into()));
    }

    #[test]
    fn shadow_type_with_param() {
        err("fn test(bool: str) {}", pos(1, 9),
            Msg::ShadowType("bool".into()));
    }

    #[test]
    fn shadow_type_with_var() {
        err("fn test() { var str = 3; }", pos(1, 13),
            Msg::ShadowType("str".into()));
    }

    #[test]
    fn shadow_function() {
        ok("fn f() { var f = 1; }");
        err("fn f() { var f = 1; f(); }", pos(1, 21),
            Msg::UnknownFunction("f".into()));
    }

    #[test]
    fn shadow_var() {
        ok("fn f() { var f = 1; var f = 2; }");
    }

    #[test]
    fn shadow_param() {
        err("fn f(a: int, b: int, a: str) {}", pos(1, 22),
            Msg::ShadowParam("a".into()));
    }

    #[test]
    fn multiple_params() {
        ok("fn f(a: int, b: int, c:str) {}");
    }

    #[test]
    fn undefined_variable() {
        err("fn f() { var b = a; }", pos(1, 18), Msg::UnknownIdentifier("a".into()));
        err("fn f() { a; }", pos(1, 10), Msg::UnknownIdentifier("a".into()));
    }

    #[test]
    fn undefined_function() {
        err("fn f() { foo(); }", pos(1, 10),
            Msg::UnknownFunction("foo".into()));
    }

    #[test]
    fn recursive_function_call() {
        ok("fn f() { f(); }");
    }

    #[test]
    fn function_call() {
        ok("fn a() {}\nfn b() { a(); }");

        // non-forward definition of functions
        ok("fn a() { b(); }\nfn b() {}");
    }
}
