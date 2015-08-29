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
        let found = self.ctxt.sym.borrow().get(f.name).is_some();

        if found {
            let name = self.ctxt.interner.str(f.name).clone_string();
            report(self.ctxt, f.pos, Msg::IdentifierExists(name));
        } else {
            self.ctxt.sym.borrow_mut().insert(f.name, SymFunction(f.id));
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
        let redefinable = self.ctxt.sym.borrow().get(p.name)
            .map_or(true, |sym| sym.is_function());

        if redefinable {
            self.ctxt.sym.borrow_mut().insert(p.name, SymVar(p.id));
        } else {
            let name = str(self.ctxt, p.name);
            report(self.ctxt, p.pos, Msg::IdentifierExists(name));
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
                    let name = str(self.ctxt, var.name);
                    report(self.ctxt, var.pos, Msg::ShadowType(name));
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
                    self.ctxt.var_uses.borrow_mut().insert(ident.id, id);
                } else {
                    let name = str(self.ctxt, ident.name);
                    report(self.ctxt, ident.pos, Msg::UnknownIdentifier(name));
                }
            }

            ExprCall(ref call) => {
                if let Some(id) = self.ctxt.sym.borrow().get_function(call.name) {
                    self.ctxt.var_uses.borrow_mut().insert(call.id, id);
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
    ctxt.interner.str(name).clone_string()
}

#[cfg(test)]
mod tests {
    use driver::ctxt::Context;
    use driver::cmd::Args;
    use parser::lexer::position::Position;
    use parser::Parser;
    use parser::ast;
    use semck::nameck;
    use semck::prelude;

    fn check<F>(code: &'static str, f: F) where F: FnOnce(&Context) -> () {
        let mut parser = Parser::from_str(code);
        let (ast, interner) = parser.parse().unwrap();
        let map = ast::map::build(&ast, &interner);
        let args : Args = Default::default();

        ast::dump::dump(&ast, &interner);

        let ctxt = Context::new(&args, &interner, &map, &ast);

        prelude::init(&ctxt);
        nameck::check(&ctxt, &ast);

        f(&ctxt);
    }

    fn ok(code: &'static str) {
        check(code, |ctxt| {
            assert!(!ctxt.diag.borrow().has_errors());
        });
    }

    fn err(code: &'static str, pos: Position) {
        check(code, |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();

            assert_eq!(1, errors.len());
            assert_eq!(pos, errors[0].pos);
        });
    }

    fn pos(line: u32, col: u32) -> Position {
        Position::new(line, col)
    }

    #[test]
    fn multiple_functions() {
        ok("fn f() {}\nfn g() {}");
    }

    #[test]
    fn redefine_function() {
        err("fn f() {}\nfn f() {}", pos(2, 1));
    }

    #[test]
    fn shadow_type_with_function() {
        err("fn int() {}", pos(1, 1));
    }

    #[test]
    fn shadow_type_with_param() {
        err("fn test(int: str) {}", pos(1, 9));
    }

    #[test]
    fn shadow_type_with_var() {
        err("fn test() { var int = 3; }", pos(1, 13));
    }

    #[test]
    fn shadow_function() {
        ok("fn f() { var f = 1; }");
        err("fn f() { var f = 1; f(); }", pos(1, 21));
    }

    #[test]
    fn shadow_var() {
        ok("fn f() { var f = 1; var f = 2; }");
    }

    #[test]
    fn shadow_param() {
        err("fn f(a: int, b: int, a: str) {}", pos(1, 22));
    }

    #[test]
    fn multiple_params() {
        ok("fn f(a: int, b: int, c:str) {}");
    }

    #[test]
    fn undefined_variable() {
        err("fn f() { var b = a; }", pos(1, 18));
        err("fn f() { a; }", pos(1, 10));
    }

    #[test]
    fn undefined_function() {
        err("fn f() { foo(); }", pos(1, 10));
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
