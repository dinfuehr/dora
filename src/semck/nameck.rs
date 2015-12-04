use std::ptr;

use ctxt::*;
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::Type::*;
use ast::visit::*;
use interner::Name;
use lexer::position::Position;

use sym::Sym::*;
use sym::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
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
        let fct = FctInfo {
            name: f.name,
            params_types: Vec::new(),
            return_type: BuiltinType::Unit,
            ast: Some(f),
            vars: Vec::new(),
            stacksize: 0,
            always_returns: false,
            contains_fct_invocation: false,
            compiled_fct: ptr::null(),
        };

        if let Err(sym) = self.ctxt.add_function(fct) {
            let name = self.ctxt.interner.str(f.name).to_string();
            let msg = if sym.is_type() {
                Msg::ShadowType(name)
            } else {
                Msg::ShadowFunction(name)
            };

            report(self.ctxt, f.pos, msg);
        }
    }
}

struct NameCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: Option<NodeId>,
}

impl<'a, 'ast> NameCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> NameCheck<'a, 'ast> {
        NameCheck {
            ctxt: ctxt,
            fct: None,
        }
    }

    fn check_stmt_var(&mut self, var: &'ast StmtVarType) {
        let varinfo = VarInfo {
            name: var.name,
            data_type: BuiltinType::Unit,
            node_id: var.id,
            offset: 0
        };

        // variables are not allowed to replace types, other variables
        // and functions can be replaced
        if let Err(sym) = self.ctxt.add_var(self.fct.unwrap(), varinfo, |sym| !sym.is_type()) {
            let name = str(self.ctxt, var.name);
            report(self.ctxt, var.pos, Msg::ShadowType(name));
        }

        if let Some(ref expr) = var.expr {
            self.visit_expr(expr);
        }
    }

    fn check_stmt_block(&mut self, block: &'ast StmtBlockType) {
        self.ctxt.sym.borrow_mut().push_level();
        for stmt in &block.stmts { self.visit_stmt(stmt); }
        self.ctxt.sym.borrow_mut().pop_level();
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.fct = Some(f.id);
        self.ctxt.sym.borrow_mut().push_level();

        for p in &f.params { self.visit_param(p); }
        self.visit_stmt(&f.block);

        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn visit_param(&mut self, p: &'ast Param) {
        let var = VarInfo {
            name: p.name,
            data_type: BuiltinType::Unit,
            node_id: p.id,
            offset: 0,
        };

        // params are only allowed to replace functions,
        // types and vars cannot be replaced
        if let Err(sym) = self.ctxt.add_var(self.fct.unwrap(), var, |sym| sym.is_function()) {
            let name = str(self.ctxt, p.name);
            let msg = if sym.is_type() {
                Msg::ShadowType(name)
            } else {
                Msg::ShadowParam(name)
            };

            report(self.ctxt, p.pos, msg);
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtBlock(ref stmt) => self.check_stmt_block(stmt),

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
                    self.ctxt.calls.borrow_mut().insert(call.id, id);
                } else {
                    let name = str(self.ctxt, call.name);
                    report(self.ctxt, call.pos, Msg::UnknownFunction(name));
                }

                // also parse function arguments
                for arg in &call.args {
                    self.visit_expr(arg);
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

    #[test]
    fn variable_outside_of_scope() {
        err("fn f() -> int { { var a = 1; } return a; }", pos(1, 39),
            Msg::UnknownIdentifier("a".into()));

        ok("fn f() -> int { var a = 1; { var a = 2; } return a; }");
    }
}
