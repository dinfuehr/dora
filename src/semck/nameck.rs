use ctxt::*;
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;
use class::ClassId;
use interner::Name;
use lexer::position::Position;

use sym::Sym;
use sym::Sym::*;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &Context<'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if fct.kind.is_src() {
            let ast = fct.ast();
            let mut nameck = NameCheck {
                ctxt: ctxt,
                fct: &mut fct,
                ast: ast,
            };

            nameck.check();
        }
    }
}

struct NameCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
}

impl<'a, 'ast> NameCheck<'a, 'ast> {
    fn check(&mut self) {
        self.ctxt.sym.borrow_mut().push_level();

        if self.fct.ctor {
            // add hidden this parameter for ctors and methods
            self.add_hidden_parameter_self();
        }

        for p in &self.ast.params { self.visit_param(p); }
        self.visit_stmt(&self.ast.block);

        self.ctxt.sym.borrow_mut().pop_level();
    }

    pub fn add_hidden_parameter_self(&mut self) {
        let var_id = VarId(self.fct.src().vars.len());
        let cls_id = self.fct.owner_class.unwrap();
        let ast_id = self.fct.src().ast.id;
        let name = self.ctxt.interner.intern("self");

        let var = Var {
            id: VarId(0),
            name: name,
            ty: BuiltinType::Class(cls_id),
            node_id: ast_id,
            offset: 0
        };

        self.fct.src_mut().vars.push(var);
    }

    pub fn add_var<F>(&mut self, mut var: Var, replacable: F) ->
            Result<VarId, Sym> where F: FnOnce(&Sym) -> bool {
        let name = var.name;
        let var_id = VarId(self.fct.src().vars.len());

        var.id = var_id;

        let result = match self.ctxt.sym.borrow().get(name) {
            Some(sym) => if replacable(&sym) { Ok(var_id) } else { Err(sym) },
            None => Ok(var_id)
        };

        if result.is_ok() {
            self.ctxt.sym.borrow_mut().insert(name, SymVar(var_id));
        }

        self.fct.src_mut().vars.push(var);

        result
    }

    fn check_stmt_let(&mut self, var: &'ast StmtLetType) {
        let var_ctxt = Var {
            id: VarId(0),
            name: var.name,
            ty: BuiltinType::Unit,
            node_id: var.id,
            offset: 0
        };

        // variables are not allowed to replace types, other variables
        // and functions can be replaced
        match self.add_var(var_ctxt, |sym| !sym.is_class()) {
            Ok(var_id) => {
                var.set_var(var_id);
            }

            Err(_) => {
                let name = str(self.ctxt, var.name);
                report(self.ctxt, var.pos, Msg::ShadowClass(name));
            }
        }

        if let Some(ref expr) = var.expr {
            self.visit_expr(expr);
        }
    }

    fn check_stmt_try(&mut self, try: &'ast StmtTryType) {
        self.visit_stmt(&try.try_block);

        for catch in &try.catch_blocks {
            self.ctxt.sym.borrow_mut().push_level();

            let var_ctxt = Var {
                id: VarId(0),
                name: catch.name,
                ty: BuiltinType::Unit,
                node_id: try.id,
                offset: 0
            };

            // variables are not allowed to replace types, other variables
            // and functions can be replaced
            match self.add_var(var_ctxt, |sym| !sym.is_class()) {
                Ok(var_id) => {
                    catch.set_var(var_id);
                }

                Err(_) => {
                    let name = str(self.ctxt, catch.name);
                    report(self.ctxt, catch.pos, Msg::ShadowClass(name));
                }
            }

            self.visit_stmt(&catch.block);
            self.ctxt.sym.borrow_mut().pop_level();
        }

        if let Some(ref finally_block) = try.finally_block {
            self.visit_stmt(finally_block);
        }
    }

    fn check_stmt_block(&mut self, block: &'ast StmtBlockType) {
        self.ctxt.sym.borrow_mut().push_level();
        for stmt in &block.stmts { self.visit_stmt(stmt); }
        self.ctxt.sym.borrow_mut().pop_level();
    }

    fn check_expr_ident(&mut self, ident: &'ast ExprIdentType) {
        if let Some(id) = self.ctxt.sym.borrow().get_var(ident.name) {
            ident.set_var(id);
            return;
        }

        if let Some(clsid) = self.fct.owner_class {
            let cls = self.ctxt.cls_by_id(clsid);

            for prop in &cls.props {
                if prop.name == ident.name {
                    ident.set_prop(clsid, prop.id);
                    return;
                }
            }
        }

        let name = self.ctxt.interner.str(ident.name).to_string();
        report(self.ctxt, ident.pos, Msg::UnknownIdentifier(name));
    }

    fn check_expr_call(&mut self, call: &'ast ExprCallType) {
        let mut found = false;

        // do not check method calls yet
        if call.with_self {
            for arg in &call.args {
                self.visit_expr(arg);
            }

            return;
        }

        if let Some(sym) = self.ctxt.sym.borrow().get(call.name) {
            match sym {
                SymFct(fct_id) => {
                    let call_type = CallType::Fct(fct_id);
                    self.fct.src_mut().calls.insert(call.id, call_type);
                    found = true;
                }

                SymClass(cls_id) => {
                    let cls = self.ctxt.cls_by_id(cls_id);

                    let call_type = CallType::Ctor(cls_id, FctId(0));
                    self.fct.src_mut().calls.insert(call.id, call_type);
                    found = true;
                }

                _ => {}
            }
        }

        if !found {
            let name = str(self.ctxt, call.name);
            report(self.ctxt, call.pos, Msg::UnknownFunction(name));
        }

        // also parse function arguments
        for arg in &call.args {
            self.visit_expr(arg);
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let var_ctxt = Var {
            id: VarId(0),
            name: p.name,
            ty: BuiltinType::Unit,
            node_id: p.id,
            offset: 0,
        };

        // params are only allowed to replace functions,
        // types and vars cannot be replaced
        match self.add_var(var_ctxt, |sym| sym.is_fct()) {
            Ok(var_id) => {
                p.set_var(var_id);
            }

            Err(sym) => {
                let name = str(self.ctxt, p.name);
                let msg = if sym.is_class() {
                    Msg::ShadowClass(name)
                } else {
                    Msg::ShadowParam(name)
                };

                report(self.ctxt, p.pos, msg);
            }
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtLet(ref stmt) => self.check_stmt_let(stmt),
            StmtBlock(ref stmt) => self.check_stmt_block(stmt),
            StmtTry(ref stmt) => self.check_stmt_try(stmt),

            // no need to handle rest of statements
            _ => visit::walk_stmt(self, s)
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprIdent(ref ident) => self.check_expr_ident(ident),
            ExprCall(ref call) => self.check_expr_call(call),

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
    use interner::Name;
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
            Msg::ShadowClass("int".into()));
    }

    #[test]
    fn shadow_type_with_param() {
        err("fn test(bool: Str) {}", pos(1, 9),
            Msg::ShadowClass("bool".into()));
    }

    #[test]
    fn shadow_type_with_var() {
        err("fn test() { let Str = 3; }", pos(1, 13),
            Msg::ShadowClass("Str".into()));
    }

    #[test]
    fn shadow_function() {
        ok("fn f() { let f = 1; }");
        err("fn f() { let f = 1; f(); }", pos(1, 21),
            Msg::UnknownFunction("f".into()));
    }

    #[test]
    fn shadow_var() {
        ok("fn f() { let f = 1; let f = 2; }");
    }

    #[test]
    fn shadow_param() {
        err("fn f(a: int, b: int, a: Str) {}", pos(1, 22),
            Msg::ShadowParam("a".into()));
    }

    #[test]
    fn multiple_params() {
        ok("fn f(a: int, b: int, c:Str) {}");
    }

    #[test]
    fn undefined_variable() {
        err("fn f() { let b = a; }", pos(1, 18), Msg::UnknownIdentifier("a".into()));
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
        err("fn f() -> int { { let a = 1; } return a; }", pos(1, 39),
            Msg::UnknownIdentifier("a".into()));

        ok("fn f() -> int { let a = 1; { let a = 2; } return a; }");
    }
}
