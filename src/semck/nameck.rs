use crate::vm::*;
use dora_parser::error::msg::Msg;

use dora_parser::ast::visit::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::sym::Sym;
use crate::sym::Sym::*;
use crate::ty::BuiltinType;

pub fn check<'ast>(vm: &VM<'ast>) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();
        let ast = fct.ast;

        let mut nameck = NameCheck {
            vm: vm,
            fct: &fct,
            src: &mut src,
            ast: ast,
        };

        nameck.check();
    }
}

struct NameCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
}

impl<'a, 'ast> NameCheck<'a, 'ast> {
    fn check(&mut self) {
        self.vm.sym.lock().push_level();

        if self.fct.has_self() {
            // add hidden this parameter for ctors and methods
            self.add_hidden_parameter_self();
        }

        for p in &self.ast.params {
            self.visit_param(p);
        }

        self.visit_stmt(self.ast.block());

        self.vm.sym.lock().pop_level();
    }

    pub fn add_hidden_parameter_self(&mut self) {
        let ty = match self.fct.parent {
            FctParent::Class(cls_id) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();

                cls.ty
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                let cls = self.vm.classes.idx(ximpl.cls_id());
                let cls = cls.read();

                cls.ty
            }

            _ => unreachable!(),
        };

        let ast_id = self.fct.ast.id;
        let name = self.vm.interner.intern("self");

        let var = Var {
            id: VarId(0),
            name: name,
            ty: ty,
            reassignable: false,
            node_id: ast_id,
        };

        self.src.vars.push(var);
    }

    pub fn add_var<F>(&mut self, mut var: Var, replacable: F) -> Result<VarId, Sym>
    where
        F: FnOnce(&Sym) -> bool,
    {
        let name = var.name;
        let var_id = VarId(self.src.vars.len());

        var.id = var_id;

        let result = match self.vm.sym.lock().get(name) {
            Some(sym) => {
                if replacable(&sym) {
                    Ok(var_id)
                } else {
                    Err(sym)
                }
            }
            None => Ok(var_id),
        };

        if result.is_ok() {
            self.vm.sym.lock().insert(name, SymVar(var_id));
        }

        self.src.vars.push(var);

        result
    }

    fn check_stmt_var(&mut self, var: &'ast StmtVarType) {
        let var_ctxt = Var {
            id: VarId(0),
            name: var.name,
            reassignable: var.reassignable,
            ty: BuiltinType::Unit,
            node_id: var.id,
        };

        if let Some(ref expr) = var.expr {
            self.visit_expr(expr);
        }

        // variables are not allowed to replace types, other variables
        // and functions can be replaced
        match self.add_var(var_ctxt, |sym| !sym.is_class()) {
            Ok(var_id) => {
                self.src.map_vars.insert(var.id, var_id);
            }

            Err(_) => {
                let name = str(self.vm, var.name);
                report(self.vm, var.pos, Msg::ShadowClass(name));
            }
        }
    }

    fn check_stmt_for(&mut self, for_loop: &'ast StmtForType) {
        self.visit_expr(&for_loop.expr);

        self.vm.sym.lock().push_level();

        let var_ctxt = Var {
            id: VarId(0),
            name: for_loop.name,
            reassignable: false,
            ty: BuiltinType::Unit,
            node_id: for_loop.id,
        };

        match self.add_var(var_ctxt, |sym| !sym.is_class()) {
            Ok(var_id) => {
                self.src.map_vars.insert(for_loop.id, var_id);
            }

            Err(_) => {
                let name = str(self.vm, for_loop.name);
                report(self.vm, for_loop.pos, Msg::ShadowClass(name));
            }
        }

        self.visit_stmt(&for_loop.block);
        self.vm.sym.lock().pop_level();
    }

    fn check_stmt_do(&mut self, r#try: &'ast StmtDoType) {
        self.visit_stmt(&r#try.do_block);

        for catch in &r#try.catch_blocks {
            self.vm.sym.lock().push_level();

            let var_ctxt = Var {
                id: VarId(0),
                name: catch.name,
                ty: BuiltinType::Unit,
                reassignable: false,
                node_id: r#try.id,
            };

            // variables are not allowed to replace types, other variables
            // and functions can be replaced
            match self.add_var(var_ctxt, |sym| !sym.is_class()) {
                Ok(var_id) => {
                    self.src.map_vars.insert(catch.id, var_id);
                }

                Err(_) => {
                    let name = str(self.vm, catch.name);
                    report(self.vm, catch.pos, Msg::ShadowClass(name));
                }
            }

            self.visit_stmt(&catch.block);
            self.vm.sym.lock().pop_level();
        }

        if let Some(ref finally_block) = r#try.finally_block {
            self.visit_stmt(&finally_block.block);
        }
    }

    fn check_stmt_block(&mut self, block: &'ast StmtBlockType) {
        self.vm.sym.lock().push_level();
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        self.vm.sym.lock().pop_level();
    }

    fn check_expr_ident(&mut self, ident: &'ast ExprIdentType) {
        let sym = self.vm.sym.lock().get(ident.name);

        match sym {
            Some(SymVar(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Var(id));
            }

            Some(SymGlobal(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Global(id));
            }

            Some(SymStruct(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Struct(id));
            }

            Some(SymConst(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Const(id));
            }

            Some(SymFct(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Fct(id));
            }

            Some(SymClass(id)) => {
                self.src.map_idents.insert(ident.id, IdentType::Class(id));
            }

            _ => {
                let name = self.vm.interner.str(ident.name).to_string();
                report(self.vm, ident.pos, Msg::UnknownIdentifier(name));
            }
        }
    }

    fn check_expr_struct(&mut self, struc: &'ast ExprLitStructType) {
        if let Some(sid) = self.vm.sym.lock().get_struct(struc.path.name()) {
            self.src.map_idents.insert(struc.id, IdentType::Struct(sid));
        } else {
            let name = self.vm.interner.str(struc.path.name()).to_string();
            report(self.vm, struc.pos, Msg::UnknownStruct(name));
        }
    }

    fn check_expr_path(&mut self, path: &'ast ExprPathType) {
        self.visit_expr(&path.lhs);
        // do not check right hand site of path
    }
}

impl<'a, 'ast> Visitor<'ast> for NameCheck<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        let var_ctxt = Var {
            id: VarId(0),
            name: p.name,
            reassignable: p.reassignable,
            ty: BuiltinType::Unit,
            node_id: p.id,
        };

        // params are only allowed to replace functions,
        // types and vars cannot be replaced
        match self.add_var(var_ctxt, |sym| sym.is_fct()) {
            Ok(var_id) => {
                self.src.map_vars.insert(p.id, var_id);
            }

            Err(sym) => {
                let name = str(self.vm, p.name);
                let msg = if sym.is_class() {
                    Msg::ShadowClass(name)
                } else {
                    Msg::ShadowParam(name)
                };

                report(self.vm, p.pos, msg);
            }
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtBlock(ref stmt) => self.check_stmt_block(stmt),
            StmtDo(ref stmt) => self.check_stmt_do(stmt),
            StmtFor(ref stmt) => self.check_stmt_for(stmt),

            // no need to handle rest of statements
            _ => visit::walk_stmt(self, s),
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match e {
            &ExprIdent(ref ident) => self.check_expr_ident(ident),
            &ExprLitStruct(ref lit) => self.check_expr_struct(lit),
            &ExprPath(ref path) => self.check_expr_path(path),

            // no need to handle rest of expressions
            _ => visit::walk_expr(self, e),
        }
    }
}

fn report(vm: &VM, pos: Position, msg: Msg) {
    vm.diag.lock().report_without_path(pos, msg);
}

fn str(vm: &VM, name: Name) -> String {
    vm.interner.str(name).to_string()
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use dora_parser::error::msg::Msg;

    #[test]
    fn multiple_functions() {
        ok("fun f() {}\nfun g() {}");
    }

    #[test]
    fn redefine_function() {
        err(
            "fun f() {}\nfun f() {}",
            pos(2, 1),
            Msg::ShadowFunction("f".into()),
        );
    }

    #[test]
    fn shadow_type_with_function() {
        err("fun Int() {}", pos(1, 1), Msg::ShadowClass("Int".into()));
    }

    #[test]
    fn shadow_type_with_param() {
        err(
            "fun test(Bool: String) {}",
            pos(1, 10),
            Msg::ShadowClass("Bool".into()),
        );
    }

    #[test]
    fn shadow_type_with_var() {
        err(
            "fun test() { let String = 3; }",
            pos(1, 14),
            Msg::ShadowClass("String".into()),
        );
    }

    #[test]
    fn shadow_function() {
        ok("fun f() { let f = 1; }");
        err(
            "fun f() { let f = 1; f(); }",
            pos(1, 23),
            Msg::UnknownMethod("Int".into(), "get".into(), Vec::new()),
        );
    }

    #[test]
    fn shadow_var() {
        ok("fun f() { let f = 1; let f = 2; }");
    }

    #[test]
    fn shadow_param() {
        err(
            "fun f(a: Int, b: Int, a: String) {}",
            pos(1, 23),
            Msg::ShadowParam("a".into()),
        );
    }

    #[test]
    fn multiple_params() {
        ok("fun f(a: Int, b: Int, c:String) {}");
    }

    #[test]
    fn undefined_variable() {
        err(
            "fun f() { let b = a; }",
            pos(1, 19),
            Msg::UnknownIdentifier("a".into()),
        );
        err(
            "fun f() { a; }",
            pos(1, 11),
            Msg::UnknownIdentifier("a".into()),
        );
    }

    #[test]
    fn undefined_function() {
        err(
            "fun f() { foo(); }",
            pos(1, 11),
            Msg::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn recursive_function_call() {
        ok("fun f() { f(); }");
    }

    #[test]
    fn function_call() {
        ok("fun a() {}\nfun b() { a(); }");

        // non-forward definition of functions
        ok("fun a() { b(); }\nfun b() {}");
    }

    #[test]
    fn variable_outside_of_scope() {
        err(
            "fun f() -> Int { { let a = 1; } return a; }",
            pos(1, 40),
            Msg::UnknownIdentifier("a".into()),
        );

        ok("fun f() -> Int { let a = 1; { let a = 2; } return a; }");
    }

    #[test]
    fn const_value() {
        ok("const one: Int = 1;
            fun f() -> Int { return one; }");
    }

    #[test]
    fn for_var() {
        ok("fun f() { for i in range(0, 4) { i; } }");
    }
}
