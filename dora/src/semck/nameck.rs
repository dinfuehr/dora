use crate::error::msg::SemError;
use crate::vm::*;

use dora_parser::ast::visit::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::semck::globaldef::{report_term_shadow, report_type_shadow};
use crate::sym::TermSym::{
    SymClassConstructor, SymConst, SymFct, SymGlobal, SymModule, SymStructConstructor, SymVar,
};
use crate::sym::TypeSym::{SymClass, SymClassTypeParam, SymEnum, SymFctTypeParam, SymStruct};
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
            vm,
            fct: &fct,
            src: &mut src,
            ast,
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

        if let FctParent::Class(cls_id) = self.fct.parent {
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            for (tpid, tp) in cls.type_params.iter().enumerate() {
                self.vm
                    .sym
                    .lock()
                    .insert_type(tp.name, SymClassTypeParam(cls_id, tpid.into()));
            }
        }

        if let Some(ref type_params) = self.fct.ast.type_params {
            for (tpid, tp) in type_params.iter().enumerate() {
                self.vm
                    .sym
                    .lock()
                    .insert_type(tp.name, SymFctTypeParam(self.fct.id, tpid.into()));
            }
        }

        for p in &self.ast.params {
            self.visit_param(p);
        }

        {
            let block = self.ast.block();

            for stmt in &block.stmts {
                self.visit_stmt(stmt);
            }

            if let Some(ref value) = block.expr {
                self.visit_expr(value);
            }
        }

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
                let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
                let cls = cls.read();

                cls.ty
            }

            FctParent::Extension(extension_id) => {
                let extension = self.vm.extensions[extension_id].read();
                extension.class_ty
            }

            _ => unreachable!(),
        };

        let ast_id = self.fct.ast.id;
        let name = self.vm.interner.intern("self");

        let var = Var {
            id: VarId(0),
            name,
            ty,
            reassignable: false,
            node_id: ast_id,
        };

        self.src.vars.push(var);
    }

    pub fn add_var(&mut self, mut var: Var) -> VarId {
        let name = var.name;
        let var_id = VarId(self.src.vars.len());

        var.id = var_id;

        self.vm.sym.lock().insert_term(name, SymVar(var_id));
        self.src.vars.push(var);

        var_id
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

        let type_sym = self.vm.sym.lock().get_type(var.name);
        match type_sym {
            Some(SymClass(_)) | Some(SymStruct(_)) => {
                report_type_shadow(self.vm, var.name, self.fct.file, var.pos, type_sym.unwrap())
            }
            _ => {
                let var_id = self.add_var(var_ctxt);
                self.src.map_vars.insert(var.id, var_id)
            }
        }
    }

    fn check_stmt_for(&mut self, fl: &'ast StmtForType) {
        self.visit_expr(&fl.expr);

        self.vm.sym.lock().push_level();

        let var_ctxt = Var {
            id: VarId(0),
            name: fl.name,
            reassignable: false,
            ty: BuiltinType::Unit,
            node_id: fl.id,
        };
        let type_sym = self.vm.sym.lock().get_type(fl.name);
        match type_sym {
            Some(SymClass(_)) | Some(SymStruct(_)) => {
                report_type_shadow(self.vm, fl.name, self.fct.file, fl.pos, type_sym.unwrap())
            }
            _ => {
                let var_id = self.add_var(var_ctxt);
                self.src.map_vars.insert(fl.id, var_id);
            }
        }

        self.visit_stmt(&fl.block);
        self.vm.sym.lock().pop_level();
    }

    fn check_expr_ident(&mut self, ident: &'ast ExprIdentType) {
        let term_sym = self.vm.sym.lock().get_term(ident.name);
        let type_sym = self.vm.sym.lock().get_type(ident.name);

        match (term_sym, type_sym) {
            (Some(SymVar(id)), None) => {
                self.src.map_idents.insert(ident.id, IdentType::Var(id));
            }

            (Some(SymGlobal(id)), None) => {
                self.src.map_idents.insert(ident.id, IdentType::Global(id));
            }

            (Some(SymConst(id)), None) => {
                self.src.map_idents.insert(ident.id, IdentType::Const(id));
            }

            (Some(SymFct(id)), None) => {
                self.src.map_idents.insert(ident.id, IdentType::Fct(id));
            }

            (Some(SymModule(id)), None) => {
                self.src.map_idents.insert(ident.id, IdentType::Module(id));
            }

            (None, Some(SymStruct(id))) => {
                self.src.map_idents.insert(ident.id, IdentType::Struct(id));
            }

            (None, Some(SymClass(id))) => {
                self.src.map_idents.insert(ident.id, IdentType::Class(id));
            }

            (None, Some(SymFctTypeParam(fct_id, id))) => {
                let ty = BuiltinType::FctTypeParam(fct_id, id);
                self.src
                    .map_idents
                    .insert(ident.id, IdentType::TypeParam(ty));
            }

            (None, Some(SymClassTypeParam(cls_id, id))) => {
                let ty = BuiltinType::ClassTypeParam(cls_id, id);
                self.src
                    .map_idents
                    .insert(ident.id, IdentType::TypeParam(ty));
            }

            (None, Some(SymEnum(id))) => {
                self.src.map_idents.insert(ident.id, IdentType::Enum(id));
            }

            (Some(SymClassConstructor(id)), _) => {
                self.src.map_idents.insert(ident.id, IdentType::Class(id))
            }

            (Some(SymStructConstructor(id)), _) => {
                self.src.map_idents.insert(ident.id, IdentType::Struct(id))
            }

            (None, None) => {
                let name = self.vm.interner.str(ident.name).to_string();
                report(
                    self.vm,
                    self.fct.file,
                    ident.pos,
                    SemError::UnknownIdentifier(name),
                );
            }

            _ => unreachable!(),
        }
    }

    fn check_expr_path(&mut self, path: &'ast ExprPathType) {
        self.visit_expr(&path.lhs);
        // do not check right hand site of path
    }

    fn check_expr_dot(&mut self, dot: &'ast ExprDotType) {
        self.visit_expr(&dot.lhs);
        // do not check right hand site of dot
    }

    fn check_expr_block(&mut self, block: &'ast ExprBlockType) {
        self.vm.sym.lock().push_level();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref expr) = block.expr {
            self.visit_expr(expr);
        }

        self.vm.sym.lock().pop_level();
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

        // params are only allowed to replace functions, vars cannot be replaced
        let term_sym = self.vm.sym.lock().get_term(p.name);
        match term_sym {
            Some(SymFct(_)) | None => {
                let var_id = self.add_var(var_ctxt);
                self.src.map_vars.insert(p.id, var_id)
            }
            Some(conflict_sym) => {
                report_term_shadow(self.vm, p.name, self.fct.file, p.pos, conflict_sym)
            }
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtFor(ref stmt) => self.check_stmt_for(stmt),

            // no need to handle rest of statements
            _ => visit::walk_stmt(self, s),
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match e {
            &ExprIdent(ref ident) => self.check_expr_ident(ident),
            &ExprPath(ref path) => self.check_expr_path(path),
            &ExprDot(ref dot) => self.check_expr_dot(dot),
            &ExprBlock(ref block) => self.check_expr_block(block),

            // no need to handle rest of expressions
            _ => visit::walk_expr(self, e),
        }
    }
}

fn report(vm: &VM, file: FileId, pos: Position, msg: SemError) {
    vm.diag.lock().report(file, pos, msg);
}

fn str(vm: &VM, name: Name) -> String {
    vm.interner.str(name).to_string()
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::error::msg::SemError::ShadowClassConstructor;
    use crate::semck::tests::*;

    #[test]
    fn multiple_functions() {
        ok("fun f() {}\nfun g() {}");
    }

    #[test]
    fn redefine_function() {
        err(
            "fun f() {}\nfun f() {}",
            pos(2, 1),
            SemError::ShadowFunction("f".into()),
        );
    }

    #[test]
    fn shadow_type_with_function() {
        err(
            "fun Int() {}",
            pos(1, 1),
            SemError::ShadowClassConstructor("Int".into()),
        );
    }

    #[test]
    fn shadow_type_with_param() {
        err(
            "fun test(Bool: String) {}",
            pos(1, 10),
            ShadowClassConstructor("Bool".into()),
        );
    }

    #[test]
    fn shadow_type_with_var() {
        err(
            "fun test() { let String = 3; }",
            pos(1, 14),
            SemError::ShadowClass("String".into()),
        );
    }

    #[test]
    fn shadow_function() {
        ok("fun f() { let f = 1; }");
        err(
            "fun f() { let f = 1; f(); }",
            pos(1, 23),
            SemError::UnknownMethod("Int".into(), "get".into(), Vec::new()),
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
            SemError::ShadowParam("a".into()),
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
            SemError::UnknownIdentifier("a".into()),
        );
        err(
            "fun f() { a; }",
            pos(1, 11),
            SemError::UnknownIdentifier("a".into()),
        );
    }

    #[test]
    fn undefined_function() {
        err(
            "fun f() { foo(); }",
            pos(1, 11),
            SemError::UnknownIdentifier("foo".into()),
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
            SemError::UnknownIdentifier("a".into()),
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
