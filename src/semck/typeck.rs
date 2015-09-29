use parser::ast::ctxt::Context;
use error::msg::Msg;

use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::Type::*;
use parser::ast::visit::Visitor;

use sym::*;
use sym::Sym::*;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>, ast: &'ast Ast) {
    DefCheck::new(ctxt).visit_ast(ast);

    if ctxt.diag.borrow().has_errors() { return; }

    TypeCheck::new(ctxt).visit_ast(ast);
}

struct DefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    current_fct: Option<NodeId>,
    current_id: Option<NodeId>,
    current_type: BuiltinType,
}

impl<'a, 'ast> DefCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> DefCheck<'a, 'ast> {
        DefCheck {
            ctxt: ctxt,
            current_fct: None,
            current_id: None,
            current_type: BuiltinType::Unit,
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for DefCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.current_fct = Some(f.id);

        for p in &f.params {
            self.visit_param(p);
        }

        if let Some(ref ty) = f.return_type {
            self.visit_type(ty);

            self.ctxt.function(self.current_fct.unwrap(), |fct| {
                fct.return_type = self.current_type;
            });
        }

        self.visit_stmt(&f.block);
    }

    fn visit_param(&mut self, p: &'ast Param) {
        self.visit_type(&p.data_type);
        self.ctxt.var(p.id, |var| { var.data_type = self.current_type; });

        self.ctxt.function(self.current_fct.unwrap(), |fct| {
            fct.params_types.push(self.current_type);
        });
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtVar(ref var) = *s {
            if let Some(ref data_type) = var.data_type {
                self.visit_type(data_type);
                self.ctxt.var(var.id, |var| { var.data_type = self.current_type; });
            }

            if let Some(ref expr) = var.expr {
                visit::walk_expr(self, expr);
            }

        } else {
            visit::walk_stmt(self, s);
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        match *t {
            TypeBasic(ref basic) => {
                if let Some(builtin) = self.ctxt.sym.borrow().get_type(basic.name) {
                    self.current_type = builtin;
                } else {
                    let tyname = self.ctxt.interner.str(basic.name).to_string();
                    let msg = Msg::UnknownType(tyname);
                    self.ctxt.diag.borrow_mut().report(basic.pos, msg);
                }
            }

            _ => self.ctxt.diag.borrow_mut().report_unimplemented(t.pos())
        }
    }
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    expr_type: BuiltinType,
    fct: Option<&'ast Function>,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>) -> TypeCheck<'a, 'ast> {
        TypeCheck {
            ctxt: ctxt,
            expr_type: BuiltinType::Unit,
            fct: None
        }
    }

    fn check_stmt_var(&mut self, s: &'ast StmtVarType) {
        let expr_type = s.expr.as_ref().map(|expr| {
            self.visit_expr(&expr);
            self.expr_type
        });

        let defined_type = if let Some(_) = s.data_type {
            let ty = self.ctxt.var(s.id, |var| var.data_type);
            if ty == BuiltinType::Unit { None } else { Some(ty) }
        } else {
            expr_type
        };

        let defined_type = match defined_type {
            Some(ty) => ty,
            None => {
                let tyname = self.ctxt.interner.str(s.name).to_string();
                self.ctxt.diag.borrow_mut().report(s.pos, Msg::VarNeedsTypeInfo(tyname));

                return;
            }
        };

        // update type of variable, necessary when variable is only initialized with
        // an expression
        self.ctxt.var(s.id, |var| { var.data_type = defined_type });

        if expr_type.is_some() && (defined_type != expr_type.unwrap()) {
            let varname = self.ctxt.interner.str(s.name).to_string();
            let expr_type = expr_type.unwrap();
            let msg = Msg::AssignType(varname, defined_type, expr_type);

            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }
    }

    fn check_stmt_while(&mut self, s: &'ast StmtWhileType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let tyname = self.expr_type.to_string();
            let msg = Msg::WhileCondType(tyname);

            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_if(&mut self, s: &'ast StmtIfType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let tyname = self.expr_type.to_string();
            let msg = Msg::IfCondType(tyname);

            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            self.visit_stmt(else_block);
        }
    }

    fn check_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let expr_type = s.expr.as_ref().map(|expr| {
            self.visit_expr(&expr);

            self.expr_type
        }).unwrap_or(BuiltinType::Unit);

        let fct = self.fct.unwrap();
        let fct_type = self.ctxt.function(fct.id, |fct| fct.return_type);

        if expr_type != fct_type {
            let msg = Msg::ReturnType(fct_type.to_string(), expr_type.to_string());
            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        self.expr_type = self.ctxt.var(e.id, |var| var.data_type);
    }

    fn check_expr_assign(&mut self, e: &'ast ExprAssignType) {
        if !e.lhs.is_ident() {
            self.ctxt.diag.borrow_mut().report(e.pos, Msg::LvalueExpected);
            return;
        }

        self.visit_expr(&e.lhs);
        let lhs_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        if lhs_type != rhs_type {
            let ident = e.lhs.to_ident().unwrap();
            let ident = self.ctxt.interner.str(ident.name).to_string();
            let msg = Msg::AssignType(ident, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.expr_type = lhs_type;
    }

    fn check_expr_un(&mut self, e: &'ast ExprUnType) {
        let expected_type = if e.op == UnOp::Not {
            BuiltinType::Bool
        } else {
            BuiltinType::Int
        };

        self.visit_expr(&e.opnd);
        let opnd_type = self.expr_type;

        if expected_type != opnd_type {
            let op = e.op.to_string();
            let msg = Msg::UnOpType(op, opnd_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_bin(&mut self, e: &'ast ExprBinType) {
        let expected_type = if e.op == BinOp::Or || e.op == BinOp::And {
            BuiltinType::Bool
        } else {
            BuiltinType::Int
        };

        self.visit_expr(&e.lhs);
        let lhs_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        if expected_type != lhs_type || expected_type != rhs_type {
            let op = e.op.as_str().into();
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.expr_type = match e.op {
            BinOp::Or => BuiltinType::Bool,
            BinOp::And => BuiltinType::Bool,
            BinOp::Cmp(_) => BuiltinType::Bool,
            _ => BuiltinType::Int,
        };
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType) {
        let calls = self.ctxt.calls.borrow();
        let fct_id = *calls.get(&e.id).unwrap();

        let fct_infos = self.ctxt.fct_infos.borrow();
        let fct = &fct_infos[fct_id.0];
        self.expr_type = fct.return_type;

        let mut call_types = Vec::with_capacity(e.args.len());

        for arg in &e.args {
            self.visit_expr(arg);
            call_types.push(self.expr_type);
        }

        if fct.params_types != call_types {
            let fct_name = self.ctxt.interner.str(fct.name).to_string();
            let msg = Msg::ParamTypesIncompatible(fct_name, fct.params_types.clone(), call_types);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.fct = Some(f);

        visit::walk_fct(self, f);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(_) => self.expr_type = BuiltinType::Int,
            ExprLitStr(_) => self.expr_type = BuiltinType::Str,
            ExprLitBool(_) => self.expr_type = BuiltinType::Bool,
            ExprIdent(ref expr) => self.check_expr_ident(expr),
            ExprAssign(ref expr) => self.check_expr_assign(expr),
            ExprUn(ref expr) => self.check_expr_un(expr),
            ExprBin(ref expr) => self.check_expr_bin(expr),
            ExprCall(ref expr) => self.check_expr_call(expr),
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtIf(ref stmt) => self.check_stmt_if(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(ref stmt) => visit::walk_stmt(self, s),
            StmtContinue(ref stmt) => visit::walk_stmt(self, s),
            StmtLoop(ref stmt) => visit::walk_stmt(self, s),
            StmtExpr(ref stmt) => visit::walk_stmt(self, s),
            StmtBlock(ref stmt) => visit::walk_stmt(self, s),
        }
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;
    use sym::BuiltinType;

    #[test]
    fn type_def_for_return_type() {
        ok("fn a() -> int { return 1; }");
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

    #[test]
    fn type_var_needs_expr_or_definition() {
        err("fn a() { var a; }", pos(1, 10), Msg::VarNeedsTypeInfo("a".into()));
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fn f() { var a : int = 1; }");
        ok("fn f() { var a : bool = false; }");
        ok("fn f() { var a : str = \"f\"; }");

        err("fn f() { var a : int = true; }",
            pos(1, 10), Msg::AssignType(
                "a".into(), BuiltinType::Int, BuiltinType::Bool));
        err("fn f() { var b : bool = 2; }",
            pos(1, 10), Msg::AssignType(
                "b".into(), BuiltinType::Bool, BuiltinType::Int));
    }

    #[test]
    fn type_while() {
        ok("fn x() { while true { } }");
        ok("fn x() { while false { } }");
        err("fn x() { while 2 { } }", pos(1, 10), Msg::WhileCondType("int".into()));
    }

    #[test]
    fn type_if() {
        ok("fn x() { if true { } }");
        ok("fn x() { if false { } }");
        err("fn x() { if 4 { } }", pos(1, 10), Msg::IfCondType("int".into()));
    }

    #[test]
    fn type_return_unit() {
        ok("fn f() { return; }");
        err("fn f() { return 1; }", pos(1, 10), Msg::ReturnType("()".into(), "int".into()));
    }

    #[test]
    fn type_return() {
        ok("fn f() -> int { var a = 1; return a; }");
        ok("fn f() -> int { return 1; }");
        err("fn f() -> int { return; }", pos(1, 17),
            Msg::ReturnType("int".into(), "()".into()));
    }

    #[test]
    fn type_variable() {
        ok("fn f(a: int) { var b: int = a; }");
    }

    #[test]
    fn type_assign() {
        ok("fn f(a: int) { a = 1; }");
        err("fn f(a: int) { a = true; }", pos(1, 18),
            Msg::AssignType("a".into(), BuiltinType::Int, BuiltinType::Bool));
    }

    #[test]
    fn type_assign_lvalue() {
        err("fn f() { 1 = 3; }", pos(1, 12), Msg::LvalueExpected);
    }

    #[test]
    fn type_un_op() {
        ok("fn f(a: int) { ~a; -a; +a; }");
        err("fn f(a: int) { !a; }", pos(1, 16),
            Msg::UnOpType("!".into(), BuiltinType::Int));

        err("fn f(a: bool) { ~a; }", pos(1, 17),
            Msg::UnOpType("~".into(), BuiltinType::Bool));
        err("fn f(a: bool) { -a; }", pos(1, 17),
            Msg::UnOpType("-".into(), BuiltinType::Bool));
        err("fn f(a: bool) { +a; }", pos(1, 17),
            Msg::UnOpType("+".into(), BuiltinType::Bool));
    }

    #[test]
    fn type_bin_op() {
        ok("fn f(a: int) { a+a; a-a; a*a; a/a; a%a; }");
        ok("fn f(a: int) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fn f(a: int) { a|a; a&a; a^a; }");
        ok("fn f(a: bool) { a||a; a&&a; }");

        err("fn f(a: bool) { a+a; }", pos(1, 18),
            Msg::BinOpType("+".into(), BuiltinType::Bool, BuiltinType::Bool));
        err("fn f(a: bool) { a^a; }", pos(1, 18),
            Msg::BinOpType("^".into(), BuiltinType::Bool, BuiltinType::Bool));
        err("fn f(a: int) { a||a; }", pos(1, 17),
            Msg::BinOpType("||".into(), BuiltinType::Int, BuiltinType::Int));
        err("fn f(a: int) { a&&a; }", pos(1, 17),
            Msg::BinOpType("&&".into(), BuiltinType::Int, BuiltinType::Int));
    }

    #[test]
    fn type_function_return_type() {
        ok("fn foo() -> int { return 1; }\nfn f() { var i: int = foo(); }");
        err("fn foo() -> int { return 1; }\nfn f() { var i: bool = foo(); }",
            pos(2, 10),
            Msg::AssignType("i".into(),
                BuiltinType::Bool, BuiltinType::Int));
    }

    #[test]
    fn type_function_params() {
        ok("fn foo() {}\nfn f() { foo(); }");
        ok("fn foo(a: int) {}\nfn f() { foo(1); }");
        ok("fn foo(a: int, b: bool) {}\nfn f() { foo(1, true); }");

        err("fn foo() {}\nfn f() { foo(1); }", pos(2, 10),
            Msg::ParamTypesIncompatible("foo".into(),
                vec![],
                vec![BuiltinType::Int]));
        err("fn foo(a: int) {}\nfn f() { foo(true); }", pos(2, 10),
            Msg::ParamTypesIncompatible("foo".into(),
                vec![BuiltinType::Int],
                vec![BuiltinType::Bool]));
        err("fn foo(a: int, b: bool) {}\nfn f() { foo(1, 2); }", pos(2, 10),
            Msg::ParamTypesIncompatible("foo".into(),
                vec![BuiltinType::Int, BuiltinType::Bool],
                vec![BuiltinType::Int, BuiltinType::Int]));
    }
}
