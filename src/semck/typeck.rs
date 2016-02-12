use ctxt::{Context, FctContext};
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::Type::*;
use ast::visit::Visitor;

use sym::*;
use sym::Sym::*;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'a, 'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if let Some(ast) = fct.ast {
            let mut typeck = TypeCheck {
                ctxt: ctxt,
                fct: &mut fct,
                ast: ast,
                expr_type: BuiltinType::Unit,
            };

            typeck.check();
        }
    }
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'a mut FctContext<'ast>,
    ast: &'ast Function,
    expr_type: BuiltinType,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    fn check_stmt_var(&mut self, s: &'ast StmtVarType) {
        let expr_type = s.expr.as_ref().map(|expr| {
            self.visit_expr(&expr);
            self.expr_type
        });

        let defined_type = if let Some(_) = s.data_type {
            let ty = self.fct.var_by_node_id(s.id).data_type;
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
        self.fct.var_by_node_id_mut(s.id).data_type = defined_type;

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
            let msg = Msg::WhileCondType(self.expr_type);
            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_if(&mut self, s: &'ast StmtIfType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let msg = Msg::IfCondType(self.expr_type);
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

        let fct_type = self.fct.return_type;

        if expr_type != fct_type {
            let msg = Msg::ReturnType(fct_type, expr_type);
            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        self.expr_type = self.fct.var_by_node_id(e.id).data_type;
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
            let op = e.op.as_str().to_string();
            let msg = Msg::UnOpType(op, opnd_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_bin(&mut self, e: &'ast ExprBinType) {
        self.visit_expr(&e.lhs);
        let lhs_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        let expected_type = match e.op {
            BinOp::Or | BinOp::And => BuiltinType::Bool,
            BinOp::Cmp(CmpOp::Is) | BinOp::Cmp(CmpOp::IsNot) => BuiltinType::Str,
            BinOp::Cmp(_) | BinOp::Add =>
                if lhs_type == BuiltinType::Str {
                    BuiltinType::Str
                } else {
                    BuiltinType::Int
                },
            _ => BuiltinType::Int
        };

        if expected_type != lhs_type || expected_type != rhs_type {
            let op = e.op.as_str().into();
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.expr_type = match e.op {
            BinOp::Cmp(_) => BuiltinType::Bool,
            _ => expected_type,
        };
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType) {
        let callee_id = *self.fct.calls.get(&e.id).unwrap();
        let caller_id = self.fct.id;

        let call_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        let callee_name;
        let callee_params;
        let callee_return;

        if callee_id == caller_id {
            callee_name = self.fct.name;
            callee_params = self.fct.params_types.clone();
            callee_return = self.fct.return_type;

        } else {
            let fct = self.ctxt.fcts[callee_id.0].clone();
            let callee = &mut fct.lock().unwrap();

            callee_name = callee.name;
            callee_params = callee.params_types.clone();
            callee_return = callee.return_type;
        }

        self.expr_type = callee_return;

        if callee_params != call_types {
            let callee_name = self.ctxt.interner.str(callee_name).to_string();
            let msg = Msg::ParamTypesIncompatible(callee_name, callee_params.clone(), call_types);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_prop(&mut self, e: &'ast ExprPropType) {
        self.visit_expr(&e.object);

        if let BuiltinType::Class(classid) = self.expr_type {
            let cls = self.ctxt.cls_by_id(classid);

            for prop in &cls.props {
                if prop.name == e.name {
                    self.expr_type = prop.ty;
                    return;
                }
            }
        }

        // property not found, report error
        let prop_name = self.ctxt.interner.str(e.name).to_string();
        let msg = Msg::UnknownProp(prop_name, self.expr_type);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);

        // we don't know the type of the property, just assume ()
        self.expr_type = BuiltinType::Unit;
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
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
            ExprProp(ref expr) => self.check_expr_prop(expr),
        }

        self.fct.types.insert(e.id(), self.expr_type);
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
    use class::ClassId;
    use error::msg::Msg;
    use semck::tests::*;
    use test::parse_with_errors;
    use ty::BuiltinType;

    #[test]
    fn type_object_prop() {
        ok("class Foo(a:int) fn f(x: Foo) -> int { return x.a; }");
        ok("class Foo(a:Str) fn f(x: Foo) -> Str { return x.a; }");
        err("class Foo(a:int) fn f(x: Foo) -> bool { return x.a; }",
             pos(1, 41), Msg::ReturnType(BuiltinType::Bool, BuiltinType::Int));

        parse_with_errors("class Foo(a:int) fn f(x: Foo) -> int { return x.b; }", |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();
            assert_eq!(2, errors.len());

            let err = &errors[0];
            assert_eq!(pos(1, 48), err.pos);
            assert_eq!(Msg::UnknownProp("b".into(), BuiltinType::Class(ClassId(0))), err.msg);

            let err = &errors[1];
            assert_eq!(pos(1, 40), err.pos);
            assert_eq!(Msg::ReturnType(BuiltinType::Int, BuiltinType::Unit), err.msg);
        });
    }

    // #[test]
    // fn type_ctor() {
    //     ok("class Foo fn f() -> Foo { return Foo(); }");
    //     ok("class Foo(a: int) fn f() -> Foo { return Foo(1); }");
    // }

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
        ok("fn f() { var a : Str = \"f\"; }");

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
        err("fn x() { while 2 { } }", pos(1, 10), Msg::WhileCondType(BuiltinType::Int));
    }

    #[test]
    fn type_if() {
        ok("fn x() { if true { } }");
        ok("fn x() { if false { } }");
        err("fn x() { if 4 { } }", pos(1, 10), Msg::IfCondType(BuiltinType::Int));
    }

    #[test]
    fn type_return_unit() {
        ok("fn f() { return; }");
        err("fn f() { return 1; }", pos(1, 10),
            Msg::ReturnType(BuiltinType::Unit, BuiltinType::Int));
    }

    #[test]
    fn type_return() {
        ok("fn f() -> int { var a = 1; return a; }");
        ok("fn f() -> int { return 1; }");
        err("fn f() -> int { return; }", pos(1, 17),
            Msg::ReturnType(BuiltinType::Int, BuiltinType::Unit));

        ok("fn f() -> int { return 0; }
            fn g() -> int { return f(); }");
        err("fn f() { }
             fn g() -> int { return f(); }", pos(2, 30),
             Msg::ReturnType(BuiltinType::Int, BuiltinType::Unit));
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
        ok("fn f(a: Str) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fn f(a: Str) { a===a; a!==a; a+a; }");
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
        err("fn f(a: int) { a===a; }", pos(1, 17),
            Msg::BinOpType("===".into(), BuiltinType::Int, BuiltinType::Int));
        err("fn f(a: int) { a!==a; }", pos(1, 17),
            Msg::BinOpType("!==".into(), BuiltinType::Int, BuiltinType::Int));
        err("fn f(a: bool) { a===a; }", pos(1, 18),
            Msg::BinOpType("===".into(), BuiltinType::Bool, BuiltinType::Bool));
        err("fn f(a: bool) { a!==a; }", pos(1, 18),
            Msg::BinOpType("!==".into(), BuiltinType::Bool, BuiltinType::Bool));
        err("fn f(a: Str) { a-a; }", pos(1, 17),
            Msg::BinOpType("-".into(), BuiltinType::Str, BuiltinType::Str));
        err("fn f(a: Str) { a*a; }", pos(1, 17),
            Msg::BinOpType("*".into(), BuiltinType::Str, BuiltinType::Str));
        err("fn f(a: Str) { a%a; }", pos(1, 17),
            Msg::BinOpType("%".into(), BuiltinType::Str, BuiltinType::Str));
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
    fn type_ident_in_function_params() {
        ok("fn f(a: int) {}\nfn g() { var a = 1; f(a); }");
    }

    #[test]
    fn type_recursive_function_call() {
        ok("fn f(a: int) { f(a); }");
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
