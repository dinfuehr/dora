use ctxt::{CallType, Context, Fct, IdentType};
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::Visitor;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if fct.kind.is_src() {
            let ast = fct.ast();
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
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    expr_type: BuiltinType,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    fn set_type(&mut self, id: NodeId, ty: BuiltinType) {
        let old_type = self.fct.src_mut().types.insert(id, ty);
        assert!(old_type.is_none());

        self.expr_type = ty;
    }

    fn check_stmt_let(&mut self, s: &'ast StmtLetType) {
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
        let ident_type = *self.fct.src().defs.get(&e.id).unwrap();

        match ident_type {
            IdentType::Var(varid) => {
                let ty = self.fct.var_by_node_id(e.id).data_type;
                self.set_type(e.id, ty);
            }

            IdentType::Prop(clsid, propid) => {
                let cls = self.ctxt.cls_by_id(clsid);
                let prop = &cls.props[propid.0];

                self.set_type(e.id, prop.ty);
            }
        }
    }

    fn check_expr_assign(&mut self, e: &'ast ExprAssignType) {
        if !e.lhs.is_ident() && !e.lhs.is_prop() {
            self.ctxt.diag.borrow_mut().report(e.pos, Msg::LvalueExpected);
            return;
        }

        self.visit_expr(&e.lhs);
        let lhs_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        if lhs_type != rhs_type {
            let msg = if e.lhs.is_ident() {
                let ident = e.lhs.to_ident().unwrap();
                let name = self.ctxt.interner.str(ident.name).to_string();

                Msg::AssignType(name, lhs_type, rhs_type)
            } else {
                let prop = e.lhs.to_prop().unwrap();
                let prop_type = self.fct.src().get_type(prop.object.id());

                Msg::AssignProp(prop.name, prop_type.cls_id(), lhs_type, rhs_type)
            };

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.set_type(e.id, lhs_type);
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

        match e.op {
            BinOp::Or | BinOp::And => self.check_expr_bin_bool(e, e.op, lhs_type, rhs_type),
            BinOp::Cmp(cmp) => self.check_expr_bin_cmp(e, cmp, lhs_type, rhs_type),
            BinOp::Add => self.check_expr_bin_add(e, e.op, lhs_type, rhs_type),
            _ => self.check_expr_bin_int(e, e.op, lhs_type, rhs_type),
        }
    }

    fn check_expr_bin_bool(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                           rhs_type: BuiltinType) {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Bool);
        self.set_type(e.id, BuiltinType::Bool);
    }

    fn check_expr_bin_int(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Int);
        self.set_type(e.id, BuiltinType::Int);
    }

    fn check_expr_bin_add(&mut self,  e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        if lhs_type == BuiltinType::Str {
            self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Str);
            self.set_type(e.id, BuiltinType::Str);
        } else {
            self.check_expr_bin_int(e, op, lhs_type, rhs_type);
        }
    }

    fn check_expr_bin_cmp(&mut self,  e: &'ast ExprBinType, cmp: CmpOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        let expected_type = match cmp {
            CmpOp::Is | CmpOp::IsNot => match lhs_type {
                BuiltinType::Str | BuiltinType::Class(_) => lhs_type,
                _ => BuiltinType::Ptr,
            },

            _ => if lhs_type == BuiltinType::Str {
                    lhs_type
                } else {
                    BuiltinType::Int
                }
        };

        self.check_type(e, BinOp::Cmp(cmp), lhs_type, rhs_type, expected_type);
        self.set_type(e.id, BuiltinType::Bool);
    }

    fn check_type(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                  rhs_type: BuiltinType, expected_type: BuiltinType) {
        if expected_type != lhs_type || expected_type != rhs_type {
            let op = op.as_str().into();
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType) {
        if e.with_self {
            self.check_method_call(e);
            return;
        }

        let call_type = *self.fct.src().calls.get(&e.id).unwrap();
        let caller_id = self.fct.id;

        let call_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        match call_type {
            CallType::Ctor(cls_id, _) => {
                self.set_type(e.id, BuiltinType::Class(cls_id));

                let cls = self.ctxt.cls_by_id(cls_id);
                let mut found = false;

                for ctor in &cls.ctors {
                    let ctor = *ctor;

                    let params_types = if self.fct.id == ctor {
                        self.fct.params_types.clone()
                    } else {
                        self.ctxt.fct_by_id(ctor, |fct| fct.params_types.clone())
                    };

                    if call_types == params_types {
                        let call_type = CallType::Ctor(cls_id, ctor);
                        assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_some());

                        found = true;
                        break;
                    }
                }

                if !found {
                    let msg = Msg::UnknownCtor(cls.name, call_types.clone());
                    self.ctxt.diag.borrow_mut().report(e.pos, msg);
                }
            }

            CallType::Fct(callee_id) => {
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

                self.set_type(e.id, callee_return);

                if callee_params != call_types {
                    let msg = Msg::ParamTypesIncompatible(callee_name, callee_params.clone(), call_types);
                    self.ctxt.diag.borrow_mut().report(e.pos, msg);
                }
            }

            _ => panic!("invocation of method")
        }
    }

    fn check_method_call(&mut self, e: &'ast ExprCallType) {
        let caller_id = self.fct.id;

        let call_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        let object_type = call_types[0];
        let call_types = &call_types[1..];

        if let BuiltinType::Class(clsid) = object_type {
            let cls = self.ctxt.cls_by_id(clsid);

            for method in &cls.methods {
                let method = *method;

                if self.fct.id == method {
                    if self.fct.name == e.name && self.fct.params_types == call_types {
                        let ty = self.fct.return_type;
                        self.set_type(e.id, ty);

                        let call_type = CallType::Method(clsid, method);
                        assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_none());
                        return;
                    } else {
                        continue;
                    }
                }

                let fct = self.ctxt.fcts[method.0].clone();
                let callee = &mut fct.lock().unwrap();

                if callee.name == e.name && callee.params_types == call_types {
                    self.set_type(e.id, callee.return_type);

                    let call_type = CallType::Method(clsid, callee.id);
                    assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_none());
                    return;
                }
            }
        }

        let call_types = call_types.iter().cloned().collect::<Vec<_>>();
        let msg = Msg::UnknownMethod(object_type, e.name, call_types);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        self.set_type(e.id, BuiltinType::Unit);
    }

    fn check_expr_prop(&mut self, e: &'ast ExprPropType) {
        self.visit_expr(&e.object);

        if let BuiltinType::Class(classid) = self.expr_type {
            let cls = self.ctxt.cls_by_id(classid);

            for prop in &cls.props {
                if prop.name == e.name {
                    let ident_type = IdentType::Prop(classid, prop.id);
                    assert!(self.fct.src_mut().defs.insert(e.id, ident_type).is_none());

                    self.set_type(e.id, prop.ty);
                    return;
                }
            }
        }

        // property not found, report error
        let prop_name = self.ctxt.interner.str(e.name).to_string();
        let msg = Msg::UnknownProp(prop_name, self.expr_type);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        // we don't know the type of the property, just assume ()
        self.set_type(e.id, BuiltinType::Unit);
    }

    fn check_expr_self(&mut self, e: &'ast ExprSelfType) {
        if let Some(clsid) = self.fct.owner_class {
            self.set_type(e.id, BuiltinType::Class(clsid));

        } else {
            let msg = Msg::SelfUnavailable;
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
            self.set_type(e.id, BuiltinType::Unit);
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.set_type(expr.id, BuiltinType::Int),
            ExprLitStr(ref expr) => self.set_type(expr.id, BuiltinType::Str),
            ExprLitBool(ref expr) => self.set_type(expr.id, BuiltinType::Bool),
            ExprIdent(ref expr) => self.check_expr_ident(expr),
            ExprAssign(ref expr) => self.check_expr_assign(expr),
            ExprUn(ref expr) => self.check_expr_un(expr),
            ExprBin(ref expr) => self.check_expr_bin(expr),
            ExprCall(ref expr) => self.check_expr_call(expr),
            ExprProp(ref expr) => self.check_expr_prop(expr),
            ExprSelf(ref expr) => self.check_expr_self(expr),
            ExprNil(ref expr) => panic!("not implemented"),
        }

        self.fct.src_mut().types.insert(e.id(), self.expr_type);
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtLet(ref stmt) => self.check_stmt_let(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtIf(ref stmt) => self.check_stmt_if(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(_) => visit::walk_stmt(self, s),
            StmtContinue(_) => visit::walk_stmt(self, s),
            StmtLoop(_) => visit::walk_stmt(self, s),
            StmtExpr(_) => visit::walk_stmt(self, s),
            StmtBlock(_) => visit::walk_stmt(self, s),
        }
    }
}

#[cfg(test)]
mod tests {
    use class::ClassId;
    use error::msg::Msg;
    use interner::Name;
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

    #[test]
    fn type_object_set_prop() {
        ok("class Foo(a: int) fn f(x: Foo) { x.a = 1; }");
        err("class Foo(a: int) fn f(x: Foo) { x.a = false; }",
            pos(1, 38),
            Msg::AssignProp(Name(1), ClassId(0), BuiltinType::Int, BuiltinType::Bool));
    }

    #[test]
    fn type_object_prop_without_self() {
        ok("class Foo(a: int) { fn f() -> int { return a; } }");
        ok("class Foo(a: int) { fn set(x: int) { a = x; } }");
        err("class Foo(a: int) { fn set(x: int) { b = x; } }",
            pos(1, 38), Msg::UnknownIdentifier(Name(5)));
    }

    #[test]
    fn type_method_call() {
        ok("class Foo {
                fn bar() {}
                fn baz() -> int { return 1; }
            }

            fn f(x: Foo) { x.bar(); }
            fn g(x: Foo) -> int { return x.baz(); }");

        err("class Foo {
                 fn bar() -> int { return 0; }
             }

             fn f(x: Foo) -> Str { return x.bar(); }",
             pos(5, 36), Msg::ReturnType(BuiltinType::Str, BuiltinType::Int));
    }

    #[test]
    fn type_method_defined_twice() {
        err("class Foo {
                 fn bar() {}
                 fn bar() {}
             }", pos(3, 18), Msg::MethodExists(
                 BuiltinType::Class(ClassId(0)), Name(1), vec![], pos(2, 18)));

        err("class Foo {
                 fn bar() {}
                 fn bar() -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 BuiltinType::Class(ClassId(0)), Name(1), vec![], pos(2, 18)));

        err("class Foo {
                 fn bar(a: int) {}
                 fn bar(a: int) -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 BuiltinType::Class(ClassId(0)), Name(1), vec![BuiltinType::Int], pos(2, 18)));

        ok("class Foo {
                fn bar(a: int) {}
                fn bar(a: Str) {}
            }");
    }

    #[test]
    fn type_self() {
        ok("class Foo { fn me() -> Foo { return self; } }");
        err("class Foo fn me() { return self; }",
            pos(1, 28), Msg::SelfUnavailable);

        ok("class Foo(a: int, b: int) {
            fn bar() -> int { return self.a + self.b; }
        }");

        ok("class Foo(a: int) {
            fn setA(a: int) { self.a = a; }
        }");

        ok("class Foo {
            fn zero() -> int { return 0; }
            fn other() -> int { return self.zero(); }
        }");

        ok("class Foo {
            fn bar() { self.bar(); }
        }");
    }

    #[test]
    fn type_unknown_method() {
        err("class Foo {
                 fn bar(a: int) { }
             }

             fn f(x: Foo) { x.bar(); }",
             pos(5, 30),
             Msg::UnknownMethod(BuiltinType::Class(ClassId(0)), Name(1), Vec::new()));

         err("class Foo { }
              fn f(x: Foo) { x.bar(1); }",
              pos(2, 31),
              Msg::UnknownMethod(BuiltinType::Class(ClassId(0)),
                Name(3), vec![BuiltinType::Int]));
    }

    #[test]
    fn type_ctor() {
        ok("class Foo fn f() -> Foo { return Foo(); }");
        ok("class Foo(a: int) fn f() -> Foo { return Foo(1); }");
        err("class Foo fn f() -> Foo { return 1; }", pos(1, 27),
            Msg::ReturnType(BuiltinType::Class(ClassId(0)), BuiltinType::Int));
    }

    #[test]
    fn type_def_for_return_type() {
        ok("fn a() -> int { return 1; }");
        err("fn a() -> unknown {}", pos(1, 11), Msg::UnknownType(Name(1)));
    }

    #[test]
    fn type_def_for_param() {
        ok("fn a(b: int) {}");
        err("fn a(b: foo) {}", pos(1, 9), Msg::UnknownType(Name(2)));
    }

    #[test]
    fn type_def_for_var() {
        ok("fn a() { let a : int = 1; }");
        err("fn a() { let a : test = 1; }", pos(1, 18), Msg::UnknownType(Name(1)));
    }

    #[test]
    fn type_var_needs_expr_or_definition() {
        err("fn a() { let a; }", pos(1, 10), Msg::VarNeedsTypeInfo("a".into()));
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fn f() { let a : int = 1; }");
        ok("fn f() { let a : bool = false; }");
        ok("fn f() { let a : Str = \"f\"; }");

        err("fn f() { let a : int = true; }",
            pos(1, 10), Msg::AssignType(
                "a".into(), BuiltinType::Int, BuiltinType::Bool));
        err("fn f() { let b : bool = 2; }",
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
        ok("fn f() -> int { let a = 1; return a; }");
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
        ok("fn f(a: int) { let b: int = a; }");
    }

    #[test]
    fn type_assign() {
        ok("fn f(mut a: int) { a = 1; }");
        err("fn f(mut a: int) { a = true; }", pos(1, 22),
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
        ok("class Foo fn f(a: Foo) { a===a; a!==a; }");
        ok("fn f(a: int) { a|a; a&a; a^a; }");
        ok("fn f(a: bool) { a||a; a&&a; }");

        err("class A class B fn f(a: A, b: B) { a === b; }", pos(1, 38),
            Msg::BinOpType("===".into(),
                BuiltinType::Class(ClassId(0)), BuiltinType::Class(ClassId(1))));
        err("class A class B fn f(a: A, b: B) { b !== a; }", pos(1, 38),
            Msg::BinOpType("!==".into(),
                BuiltinType::Class(ClassId(1)), BuiltinType::Class(ClassId(0))));
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
        ok("fn foo() -> int { return 1; }\nfn f() { let i: int = foo(); }");
        err("fn foo() -> int { return 1; }\nfn f() { let i: bool = foo(); }",
            pos(2, 10),
            Msg::AssignType("i".into(),
                BuiltinType::Bool, BuiltinType::Int));
    }

    #[test]
    fn type_ident_in_function_params() {
        ok("fn f(a: int) {}\nfn g() { let a = 1; f(a); }");
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
            Msg::ParamTypesIncompatible(Name(0),
                vec![],
                vec![BuiltinType::Int]));
        err("fn foo(a: int) {}\nfn f() { foo(true); }", pos(2, 10),
            Msg::ParamTypesIncompatible(Name(0),
                vec![BuiltinType::Int],
                vec![BuiltinType::Bool]));
        err("fn foo(a: int, b: bool) {}\nfn f() { foo(1, 2); }", pos(2, 10),
            Msg::ParamTypesIncompatible(Name(0),
                vec![BuiltinType::Int, BuiltinType::Bool],
                vec![BuiltinType::Int, BuiltinType::Int]));
    }
}
