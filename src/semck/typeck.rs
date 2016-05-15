use ctxt::{CallType, Context, Fct, FctId, IdentType};
use class::ClassId;
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::Visitor;
use interner::Name;
use lexer::position::Position;
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
            let ty = self.fct.var(s.var()).ty;
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
        self.fct.var_mut(s.var()).ty = defined_type;

        if let Some(expr_type) = expr_type {
            if !defined_type.allows(expr_type) {
                let name = self.ctxt.interner.str(s.name).to_string();
                let defined_type = defined_type.name(self.ctxt);
                let expr_type = expr_type.name(self.ctxt);
                let msg = Msg::AssignType(name, defined_type, expr_type);
                self.ctxt.diag.borrow_mut().report(s.pos, msg);
            }
        }
    }

    fn check_stmt_while(&mut self, s: &'ast StmtWhileType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let expr_type = self.expr_type.name(self.ctxt);
            let msg = Msg::WhileCondType(expr_type);
            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_if(&mut self, s: &'ast StmtIfType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let expr_type = self.expr_type.name(self.ctxt);
            let msg = Msg::IfCondType(expr_type);
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

        if !fct_type.allows(expr_type) {
            let msg = if expr_type.is_nil() {
                let fct_type = fct_type.name(self.ctxt);

                Msg::IncompatibleWithNil(fct_type)
            } else {
                let fct_type = fct_type.name(self.ctxt);
                let expr_type = expr_type.name(self.ctxt);

                Msg::ReturnType(fct_type, expr_type)
            };

            self.ctxt.diag.borrow_mut().report(s.pos, msg);
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        match e.ident_type() {
            IdentType::Var(varid) => {
                let ty = self.fct.var(varid).ty;
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
        if e.lhs.is_array() {
            let array = e.lhs.to_array().unwrap();

            self.visit_expr(&array.object);
            let object_type = self.expr_type;

            self.visit_expr(&array.index);
            let index_type = self.expr_type;

            self.visit_expr(&e.rhs);
            let value_type = self.expr_type;

            let name = self.ctxt.interner.intern("set");
            let args = vec![object_type, index_type, value_type];
            let ret_type = Some(BuiltinType::Unit);

            if let Some((cls_id, fct_id, _)) = self.find_method(e.id, e.pos, object_type, name,
                                                                  &args, ret_type) {
                let call_type = CallType::Method(cls_id, fct_id);
                assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_none());

                let index_type = if self.fct.id == fct_id {
                    self.fct.params_types[1]
                } else {
                    self.ctxt.fct_by_id(fct_id, |fct| fct.params_types[1])
                };

                self.set_type(e.id, index_type);
            }

        } else if e.lhs.is_prop() || e.lhs.is_ident() {
            self.visit_expr(&e.lhs);
            let lhs_type = self.expr_type;

            self.visit_expr(&e.rhs);
            let rhs_type = self.expr_type;

            if !lhs_type.allows(rhs_type) {
                let msg = if e.lhs.is_ident() {
                    let ident = e.lhs.to_ident().unwrap();
                    let name = self.ctxt.interner.str(ident.name).to_string();
                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);

                    Msg::AssignType(name, lhs_type, rhs_type)
                } else {
                    let prop = e.lhs.to_prop().unwrap();
                    let name = self.ctxt.interner.str(prop.name).to_string();

                    let prop_type = self.fct.src().get_type(prop.object.id());
                    let prop_type = prop_type.name(self.ctxt);

                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);

                    Msg::AssignProp(name, prop_type, lhs_type, rhs_type)
                };

                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }

            self.set_type(e.id, lhs_type);

        } else {
            self.ctxt.diag.borrow_mut().report(e.pos, Msg::LvalueExpected);
            self.set_type(e.id, BuiltinType::Unit);
        }
    }

    fn find_method(&mut self, id: NodeId, pos: Position, object_type: BuiltinType,
                   name: Name, args: &[BuiltinType],
                   return_type: Option<BuiltinType>) -> Option<(ClassId, FctId, BuiltinType)> {
        let cls_id = match object_type {
            BuiltinType::Class(cls_id) => Some(cls_id),
            _ => self.ctxt.primitive_classes.find_class(object_type)
        };

        if let Some(cls_id) = cls_id {
            let cls = self.ctxt.cls_by_id(cls_id);
            let mut candidates = Vec::new();

            for method in &cls.methods {
                let method = *method;

                if self.fct.id == method {
                    if self.fct.name == name
                        && args_compatible(&self.fct.params_types, args)
                        && (return_type.is_none()
                         || self.fct.return_type == return_type.unwrap()) {
                        candidates.push((cls_id, method, self.fct.return_type));
                    }
                } else {
                    self.ctxt.fct_by_id(method, |callee| {
                        if callee.name == name
                            && args_compatible(&callee.params_types, args)
                            && (return_type.is_none()
                             || callee.return_type == return_type.unwrap()) {
                            candidates.push((cls_id, method, callee.return_type));
                        }
                    });
                }
            }

            if candidates.len() == 1 {
                let candidate = candidates[0];
                return Some(candidate);

            } else if candidates.len() > 1 {
                let object_type = object_type.name(self.ctxt);
                let args = args[1..].iter().map(|a| a.name(self.ctxt)).collect::<Vec<String>>();
                let name = self.ctxt.interner.str(name).to_string();
                let msg = Msg::MultipleCandidates(object_type, name, args);
                self.ctxt.diag.borrow_mut().report(pos, msg);
                self.set_type(id, BuiltinType::Unit);
                return None;
            }
        }

        let type_name = object_type.name(self.ctxt);
        let name = self.ctxt.interner.str(name).to_string();
        let param_names = args[1..].iter().map(|a| a.name(self.ctxt)).collect::<Vec<String>>();
        let msg = Msg::UnknownMethod(type_name, name, param_names);
        self.ctxt.diag.borrow_mut().report(pos, msg);
        self.set_type(id, BuiltinType::Unit);

        None
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
            let opnd_type = opnd_type.name(self.ctxt);
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
        let ty = lhs_type.if_nil(rhs_type);

        let expected_type = match cmp {
            CmpOp::Is | CmpOp::IsNot => {
                match ty {
                    BuiltinType::Str
                    | BuiltinType::Class(_)
                    | BuiltinType::Nil => ty,
                    _ => {
                        let op = cmp.as_str().into();
                        let lhs_type = lhs_type.name(self.ctxt);
                        let rhs_type = rhs_type.name(self.ctxt);
                        let msg = Msg::BinOpType(op, lhs_type, rhs_type);

                        self.ctxt.diag.borrow_mut().report(e.pos, msg);
                        self.set_type(e.id, BuiltinType::Bool);
                        return;
                    }
                }
            },

            CmpOp::Eq | CmpOp::Ne => {
                match ty {
                    BuiltinType::Str
                    | BuiltinType::Bool => ty,
                    _ => BuiltinType::Int
                }
            }

            _ => if ty == BuiltinType::Str {
                    ty
                } else {
                    BuiltinType::Int
                }
        };

        self.check_type(e, BinOp::Cmp(cmp), lhs_type, rhs_type, expected_type);
        self.set_type(e.id, BuiltinType::Bool);
    }

    fn check_type(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                  rhs_type: BuiltinType, expected_type: BuiltinType) {
        if !expected_type.allows(lhs_type) || !expected_type.allows(rhs_type) {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name(self.ctxt);
            let rhs_type = rhs_type.name(self.ctxt);
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
                let cls = self.ctxt.cls_by_id(cls_id);
                self.set_type(e.id, cls.ty);
                let mut found = false;

                for ctor in &cls.ctors {
                    let ctor = *ctor;

                    let params_types = if self.fct.id == ctor {
                        self.fct.params_types.clone()
                    } else {
                        self.ctxt.fct_by_id(ctor, |fct| fct.params_types.clone())
                    };

                    if args_compatible(&params_types, &call_types) {
                        let call_type = CallType::Ctor(cls_id, ctor);
                        assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_some());

                        found = true;
                        break;
                    }
                }

                if !found {
                    let call_types = call_types.iter()
                        .map(|a| a.name(self.ctxt))
                        .collect::<Vec<_>>();
                    let name = self.ctxt.interner.str(cls.name).to_string();
                    let msg = Msg::UnknownCtor(name, call_types);
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

                if !args_compatible(&callee_params, &call_types) {
                    let callee_name = self.ctxt.interner.str(callee_name).to_string();
                    let callee_params = callee_params.iter()
                        .map(|a| a.name(self.ctxt))
                        .collect::<Vec<_>>();
                    let call_types = call_types.iter()
                        .map(|a| a.name(self.ctxt))
                        .collect::<Vec<_>>();
                    let msg = Msg::ParamTypesIncompatible(callee_name, callee_params, call_types);
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

        if let Some((cls_id, fct_id, return_type)) = self.find_method(e.id, e.pos, object_type,
                                                            e.name, &call_types, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_none());
            self.set_type(e.id, return_type);
        }
    }

    fn check_expr_prop(&mut self, e: &'ast ExprPropType) {
        self.visit_expr(&e.object);

        if let BuiltinType::Class(classid) = self.expr_type {
            let cls = self.ctxt.cls_by_id(classid);

            for prop in &cls.props {
                if prop.name == e.name {
                    e.set_cls_and_field(classid, prop.id);
                    self.set_type(e.id, prop.ty);
                    return;
                }
            }
        }

        // property not found, report error
        let prop_name = self.ctxt.interner.str(e.name).to_string();
        let expr_name = self.expr_type.name(self.ctxt);
        let msg = Msg::UnknownProp(prop_name, expr_name);
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

    fn check_expr_nil(&mut self, e: &'ast ExprNilType) {
        self.set_type(e.id, BuiltinType::Nil);
    }

    fn check_expr_array(&mut self, e: &'ast ExprArrayType) {
        self.visit_expr(&e.object);
        let object_type = self.expr_type;

        self.visit_expr(&e.index);
        let index_type = self.expr_type;

        let name = self.ctxt.interner.intern("get");
        let args = vec![object_type, index_type];

        if let Some((cls_id, fct_id, return_type)) = self.find_method(e.id, e.pos, object_type,
                                                            name, &args, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            assert!(self.fct.src_mut().calls.insert(e.id, call_type).is_none());

            self.set_type(e.id, return_type);
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
            ExprNil(ref expr) => self.check_expr_nil(expr),
            ExprArray(ref expr) => self.check_expr_array(expr),
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

fn args_compatible(def: &[BuiltinType], expr: &[BuiltinType]) -> bool {
    if def.len() != expr.len() {
        return false;
    }

    for (ind, arg) in def.iter().enumerate() {
        if !arg.allows(expr[ind]) {
            return false;
        }
    }

    true
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
    fn type_method_len() {
        ok("fn f(a: Str) -> int { return a.len(); }");
        ok("fn f(a: Str) -> int { return \"abc\".len(); }");
    }

    #[test]
    fn type_object_prop() {
        ok("class Foo(a:int) fn f(x: Foo) -> int { return x.a; }");
        ok("class Foo(a:Str) fn f(x: Foo) -> Str { return x.a; }");
        err("class Foo(a:int) fn f(x: Foo) -> bool { return x.a; }",
             pos(1, 41), Msg::ReturnType("bool".into(), "int".into()));

        parse_with_errors("class Foo(a:int) fn f(x: Foo) -> int { return x.b; }", |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();
            assert_eq!(2, errors.len());

            let err = &errors[0];
            assert_eq!(pos(1, 48), err.pos);
            assert_eq!(Msg::UnknownProp("b".into(), "Foo".into()), err.msg);

            let err = &errors[1];
            assert_eq!(pos(1, 40), err.pos);
            assert_eq!(Msg::ReturnType("int".into(), "()".into()), err.msg);
        });
    }

    #[test]
    fn type_object_set_prop() {
        ok("class Foo(a: int) fn f(x: Foo) { x.a = 1; }");
        err("class Foo(a: int) fn f(x: Foo) { x.a = false; }",
            pos(1, 38),
            Msg::AssignProp("a".into(), "Foo".into(), "int".into(), "bool".into()));
    }

    #[test]
    fn type_object_prop_without_self() {
        ok("class Foo(a: int) { fn f(self) -> int { return a; } }");
        ok("class Foo(a: int) { fn set(self, x: int) { a = x; } }");
        err("class Foo(a: int) { fn set(self, x: int) { b = x; } }",
            pos(1, 44), Msg::UnknownIdentifier("b".into()));
    }

    #[test]
    fn type_method_call() {
        ok("class Foo {
                fn bar(self) {}
                fn baz(self) -> int { return 1; }
            }

            fn f(x: Foo) { x.bar(); }
            fn g(x: Foo) -> int { return x.baz(); }");

        err("class Foo {
                 fn bar(self) -> int { return 0; }
             }

             fn f(x: Foo) -> Str { return x.bar(); }",
             pos(5, 36), Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_method_defined_twice() {
        err("class Foo {
                 fn bar(self) {}
                 fn bar(self) {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec![], pos(2, 18)));

        err("class Foo {
                 fn bar(self) {}
                 fn bar(self) -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec![], pos(2, 18)));

        err("class Foo {
                 fn bar(self, a: int) {}
                 fn bar(self, a: int) -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec!["int".into()], pos(2, 18)));

        ok("class Foo {
                fn bar(self, a: int) {}
                fn bar(self, a: Str) {}
            }");
    }

    #[test]
    fn type_self() {
        ok("class Foo { fn me(self) -> Foo { return self; } }");
        err("class Foo fn me() { return self; }",
            pos(1, 28), Msg::SelfUnavailable);

        ok("class Foo(a: int, b: int) {
            fn bar(self) -> int { return self.a + self.b; }
        }");

        ok("class Foo(a: int) {
            fn setA(self, a: int) { self.a = a; }
        }");

        ok("class Foo {
            fn zero(self) -> int { return 0; }
            fn other(self) -> int { return self.zero(); }
        }");

        ok("class Foo {
            fn bar(self) { self.bar(); }
        }");
    }

    #[test]
    fn type_unknown_method() {
        err("class Foo {
                 fn bar(self, a: int) { }
             }

             fn f(x: Foo) { x.bar(); }",
             pos(5, 30),
             Msg::UnknownMethod("Foo".into(), "bar".into(), Vec::new()));

         err("class Foo { }
              fn f(x: Foo) { x.bar(1); }",
              pos(2, 31),
              Msg::UnknownMethod("Foo".into(),
                "bar".into(), vec!["int".into()]));
    }

    #[test]
    fn type_ctor() {
        ok("class Foo fn f() -> Foo { return Foo(); }");
        ok("class Foo(a: int) fn f() -> Foo { return Foo(1); }");
        err("class Foo fn f() -> Foo { return 1; }", pos(1, 27),
            Msg::ReturnType("Foo".into(), "int".into()));
    }

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
        ok("fn a() { let a : int = 1; }");
        err("fn a() { let a : test = 1; }", pos(1, 18), Msg::UnknownType("test".into()));
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
                "a".into(), "int".into(), "bool".into()));
        err("fn f() { let b : bool = 2; }",
            pos(1, 10), Msg::AssignType(
                "b".into(), "bool".into(), "int".into()));
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
        err("fn f() { return 1; }", pos(1, 10),
            Msg::ReturnType("()".into(), "int".into()));
    }

    #[test]
    fn type_return() {
        ok("fn f() -> int { let a = 1; return a; }");
        ok("fn f() -> int { return 1; }");
        err("fn f() -> int { return; }", pos(1, 17),
            Msg::ReturnType("int".into(), "()".into()));

        ok("fn f() -> int { return 0; }
            fn g() -> int { return f(); }");
        err("fn f() { }
             fn g() -> int { return f(); }", pos(2, 30),
             Msg::ReturnType("int".into(), "()".into()));
    }

    #[test]
    fn type_variable() {
        ok("fn f(a: int) { let b: int = a; }");
    }

    #[test]
    fn type_assign() {
        ok("fn f(a: int) { a = 1; }");
        err("fn f(a: int) { a = true; }", pos(1, 18),
            Msg::AssignType("a".into(), "int".into(), "bool".into()));
    }

    #[test]
    fn type_assign_lvalue() {
        err("fn f() { 1 = 3; }", pos(1, 12), Msg::LvalueExpected);
    }

    #[test]
    fn type_un_op() {
        ok("fn f(a: int) { ~a; -a; +a; }");
        err("fn f(a: int) { !a; }", pos(1, 16),
            Msg::UnOpType("!".into(), "int".into()));

        err("fn f(a: bool) { ~a; }", pos(1, 17),
            Msg::UnOpType("~".into(), "bool".into()));
        err("fn f(a: bool) { -a; }", pos(1, 17),
            Msg::UnOpType("-".into(), "bool".into()));
        err("fn f(a: bool) { +a; }", pos(1, 17),
            Msg::UnOpType("+".into(), "bool".into()));
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
                "A".into(), "B".into()));
        err("class A class B fn f(a: A, b: B) { b !== a; }", pos(1, 38),
            Msg::BinOpType("!==".into(),
                "B".into(), "A".into()));
        err("fn f(a: bool) { a+a; }", pos(1, 18),
            Msg::BinOpType("+".into(), "bool".into(), "bool".into()));
        err("fn f(a: bool) { a^a; }", pos(1, 18),
            Msg::BinOpType("^".into(), "bool".into(), "bool".into()));
        err("fn f(a: int) { a||a; }", pos(1, 17),
            Msg::BinOpType("||".into(), "int".into(),"int".into()));
        err("fn f(a: int) { a&&a; }", pos(1, 17),
            Msg::BinOpType("&&".into(), "int".into(), "int".into()));
        err("fn f(a: int) { a===a; }", pos(1, 17),
            Msg::BinOpType("===".into(), "int".into(), "int".into()));
        err("fn f(a: int) { a!==a; }", pos(1, 17),
            Msg::BinOpType("!==".into(), "int".into(), "int".into()));
        err("fn f(a: bool) { a===a; }", pos(1, 18),
            Msg::BinOpType("===".into(), "bool".into(), "bool".into()));
        err("fn f(a: bool) { a!==a; }", pos(1, 18),
            Msg::BinOpType("!==".into(), "bool".into(), "bool".into()));
        err("fn f(a: Str) { a-a; }", pos(1, 17),
            Msg::BinOpType("-".into(), "Str".into(), "Str".into()));
        err("fn f(a: Str) { a*a; }", pos(1, 17),
            Msg::BinOpType("*".into(), "Str".into(), "Str".into()));
        err("fn f(a: Str) { a%a; }", pos(1, 17),
            Msg::BinOpType("%".into(), "Str".into(), "Str".into()));
    }

    #[test]
    fn type_function_return_type() {
        ok("fn foo() -> int { return 1; }\nfn f() { let i: int = foo(); }");
        err("fn foo() -> int { return 1; }\nfn f() { let i: bool = foo(); }",
            pos(2, 10),
            Msg::AssignType("i".into(),
                "bool".into(), "int".into()));
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
            Msg::ParamTypesIncompatible("foo".into(),
                vec![],
                vec!["int".into()]));
        err("fn foo(a: int) {}\nfn f() { foo(true); }", pos(2, 10),
            Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into()],
                vec!["bool".into()]));
        err("fn foo(a: int, b: bool) {}\nfn f() { foo(1, 2); }", pos(2, 10),
            Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into(), "bool".into()],
                vec!["int".into(), "int".into()]));
    }

    #[test]
    fn type_return_nil() {
        ok("fn foo() -> Str { return nil; }");
        ok("class Foo fn foo() -> Foo { return nil; }");
        err("fn foo() -> int { return nil; }",
            pos(1, 19), Msg::IncompatibleWithNil("int".into()));
    }

    #[test]
    fn type_nil_as_argument() {
        ok("fn foo(a: Str) {} fn test() { foo(nil); }");
        err("fn foo(a: int) {} fn test() { foo(nil); }",
            pos(1, 31), Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into()], vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_ctor() {
        ok("class Foo(a: Str) fn test() { Foo(nil); }");
        err("class Foo(a: int) fn test() { Foo(nil); }",
            pos(1, 31), Msg::UnknownCtor("Foo".into(), vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_local_variable() {
        ok("fn f() { let x: Str = nil; }");
        err("fn f() { let x: int = nil; }",
            pos(1, 10), Msg::AssignType("x".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_for_prop() {
        ok("class Foo(a: Str) fn f() { Foo(nil).a = nil; }");
        err("class Foo(a: int) fn f() { Foo(1).a = nil; }",
            pos(1, 37), Msg::AssignProp("a".into(), "Foo".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_method() {
        err("fn f() { nil.test(); }", pos(1, 13),
            Msg::UnknownMethod("nil".into(), "test".into(), Vec::new()));
    }

    #[test]
    fn type_nil_as_method_argument() {
        ok("class Foo {
            fn f(self, a: Str) {}
            fn f(self, a: int) {}
        } fn f() { Foo().f(nil); }");
        err("class Foo {
                fn f(self, a: Str) {}
                fn f(self, a: Foo) {}
            }

            fn f() {
                Foo().f(nil);
            }", pos(7, 22), Msg::MultipleCandidates("Foo".into(),
                "f".into(), vec!["nil".into()]));
    }

    #[test]
    fn type_array() {
        ok("fn f(a: IntArray) -> int { return a[1]; }");
        err("fn f(a: IntArray) -> Str { return a[1]; }", pos(1, 28),
            Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_array_assign() {
        let iarray = BuiltinType::Class(ClassId(3));
        ok("fn f(a: IntArray) -> int { return a[3] = 4; }");
        err("fn f(a: IntArray) { a[3] = \"b\"; }", pos(1, 26),
            Msg::UnknownMethod("IntArray".into(), "set".into(),
                vec!["int".into(), "Str".into()]));
        err("fn f(a: IntArray) -> Str { return a[3] = 4; }", pos(1, 28),
            Msg::ReturnType("Str".into(), "int".into()));
    }
}
