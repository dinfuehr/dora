use ctxt::{CallType, Context, Fct, FctId, FctSrc, IdentType};
use class::ClassId;
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::Visitor;
use interner::Name;
use lexer::position::Position;
use semck::read_type;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'ast>) {
    for fct in &ctxt.fcts {
        if !fct.is_src() { continue; }

        let src = fct.src();
        let mut src = src.lock().unwrap();
        let ast = fct.ast;

        let mut typeck = TypeCheck {
            ctxt: ctxt,
            fct: &fct,
            src: &mut src,
            ast: ast,
            expr_type: BuiltinType::Unit,
        };

        typeck.check();
    }
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc<'ast>,
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
            let ty = self.src.vars[s.var()].ty;
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
        self.src.vars[s.var()].ty = defined_type;

        if let Some(expr_type) = expr_type {
            if !defined_type.allows(self.ctxt, expr_type) {
                let name = self.ctxt.interner.str(s.name).to_string();
                let defined_type = defined_type.name(self.ctxt);
                let expr_type = expr_type.name(self.ctxt);
                let msg = Msg::AssignType(name, defined_type, expr_type);
                self.ctxt.diag.borrow_mut().report(s.pos, msg);
            }

        // let variable binding needs to be assigned
        } else if !s.reassignable {
            self.ctxt.diag.borrow_mut().report(s.pos, Msg::LetMissingInitialization);
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

        if !fct_type.allows(self.ctxt, expr_type) {
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

    fn check_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        self.visit_expr(&s.expr);
        let ty = self.expr_type;

        if ty.is_nil() {
            self.ctxt.diag.borrow_mut().report(s.pos, Msg::ThrowNil);


        } else if !ty.reference_type() {
            let tyname = ty.name(self.ctxt);
            self.ctxt.diag.borrow_mut().report(s.pos, Msg::ReferenceTypeExpected(tyname));
        }
    }

    fn check_stmt_try(&mut self, s: &'ast StmtTryType) {
        self.visit_stmt(&s.try_block);

        for catch in &s.catch_blocks {
            self.visit_stmt(&catch.block);
        }

        if let Some(ref finally_block) = s.finally_block {
            self.visit_stmt(&finally_block.block);
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        match e.ident_type() {
            IdentType::Var(varid) => {
                let ty = self.src.vars[varid].ty;
                e.set_ty(ty);
                self.expr_type = ty;
            }

            IdentType::Field(clsid, fieldid) => {
                let cls = self.ctxt.cls_by_id(clsid);
                let field = &cls.fields[fieldid];

                e.set_ty(field.ty);
                self.expr_type = field.ty;
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
            let args = vec![index_type, value_type];
            let ret_type = Some(BuiltinType::Unit);

            if let Some((cls_id, fct_id, _)) = self.find_method(e.pos, object_type, name,
                                                                  &args, ret_type) {
                let call_type = CallType::Method(cls_id, fct_id);
                assert!(self.src.calls.insert(e.id, call_type).is_none());

                let index_type = self.ctxt.fct_by_id(fct_id).params_types[1];

                e.set_ty(index_type);
                self.expr_type = index_type;
            } else {
                e.set_ty(BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
            }

        } else if e.lhs.is_field() || e.lhs.is_ident() {
            self.visit_expr(&e.lhs);
            let lhs_type = self.expr_type;

            self.visit_expr(&e.rhs);
            let rhs_type = self.expr_type;

            // check if variable is reassignable
            if e.lhs.is_ident() {
                let ident = e.lhs.to_ident().unwrap();
                let ident_type = ident.ident_type();

                if let IdentType::Var(varid) = ident_type {
                    if !self.src.vars[varid].reassignable {
                        self.ctxt.diag.borrow_mut().report(e.pos, Msg::LetReassigned);
                    }

                } else if let IdentType::Field(clsid, fieldid) = ident_type {
                    if !self.fct.ctor.is() && !self.ctxt.cls_by_id(clsid).fields[fieldid].reassignable {
                        self.ctxt.diag.borrow_mut().report(e.pos, Msg::LetReassigned);
                    }
                }

            // check if field is reassignable, assignment only allowed in ctor
            } else if !self.fct.ctor.is() {
                let lhs = e.lhs.to_field().unwrap();

                if lhs.has_cls_and_field() {
                    let (cls, fieldid) = lhs.cls_and_field();

                    if !self.ctxt.cls_by_id(cls).fields[fieldid].reassignable {
                        self.ctxt.diag.borrow_mut().report(e.pos, Msg::LetReassigned);
                    }

                } else {
                    return;
                }
            }

            if !lhs_type.allows(self.ctxt, rhs_type) {
                let msg = if e.lhs.is_ident() {
                    let ident = e.lhs.to_ident().unwrap();
                    let name = self.ctxt.interner.str(ident.name).to_string();
                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);

                    Msg::AssignType(name, lhs_type, rhs_type)

                } else {
                    let field = e.lhs.to_field().unwrap();
                    let name = self.ctxt.interner.str(field.name).to_string();

                    let field_type = field.object.ty();
                    let field_type = field_type.name(self.ctxt);

                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);

                    Msg::AssignField(name, field_type, lhs_type, rhs_type)
                };

                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }

            e.set_ty(lhs_type);
            self.expr_type = lhs_type;

        } else {
            self.ctxt.diag.borrow_mut().report(e.pos, Msg::LvalueExpected);
            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn find_method(&mut self, pos: Position, object_type: BuiltinType,
                   name: Name, args: &[BuiltinType],
                   return_type: Option<BuiltinType>) -> Option<(ClassId, FctId, BuiltinType)> {
        let cls_id = match object_type {
            BuiltinType::Class(cls_id) => Some(cls_id),
            _ => self.ctxt.primitive_classes.find_class(object_type)
        };

        if let Some(cls_id) = cls_id {
            let cls = self.ctxt.cls_by_id(cls_id);
            let ctxt = self.ctxt;

            let candidates = cls.find_methods_with(ctxt, name, |method| {
                args_compatible(ctxt, &method.params_types, args)
                && (return_type.is_none()
                    || method.return_type == return_type.unwrap())
            });

            if candidates.len() == 1 {
                let candidate = candidates[0];
                let fct = self.ctxt.fct_by_id(candidate);

                return Some((fct.owner_class.unwrap(), candidate, fct.return_type));

            } else if candidates.len() > 1 {
                let object_type = object_type.name(self.ctxt);
                let args = args.iter().map(|a| a.name(self.ctxt)).collect::<Vec<String>>();
                let name = self.ctxt.interner.str(name).to_string();
                let msg = Msg::MultipleCandidates(object_type, name, args);
                self.ctxt.diag.borrow_mut().report(pos, msg);
                return None;
            }
        }

        let type_name = object_type.name(self.ctxt);
        let name = self.ctxt.interner.str(name).to_string();
        let param_names = args.iter().map(|a| a.name(self.ctxt)).collect::<Vec<String>>();
        let msg = Msg::UnknownMethod(type_name, name, param_names);
        self.ctxt.diag.borrow_mut().report(pos, msg);

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

            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;

        } else {
            e.set_ty(expected_type);
            self.expr_type = expected_type;
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
        e.set_ty(BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_expr_bin_int(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Int);
        e.set_ty(BuiltinType::Int);
        self.expr_type = BuiltinType::Int;
    }

    fn check_expr_bin_add(&mut self,  e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        if lhs_type == BuiltinType::Str {
            self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Str);
            e.set_ty(BuiltinType::Str);
            self.expr_type = BuiltinType::Str;
        } else {
            self.check_expr_bin_int(e, op, lhs_type, rhs_type);
        }
    }

    fn check_expr_bin_cmp(&mut self,  e: &'ast ExprBinType, cmp: CmpOp, lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        let ty = lhs_type.if_nil(rhs_type);

        let expected_type = match cmp {
            CmpOp::Is | CmpOp::IsNot => {
                if !lhs_type.reference_type() {
                    let lhs_type = lhs_type.name(self.ctxt);
                    self.ctxt.diag.borrow_mut().report(e.pos,
                        Msg::ReferenceTypeExpected(lhs_type));
                }

                if !rhs_type.reference_type() {
                    let rhs_type = rhs_type.name(self.ctxt);
                    self.ctxt.diag.borrow_mut().report(e.pos,
                        Msg::ReferenceTypeExpected(rhs_type));
                }

                if !(lhs_type.is_nil() || lhs_type.allows(self.ctxt, rhs_type))
                    && !(rhs_type.is_nil() || rhs_type.allows(self.ctxt, lhs_type)) {
                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);
                    self.ctxt.diag.borrow_mut().report(e.pos,
                        Msg::TypesIncompatible(lhs_type, rhs_type));
                }

                e.set_ty(BuiltinType::Bool);
                self.expr_type = BuiltinType::Bool;
                return;
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
        e.set_ty(BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_type(&mut self, e: &'ast ExprBinType, op: BinOp, lhs_type: BuiltinType,
                  rhs_type: BuiltinType, expected_type: BuiltinType) {
        if !expected_type.allows(self.ctxt, lhs_type)
            || !expected_type.allows(self.ctxt, rhs_type) {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name(self.ctxt);
            let rhs_type = rhs_type.name(self.ctxt);
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType) {
        if e.object.is_some() {
            self.check_method_call(e);
            return;
        }

        let call_type = *self.src.calls.get(&e.id).unwrap();

        let call_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        match call_type {
            CallType::CtorNew(cls_id, _) => {
                let cls = self.ctxt.cls_by_id(cls_id);
                e.set_ty(cls.ty);
                self.expr_type = cls.ty;
                let mut found = false;

                for &ctor in &cls.ctors {
                    let ctor = self.ctxt.fct_by_id(ctor);

                    if args_compatible(self.ctxt, &ctor.params_types, &call_types) {
                        let call_type = CallType::CtorNew(cls_id, ctor.id);
                        assert!(self.src.calls.insert(e.id, call_type).is_some());

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
                let callee = &self.ctxt.fcts[callee_id];

                e.set_ty(callee.return_type);
                self.expr_type = callee.return_type;

                if !args_compatible(self.ctxt, &callee.params_types, &call_types) {
                    let callee_name = self.ctxt.interner.str(callee.name).to_string();
                    let callee_params = callee.params_types.iter()
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

    fn check_expr_delegation(&mut self, e: &'ast ExprDelegationType) {
        let arg_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        let owner = self.ctxt.cls_by_id(self.fct.owner_class.unwrap());

        // init(..) : super(..) is not allowed for classes with primary ctor
        if e.ty.is_super() && owner.primary_ctor && self.fct.ctor.is_secondary() {
            let name = self.ctxt.interner.str(owner.name).to_string();
            let msg = Msg::NoSuperDelegationWithPrimaryCtor(name);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);

            return;
        }

        // init(..) : super(..) not allowed for classes without base class
        if e.ty.is_super() && owner.parent_class.is_none() {
            let name = self.ctxt.interner.str(owner.name).to_string();
            let msg = Msg::NoSuperClass(name);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);

            return;
        }

        let cls_id = if e.ty.is_super() {
            owner.parent_class.unwrap()
        } else {
            owner.id
        };

        let cls = self.ctxt.cls_by_id(cls_id);

        for &ctor_id in &cls.ctors {
            let ctor = self.ctxt.fct_by_id(ctor_id);

            if args_compatible(self.ctxt, &ctor.params_types, &arg_types) {
                e.set_fct_id(ctor_id);
                e.set_class_id(cls.id);

                let call_type = CallType::Ctor(cls.id, ctor.id);
                assert!(self.src.calls.insert(e.id, call_type).is_none());
                return;
            }
        }

        let name = self.ctxt.interner.str(cls.name).to_string();
        let arg_types = arg_types.iter().map(|t| t.name(self.ctxt)).collect();
        let msg = Msg::UnknownCtor(name, arg_types);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
    }

    fn check_method_call(&mut self, e: &'ast ExprCallType) {
        let object = e.object.as_ref().unwrap();

        if let Some(ref expr) = object.to_super() {
            expr.set_method(true);
        }

        self.visit_expr(object);
        let object_type = self.expr_type;

        let call_types : Vec<BuiltinType> = e.args.iter().map(|arg| {
            self.visit_expr(arg);
            self.expr_type
        }).collect();

        if let Some((cls_id, fct_id, return_type)) = self.find_method(e.pos, object_type,
                                                            e.name, &call_types, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            assert!(self.src.calls.insert(e.id, call_type).is_none());
            e.set_ty(return_type);
            self.expr_type = return_type;
        } else {
            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn check_expr_field(&mut self, e: &'ast ExprFieldType) {
        self.visit_expr(&e.object);

        if let BuiltinType::Class(class_id) = self.expr_type {
            let cls = self.ctxt.cls_by_id(class_id);

            if let Some((class_id, field_id)) = cls.find_field(self.ctxt, e.name) {
                e.set_cls_and_field(class_id, field_id);

                let field = self.ctxt.field(class_id, field_id);
                e.set_ty(field.ty);
                self.expr_type = field.ty;
                return;
            }
        }

        // field not found, report error
        let field_name = self.ctxt.interner.str(e.name).to_string();
        let expr_name = self.expr_type.name(self.ctxt);
        let msg = Msg::UnknownField(field_name, expr_name);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        // we don't know the type of the field, just assume ()
        e.set_ty(BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_this(&mut self, e: &'ast ExprSelfType) {
        if let Some(clsid) = self.fct.owner_class {
            let ty = BuiltinType::Class(clsid);
            e.set_ty(ty);
            self.expr_type = ty;

        } else {
            let msg = Msg::ThisUnavailable;
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn check_expr_super(&mut self, e: &'ast ExprSuperType) {
        if !e.method() {
            let msg = Msg::SuperNeedsMethodCall;
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }

        if let Some(clsid) = self.fct.owner_class {
            let cls = self.ctxt.cls_by_id(clsid);

            if let Some(superid) = cls.parent_class {
                let ty = BuiltinType::Class(superid);
                e.set_ty(ty);
                self.expr_type = ty;

                return;
            }
        }

        let msg = Msg::SuperUnavailable;
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        e.set_ty(BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_nil(&mut self, e: &'ast ExprNilType) {
        e.set_ty(BuiltinType::Nil);
        self.expr_type = BuiltinType::Nil;
    }

    fn check_expr_array(&mut self, e: &'ast ExprArrayType) {
        self.visit_expr(&e.object);
        let object_type = self.expr_type;

        self.visit_expr(&e.index);
        let index_type = self.expr_type;

        let name = self.ctxt.interner.intern("get");
        let args = vec![index_type];

        if let Some((cls_id, fct_id, return_type)) = self.find_method(e.pos, object_type,
                                                            name, &args, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            assert!(self.src.calls.insert(e.id, call_type).is_none());

            e.set_ty(return_type);
            self.expr_type = return_type;
        } else {
            e.set_ty(BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn check_expr_conv(&mut self, e: &'ast ExprConvType) {
        self.visit_expr(&e.object);
        let object_type = self.expr_type;
        e.object.set_ty(self.expr_type);

        let check_type = match read_type(self.ctxt, &e.data_type) {
            Some(ty) => ty,
            None => { return; }
        };

        if !check_type.reference_type() {
            let name = check_type.name(self.ctxt);
            self.ctxt.diag.borrow_mut().report(e.pos, Msg::ReferenceTypeExpected(name));
            return;
        }

        if object_type.subclass_from(self.ctxt, check_type) {
            // open class A { } class B: A { }
            // (b is A) is valid

            e.set_valid(true);

        } else if check_type.subclass_from(self.ctxt, object_type) {
            // normal check

        } else {
            let object_type = object_type.name(self.ctxt);
            let check_type = check_type.name(self.ctxt);
            let msg = Msg::TypesIncompatible(object_type, check_type);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        e.set_cls_id(check_type.cls_id(self.ctxt));
        self.expr_type = if e.is {
            BuiltinType::Bool
        } else {
            check_type
        };
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(_) => { self.expr_type = BuiltinType::Int; },
            ExprLitStr(_) => { self.expr_type = BuiltinType::Str; },
            ExprLitBool(_) => { self.expr_type = BuiltinType::Bool; },
            ExprIdent(ref expr) => self.check_expr_ident(expr),
            ExprAssign(ref expr) => self.check_expr_assign(expr),
            ExprUn(ref expr) => self.check_expr_un(expr),
            ExprBin(ref expr) => self.check_expr_bin(expr),
            ExprCall(ref expr) => self.check_expr_call(expr),
            ExprDelegation(ref expr) => self.check_expr_delegation(expr),
            ExprField(ref expr) => self.check_expr_field(expr),
            ExprSelf(ref expr) => self.check_expr_this(expr),
            ExprSuper(ref expr) => self.check_expr_super(expr),
            ExprNil(ref expr) => self.check_expr_nil(expr),
            ExprArray(ref expr) => self.check_expr_array(expr),
            ExprConv(ref expr) => self.check_expr_conv(expr),
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtIf(ref stmt) => self.check_stmt_if(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),
            StmtThrow(ref stmt) => self.check_stmt_throw(stmt),
            StmtTry(ref stmt) => self.check_stmt_try(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(_) => visit::walk_stmt(self, s),
            StmtContinue(_) => visit::walk_stmt(self, s),
            StmtLoop(_) => visit::walk_stmt(self, s),
            StmtExpr(_) => visit::walk_stmt(self, s),
            StmtBlock(_) => visit::walk_stmt(self, s),
        }
    }
}

fn args_compatible(ctxt: &Context, def: &[BuiltinType], expr: &[BuiltinType]) -> bool {
    if def.len() != expr.len() {
        return false;
    }

    for (ind, arg) in def.iter().enumerate() {
        if !arg.allows(ctxt, expr[ind]) {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;
    use test::parse_with_errors;

    #[test]
    fn type_method_len() {
        ok("fun f(a: Str) -> int { return a.len(); }");
        ok("fun f(a: Str) -> int { return \"abc\".len(); }");
    }

    #[test]
    fn type_object_field() {
        ok("class Foo(let a:int) fun f(x: Foo) -> int { return x.a; }");
        ok("class Foo(let a:Str) fun f(x: Foo) -> Str { return x.a; }");
        err("class Foo(let a:int) fun f(x: Foo) -> bool { return x.a; }",
             pos(1, 46), Msg::ReturnType("bool".into(), "int".into()));

        parse_with_errors("class Foo(let a:int) fun f(x: Foo) -> int { return x.b; }", |ctxt| {
            let diag = ctxt.diag.borrow();
            let errors = diag.errors();
            assert_eq!(2, errors.len());

            let err = &errors[0];
            assert_eq!(pos(1, 53), err.pos);
            assert_eq!(Msg::UnknownField("b".into(), "Foo".into()), err.msg);

            let err = &errors[1];
            assert_eq!(pos(1, 45), err.pos);
            assert_eq!(Msg::ReturnType("int".into(), "()".into()), err.msg);
        });
    }

    #[test]
    fn type_object_set_field() {
        ok("class Foo(var a: int) fun f(x: Foo) { x.a = 1; }");
        err("class Foo(var a: int) fun f(x: Foo) { x.a = false; }",
            pos(1, 43),
            Msg::AssignField("a".into(), "Foo".into(), "int".into(), "bool".into()));
    }

    #[test]
    fn type_object_field_without_self() {
        ok("class Foo(let a: int) { fun f() -> int { return a; } }");
        ok("class Foo(var a: int) { fun set(x: int) { a = x; } }");
        err("class Foo(let a: int) { fun set(x: int) { a = x; } }",
            pos(1, 45), Msg::LetReassigned);
        err("class Foo(let a: int) { fun set(x: int) { b = x; } }",
            pos(1, 43), Msg::UnknownIdentifier("b".into()));
    }

    #[test]
    fn type_method_call() {
        ok("class Foo {
                fun bar() {}
                fun baz() -> int { return 1; }
            }

            fun f(x: Foo) { x.bar(); }
            fun g(x: Foo) -> int { return x.baz(); }");

        err("class Foo {
                 fun bar() -> int { return 0; }
             }

             fun f(x: Foo) -> Str { return x.bar(); }",
             pos(5, 37), Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_method_defined_twice() {
        err("class Foo {
                 fun bar() {}
                 fun bar() {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec![], pos(2, 18)));

        err("class Foo {
                 fun bar() {}
                 fun bar() -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec![], pos(2, 18)));

        err("class Foo {
                 fun bar(a: int) {}
                 fun bar(a: int) -> int {}
             }", pos(3, 18), Msg::MethodExists(
                 "Foo".into(), "bar".into(), vec!["int".into()], pos(2, 18)));

        ok("class Foo {
                fun bar(a: int) {}
                fun bar(a: Str) {}
            }");
    }

    #[test]
    fn type_self() {
        ok("class Foo { fun me() -> Foo { return self; } }");
        err("class Foo fun me() { return self; }",
            pos(1, 29), Msg::ThisUnavailable);

        ok("class Foo(let a: int, let b: int) {
            fun bar() -> int { return self.a + self.b; }
        }");

        ok("class Foo(var a: int) {
            fun setA(a: int) { self.a = a; }
        }");

        ok("class Foo {
            fun zero() -> int { return 0; }
            fun other() -> int { return self.zero(); }
        }");

        ok("class Foo {
            fun bar() { self.bar(); }
        }");
    }

    #[test]
    fn type_unknown_method() {
        err("class Foo {
                 fun bar(a: int) { }
             }

             fun f(x: Foo) { x.bar(); }",
             pos(5, 31),
             Msg::UnknownMethod("Foo".into(), "bar".into(), Vec::new()));

         err("class Foo { }
              fun f(x: Foo) { x.bar(1); }",
              pos(2, 32),
              Msg::UnknownMethod("Foo".into(),
                "bar".into(), vec!["int".into()]));
    }

    #[test]
    fn type_ctor() {
        ok("class Foo fun f() -> Foo { return Foo(); }");
        ok("class Foo(let a: int) fun f() -> Foo { return Foo(1); }");
        err("class Foo fun f() -> Foo { return 1; }", pos(1, 28),
            Msg::ReturnType("Foo".into(), "int".into()));
    }

    #[test]
    fn type_def_for_return_type() {
        ok("fun a() -> int { return 1; }");
        err("fun a() -> unknown {}", pos(1, 12), Msg::UnknownType("unknown".into()));
    }

    #[test]
    fn type_def_for_param() {
        ok("fun a(b: int) {}");
        err("fun a(b: foo) {}", pos(1, 10), Msg::UnknownType("foo".into()));
    }

    #[test]
    fn type_def_for_var() {
        ok("fun a() { let a : int = 1; }");
        err("fun a() { let a : test = 1; }", pos(1, 19), Msg::UnknownType("test".into()));
    }

    #[test]
    fn type_var_needs_expr_or_definition() {
        err("fun a() { let a; }", pos(1, 11), Msg::VarNeedsTypeInfo("a".into()));
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fun f() { let a : int = 1; }");
        ok("fun f() { let a : bool = false; }");
        ok("fun f() { let a : Str = \"f\"; }");

        err("fun f() { let a : int = true; }",
            pos(1, 11), Msg::AssignType(
                "a".into(), "int".into(), "bool".into()));
        err("fun f() { let b : bool = 2; }",
            pos(1, 11), Msg::AssignType(
                "b".into(), "bool".into(), "int".into()));
    }

    #[test]
    fn type_while() {
        ok("fun x() { while true { } }");
        ok("fun x() { while false { } }");
        err("fun x() { while 2 { } }", pos(1, 11), Msg::WhileCondType("int".into()));
    }

    #[test]
    fn type_if() {
        ok("fun x() { if true { } }");
        ok("fun x() { if false { } }");
        err("fun x() { if 4 { } }", pos(1, 11), Msg::IfCondType("int".into()));
    }

    #[test]
    fn type_return_unit() {
        ok("fun f() { return; }");
        err("fun f() { return 1; }", pos(1, 11),
            Msg::ReturnType("()".into(), "int".into()));
    }

    #[test]
    fn type_return() {
        ok("fun f() -> int { let a = 1; return a; }");
        ok("fun f() -> int { return 1; }");
        err("fun f() -> int { return; }", pos(1, 18),
            Msg::ReturnType("int".into(), "()".into()));

        ok("fun f() -> int { return 0; }
            fun g() -> int { return f(); }");
        err("fun f() { }
             fun g() -> int { return f(); }", pos(2, 31),
             Msg::ReturnType("int".into(), "()".into()));
    }

    #[test]
    fn type_variable() {
        ok("fun f(a: int) { let b: int = a; }");
    }

    #[test]
    fn type_assign_lvalue() {
        err("fun f() { 1 = 3; }", pos(1, 13), Msg::LvalueExpected);
    }

    #[test]
    fn type_un_op() {
        ok("fun f(a: int) { ~a; -a; +a; }");
        err("fun f(a: int) { !a; }", pos(1, 17),
            Msg::UnOpType("!".into(), "int".into()));

        err("fun f(a: bool) { ~a; }", pos(1, 18),
            Msg::UnOpType("~".into(), "bool".into()));
        err("fun f(a: bool) { -a; }", pos(1, 18),
            Msg::UnOpType("-".into(), "bool".into()));
        err("fun f(a: bool) { +a; }", pos(1, 18),
            Msg::UnOpType("+".into(), "bool".into()));
    }

    #[test]
    fn type_bin_op() {
        ok("fun f(a: int) { a+a; a-a; a*a; a/a; a%a; }");
        ok("fun f(a: int) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fun f(a: Str) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fun f(a: Str) { a===a; a!==a; a+a; }");
        ok("class Foo fun f(a: Foo) { a===a; a!==a; }");
        ok("fun f(a: int) { a|a; a&a; a^a; }");
        ok("fun f(a: bool) { a||a; a&&a; }");

        err("class A class B fun f(a: A, b: B) { a === b; }", pos(1, 39),
            Msg::TypesIncompatible("A".into(), "B".into()));
        err("class A class B fun f(a: A, b: B) { b !== a; }", pos(1, 39),
            Msg::TypesIncompatible("B".into(), "A".into()));
        err("fun f(a: bool) { a+a; }", pos(1, 19),
            Msg::BinOpType("+".into(), "bool".into(), "bool".into()));
        err("fun f(a: bool) { a^a; }", pos(1, 19),
            Msg::BinOpType("^".into(), "bool".into(), "bool".into()));
        err("fun f(a: int) { a||a; }", pos(1, 18),
            Msg::BinOpType("||".into(), "int".into(),"int".into()));
        err("fun f(a: int) { a&&a; }", pos(1, 18),
            Msg::BinOpType("&&".into(), "int".into(), "int".into()));
        err("fun f(a: Str) { a-a; }", pos(1, 18),
            Msg::BinOpType("-".into(), "Str".into(), "Str".into()));
        err("fun f(a: Str) { a*a; }", pos(1, 18),
            Msg::BinOpType("*".into(), "Str".into(), "Str".into()));
        err("fun f(a: Str) { a%a; }", pos(1, 18),
            Msg::BinOpType("%".into(), "Str".into(), "Str".into()));
    }

    #[test]
    fn type_function_return_type() {
        ok("fun foo() -> int { return 1; }\nfun f() { let i: int = foo(); }");
        err("fun foo() -> int { return 1; }\nfun f() { let i: bool = foo(); }",
            pos(2, 11),
            Msg::AssignType("i".into(),
                "bool".into(), "int".into()));
    }

    #[test]
    fn type_ident_in_function_params() {
        ok("fun f(a: int) {}\nfun g() { let a = 1; f(a); }");
    }

    #[test]
    fn type_recursive_function_call() {
        ok("fun f(a: int) { f(a); }");
    }

    #[test]
    fn type_function_params() {
        ok("fun foo() {}\nfun f() { foo(); }");
        ok("fun foo(a: int) {}\nfun f() { foo(1); }");
        ok("fun foo(a: int, b: bool) {}\nfun f() { foo(1, true); }");

        err("fun foo() {}\nfun f() { foo(1); }", pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(),
                vec![],
                vec!["int".into()]));
        err("fun foo(a: int) {}\nfun f() { foo(true); }", pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into()],
                vec!["bool".into()]));
        err("fun foo(a: int, b: bool) {}\nfun f() { foo(1, 2); }", pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into(), "bool".into()],
                vec!["int".into(), "int".into()]));
    }

    #[test]
    fn type_return_nil() {
        ok("fun foo() -> Str { return nil; }");
        ok("class Foo fun foo() -> Foo { return nil; }");
        err("fun foo() -> int { return nil; }",
            pos(1, 20), Msg::IncompatibleWithNil("int".into()));
    }

    #[test]
    fn type_nil_as_argument() {
        ok("fun foo(a: Str) {} fun test() { foo(nil); }");
        err("fun foo(a: int) {} fun test() { foo(nil); }",
            pos(1, 33), Msg::ParamTypesIncompatible("foo".into(),
                vec!["int".into()], vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_ctor() {
        ok("class Foo(let a: Str) fun test() { Foo(nil); }");
        err("class Foo(let a: int) fun test() { Foo(nil); }",
            pos(1, 36), Msg::UnknownCtor("Foo".into(), vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_local_variable() {
        ok("fun f() { let x: Str = nil; }");
        err("fun f() { let x: int = nil; }",
            pos(1, 11), Msg::AssignType("x".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_for_field() {
        ok("class Foo(var a: Str) fun f() { Foo(nil).a = nil; }");
        err("class Foo(var a: int) fun f() { Foo(1).a = nil; }",
            pos(1, 42), Msg::AssignField("a".into(), "Foo".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_method() {
        err("fun f() { nil.test(); }", pos(1, 14),
            Msg::UnknownMethod("nil".into(), "test".into(), Vec::new()));
    }

    #[test]
    fn type_nil_as_method_argument() {
        ok("class Foo {
            fun f(a: Str) {}
            fun f(a: int) {}
        } fun f() { Foo().f(nil); }");
        err("class Foo {
                fun f(a: Str) {}
                fun f(a: Foo) {}
            }

            fun f() {
                Foo().f(nil);
            }", pos(7, 22), Msg::MultipleCandidates("Foo".into(),
                "f".into(), vec!["nil".into()]));
    }

    #[test]
    fn type_array() {
        ok("fun f(a: IntArray) -> int { return a[1]; }");
        err("fun f(a: IntArray) -> Str { return a[1]; }", pos(1, 29),
            Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_array_assign() {
        ok("fun f(a: IntArray) -> int { return a[3] = 4; }");
        err("fun f(a: IntArray) { a[3] = \"b\"; }", pos(1, 27),
            Msg::UnknownMethod("IntArray".into(), "set".into(),
                vec!["int".into(), "Str".into()]));
        err("fun f(a: IntArray) -> Str { return a[3] = 4; }", pos(1, 29),
            Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_throw() {
        ok("fun f() { throw \"abc\"; }");
        ok("fun f() { throw emptyIntArray(); }");
        err("fun f() { throw 1; }", pos(1, 11), Msg::ReferenceTypeExpected("int".into()));
        err("fun f() { throw nil; }", pos(1, 11), Msg::ThrowNil);
    }

    #[test]
    fn type_catch_variable() {
        ok("fun f() { try {} catch a: Str { print(a); } }");
        ok("fun f() { var x = 0; try {} catch a: IntArray { x=a.len(); } }");
    }

    #[test]
    fn try_value_type() {
        err("fun f() { try {} catch a: int {} }", pos(1, 27),
            Msg::ReferenceTypeExpected("int".into()));
    }

    #[test]
    fn try_missing_catch() {
        err("fun f() { try {} }", pos(1, 11), Msg::CatchOrFinallyExpected);
    }

    #[test]
    fn try_check_blocks() {
        err("fun f() { try {} catch a: IntArray {} a.len(); }", pos(1, 39),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { try {} catch a: IntArray {} finally { a.len(); } }", pos(1, 49),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { try { return a; } catch a: IntArray {} }", pos(1, 24),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { try { } catch a: IntArray { return a; } }", pos(1, 39),
            Msg::ReturnType("()".into(), "IntArray".into()));
    }

    #[test]
    fn let_without_initialization() {
        err("fun f() { let x: int; }", pos(1, 11),
            Msg::LetMissingInitialization);
    }

    #[test]
    fn var_without_initialization() {
        ok("fun f() { var x: int; }");
    }

    #[test]
    fn reassign_param() {
        ok("fun f(var a: int) { a = 1; }");
        err("fun f(a: int) { a = 1; }", pos(1, 19), Msg::LetReassigned);
    }

    #[test]
    fn reassign_field() {
        ok("class Foo(var x: int) fun foo(var f: Foo) { f.x = 1; }");
        err("class Foo(let x: int) fun foo(var f: Foo) { f.x = 1; }",
            pos(1, 49), Msg::LetReassigned);
    }

    #[test]
    fn reassign_catch() {
        err("fun f() {
               try {
                 throw \"test\";
               } catch x: IntArray {
                 x = emptyIntArray();
               }
             }", pos(5, 20), Msg::LetReassigned);
    }

    #[test]
    fn reassign_var() {
        ok("fun f() { var a=1; a=2; }");
    }

    #[test]
    fn reassign_let() {
        err("fun f() { let a=1; a=2; }", pos(1, 21), Msg::LetReassigned);
    }

    #[test]
    fn reassign_self() {
        err("class Foo {
            fun f() { self = Foo(); }
        }", pos(2, 28), Msg::LvalueExpected);
    }

    #[test]
    fn super_class() {
        ok("open class A class B: A");
        ok("open class A class B: A()");
        ok("open class A(a: int) class B: A(1)");
        err("open class A(a: int) class B: A(true)", pos(1, 31),
            Msg::UnknownCtor("A".into(), vec!["bool".into()]));
    }

    #[test]
    fn access_super_class_field() {
        ok("open class A(var a: int) class B(x: int): A(x*2)
            fun foo(b: B) { b.a = b.a + 10; }");
    }

    #[test]
    fn check_is() {
        ok("open class A class B: A
            fun f(a: A) -> bool { return a is B; }");
        ok("open class A class B: A
            fun f(b: B) -> bool { return b is A; }");
        ok("class A
            fun f(a: A) -> bool { return a is A; }");
        err("open class A class B: A
             fun f(a: A) -> bool { return a is Str; }", pos(2, 45),
            Msg::TypesIncompatible("A".into(), "Str".into()));
        err("open class A class B: A class C
             fun f(a: A) -> bool { return a is C; }", pos(2, 45),
            Msg::TypesIncompatible("A".into(), "C".into()));

        ok("open class A class B: A fun f() -> A { return B(); }");
        ok("open class A class B: A fun f() { let a: A = B(); }");
    }

    #[test]
    fn check_as() {
        ok("open class A class B: A
            fun f(a: A) -> B { return a as B; }");
        ok("class A
            fun f(a: A) -> A { return a as A; }");
        err("open class A class B: A
             fun f(a: A) -> Str { return a as Str; }", pos(2, 44),
            Msg::TypesIncompatible("A".into(), "Str".into()));
        err("open class A class B: A class C
             fun f(a: A) -> C { return a as C; }", pos(2, 42),
            Msg::TypesIncompatible("A".into(), "C".into()));
    }

    #[test]
    fn check_upcast() {
        ok("open class A class B: A
            fun f(b: B) -> A {
                let a: A = b;
                return a;
                //g(b);
                //return b;
            }

            fun g(a: A) {}");
    }

    #[test]
    fn check_cmp_is() {
        ok("fun f(x: Str) {
                let a = nil === x;
                let b = x === nil;
                let c = nil === nil;
            }");
    }

    #[test]
    fn super_delegation() {
        ok("open class A { fun f() {} }
            class B: A { fun g() {} }

            fun foo(b: B) {
                b.f();
                b.g();
            }");
    }

    #[test]
    fn super_method_call() {
        ok("open class A { open fun f() -> int { return 1; } }
            class B: A { override fun f() -> int { return super.f() + 1; } }");
    }

    #[test]
    fn super_as_normal_expression() {
        err("open class A { }
            class B: A { fun me() { let x = super; } }", pos(2, 45),
            Msg::SuperNeedsMethodCall);
    }
}
