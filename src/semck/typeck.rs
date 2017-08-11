use std::{f32, f64};

use ctxt::{CallType, ConstData, ConstValue, SemContext, ConvInfo, Fct, FctId, FctParent, FctSrc,
           IdentType};
use class::ClassId;
use dora_parser::error::msg::Msg;

use dora_parser::ast::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::visit::Visitor;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntBase, IntSuffix};
use semck::specialize::{self, SpecializeFor};
use sym::Sym::SymClass;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &SemContext<'ast>) {
    for fct in ctxt.fcts.iter() {
        let fct = fct.borrow();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.borrow_mut();
        let ast = fct.ast;

        let mut typeck = TypeCheck {
            ctxt: ctxt,
            fct: &fct,
            src: &mut src,
            ast: ast,
            expr_type: BuiltinType::Unit,
            negative_expr_id: NodeId(0),
        };

        typeck.check();
    }

    for xconst in ctxt.consts.iter() {
        let mut xconst = xconst.borrow_mut();

        let (_, value) = {
            let mut constck = ConstCheck {
                ctxt: ctxt,
                xconst: &*xconst,
                negative_expr_id: NodeId(0),
            };

            constck.check_expr(xconst.expr)
        };

        xconst.value = value;
    }
}

struct TypeCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    expr_type: BuiltinType,
    negative_expr_id: NodeId,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    fn check_stmt_var(&mut self, s: &'ast StmtVarType) {
        let var = *self.src.map_vars.get(s.id).unwrap();

        let expr_type = s.expr
            .as_ref()
            .map(|expr| {
                     self.visit_expr(&expr);
                     self.expr_type
                 });

        let defined_type = if let Some(_) = s.data_type {
            let ty = self.src.vars[var].ty;
            if ty == BuiltinType::Unit {
                None
            } else {
                Some(ty)
            }
        } else {
            expr_type
        };

        let defined_type = match defined_type {
            Some(ty) => ty,
            None => {
                let tyname = self.ctxt.interner.str(s.name).to_string();
                self.ctxt
                    .diag
                    .borrow_mut()
                    .report(s.pos, Msg::VarNeedsTypeInfo(tyname));

                return;
            }
        };

        // update type of variable, necessary when variable is only initialized with
        // an expression
        self.src.vars[var].ty = defined_type;

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
            self.ctxt
                .diag
                .borrow_mut()
                .report(s.pos, Msg::LetMissingInitialization);
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
        let expr_type = s.expr
            .as_ref()
            .map(|expr| {
                     self.visit_expr(&expr);

                     self.expr_type
                 })
            .unwrap_or(BuiltinType::Unit);

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
            self.ctxt
                .diag
                .borrow_mut()
                .report(s.pos, Msg::ThrowNil);


        } else if !ty.reference_type() {
            let tyname = ty.name(self.ctxt);
            self.ctxt
                .diag
                .borrow_mut()
                .report(s.pos, Msg::ReferenceTypeExpected(tyname));
        }
    }

    fn check_stmt_defer(&mut self, s: &'ast StmtDeferType) {
        self.visit_expr(&s.expr);

        if !s.expr.is_call() {
            self.ctxt
                .diag
                .borrow_mut()
                .report(s.pos, Msg::FctCallExpected);
        }
    }


    fn check_stmt_do(&mut self, s: &'ast StmtDoType) {
        self.visit_stmt(&s.do_block);

        for catch in &s.catch_blocks {
            self.visit_stmt(&catch.block);
        }

        if let Some(ref finally_block) = s.finally_block {
            self.visit_stmt(&finally_block.block);
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        let ident_type = *self.src.map_idents.get(e.id).unwrap();

        match ident_type {
            IdentType::Var(varid) => {
                let ty = self.src.vars[varid].ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            IdentType::Global(globalid) => {
                let ty = self.ctxt.globals[globalid].borrow().ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            IdentType::Field(clsid, fieldid) => {
                let cls = self.ctxt.classes[clsid].borrow();
                let field = &cls.fields[fieldid];

                self.src.set_ty(e.id, field.ty);
                self.expr_type = field.ty;
            }

            IdentType::Struct(sid) => {
                let ty = BuiltinType::Struct(sid);
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            IdentType::Const(const_id) => {
                let xconst = self.ctxt.consts[const_id].borrow();

                self.src.set_ty(e.id, xconst.ty);
                self.expr_type = xconst.ty;
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

            if let Some((cls_id, fct_id, _)) =
                self.find_method(e.pos, object_type, false, name, &args, ret_type) {
                let call_type = CallType::Method(cls_id, fct_id);
                self.src.map_calls.insert_or_replace(e.id, call_type);
            }

        } else if e.lhs.is_field() || e.lhs.is_ident() {
            self.visit_expr(&e.lhs);
            let lhs_type = self.expr_type;

            self.visit_expr(&e.rhs);
            let rhs_type = self.expr_type;

            if let Some(ident_type) = self.src.map_idents.get(e.lhs.id()) {
                match ident_type {
                    &IdentType::Var(varid) => {
                        if !self.src.vars[varid].reassignable {
                            self.ctxt
                                .diag
                                .borrow_mut()
                                .report(e.pos, Msg::LetReassigned);
                        }
                    }

                    &IdentType::Global(gid) => {
                        if !self.ctxt.globals[gid].borrow().reassignable {
                            self.ctxt
                                .diag
                                .borrow_mut()
                                .report(e.pos, Msg::LetReassigned);
                        }
                    }

                    &IdentType::Field(clsid, fieldid) => {
                        if !self.fct.ctor.is() &&
                           !self.ctxt.classes[clsid].borrow().fields[fieldid].reassignable {
                            self.ctxt
                                .diag
                                .borrow_mut()
                                .report(e.pos, Msg::LetReassigned);
                        }
                    }

                    &IdentType::Struct(_) => {
                        unimplemented!();
                    }

                    &IdentType::Const(_) => {
                        self.ctxt
                            .diag
                            .borrow_mut()
                            .report(e.pos, Msg::AssignmentToConst);
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

                        let field_type = self.src.ty(field.object.id());
                        let field_type = field_type.name(self.ctxt);

                        let lhs_type = lhs_type.name(self.ctxt);
                        let rhs_type = rhs_type.name(self.ctxt);

                        Msg::AssignField(name, field_type, lhs_type, rhs_type)
                    };

                    self.ctxt.diag.borrow_mut().report(e.pos, msg);
                }
            }

        } else {
            self.ctxt
                .diag
                .borrow_mut()
                .report(e.pos, Msg::LvalueExpected);
        }

        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn find_method(&mut self,
                   pos: Position,
                   object_type: BuiltinType,
                   is_static: bool,
                   name: Name,
                   args: &[BuiltinType],
                   return_type: Option<BuiltinType>)
                   -> Option<(ClassId, FctId, BuiltinType)> {
        let result = lookup_method(self.ctxt, object_type, is_static, name, args, return_type);

        if result.is_none() {
            let type_name = object_type.name(self.ctxt);
            let name = self.ctxt.interner.str(name).to_string();
            let param_names = args.iter()
                .map(|a| a.name(self.ctxt))
                .collect::<Vec<String>>();
            let msg = if is_static {
                Msg::UnknownStaticMethod(type_name, name, param_names)
            } else {
                Msg::UnknownMethod(type_name, name, param_names)
            };

            self.ctxt.diag.borrow_mut().report(pos, msg);
        }

        result
    }

    fn check_expr_un(&mut self, e: &'ast ExprUnType) {
        if e.op == UnOp::Neg {
            if self.negative_expr_id != e.id {
                self.negative_expr_id = e.opnd.id();
            }
        }

        self.visit_expr(&e.opnd);
        let opnd = self.expr_type;

        match e.op {
            UnOp::Plus => self.check_expr_un_method(e, e.op, "unaryPlus", opnd),
            UnOp::Neg => self.check_expr_un_method(e, e.op, "unaryMinus", opnd),
            UnOp::Not => self.check_expr_un_method(e, e.op, "not", opnd),
        }
    }

    fn check_expr_un_method(&mut self,
                            e: &'ast ExprUnType,
                            op: UnOp,
                            name: &str,
                            ty: BuiltinType) {
        let name = self.ctxt.interner.intern(name);
        let call_types = [];

        if let Some((cls_id, fct_id, return_type)) =
            lookup_method(self.ctxt, ty, false, name, &call_types, None) {

            let call_type = CallType::Method(cls_id, fct_id);
            self.src.map_calls.insert(e.id, call_type);

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;

        } else {
            let ty = ty.name(self.ctxt);
            let msg = Msg::UnOpType(op.as_str().into(), ty);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
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
            BinOp::Add => self.check_expr_bin_method(e, e.op, "plus", lhs_type, rhs_type),
            BinOp::Sub => self.check_expr_bin_method(e, e.op, "minus", lhs_type, rhs_type),
            BinOp::Mul => self.check_expr_bin_method(e, e.op, "times", lhs_type, rhs_type),
            BinOp::Div => self.check_expr_bin_method(e, e.op, "div", lhs_type, rhs_type),
            BinOp::Mod => self.check_expr_bin_method(e, e.op, "mod", lhs_type, rhs_type),
            BinOp::BitOr => self.check_expr_bin_method(e, e.op, "bitwiseOr", lhs_type, rhs_type),
            BinOp::BitAnd => self.check_expr_bin_method(e, e.op, "bitwiseAnd", lhs_type, rhs_type),
            BinOp::BitXor => self.check_expr_bin_method(e, e.op, "bitwiseXor", lhs_type, rhs_type),
            BinOp::ShiftL => self.check_expr_bin_method(e, e.op, "shiftLeft", lhs_type, rhs_type),
            BinOp::ShiftR => self.check_expr_bin_method(e, e.op, "shiftRight", lhs_type, rhs_type),
            BinOp::UnShiftR => {
                self.check_expr_bin_method(e, e.op, "unsignedShiftRight", lhs_type, rhs_type)
            }
        }
    }

    fn check_expr_bin_bool(&mut self,
                           e: &'ast ExprBinType,
                           op: BinOp,
                           lhs_type: BuiltinType,
                           rhs_type: BuiltinType) {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Bool);
        self.src.set_ty(e.id, BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_expr_bin_method(&mut self,
                             e: &'ast ExprBinType,
                             op: BinOp,
                             name: &str,
                             lhs_type: BuiltinType,
                             rhs_type: BuiltinType) {
        let name = self.ctxt.interner.intern(name);
        let call_types = [rhs_type];

        if let Some((cls_id, fct_id, return_type)) =
            lookup_method(self.ctxt, lhs_type, false, name, &call_types, None) {

            let call_type = CallType::Method(cls_id, fct_id);
            self.src.map_calls.insert_or_replace(e.id, call_type);

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;

        } else {
            let lhs_type = lhs_type.name(self.ctxt);
            let rhs_type = rhs_type.name(self.ctxt);
            let msg = Msg::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn check_expr_bin_cmp(&mut self,
                          e: &'ast ExprBinType,
                          cmp: CmpOp,
                          lhs_type: BuiltinType,
                          rhs_type: BuiltinType) {
        match cmp {
            CmpOp::Is | CmpOp::IsNot => {
                if !lhs_type.reference_type() {
                    let lhs_type = lhs_type.name(self.ctxt);
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(e.pos, Msg::ReferenceTypeExpected(lhs_type));
                }

                if !rhs_type.reference_type() {
                    let rhs_type = rhs_type.name(self.ctxt);
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(e.pos, Msg::ReferenceTypeExpected(rhs_type));
                }

                if !(lhs_type.is_nil() || lhs_type.allows(self.ctxt, rhs_type)) &&
                   !(rhs_type.is_nil() || rhs_type.allows(self.ctxt, lhs_type)) {
                    let lhs_type = lhs_type.name(self.ctxt);
                    let rhs_type = rhs_type.name(self.ctxt);
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(e.pos, Msg::TypesIncompatible(lhs_type, rhs_type));
                }

                self.src.set_ty(e.id, BuiltinType::Bool);
                self.expr_type = BuiltinType::Bool;
                return;
            }

            CmpOp::Eq | CmpOp::Ne => {
                self.check_expr_bin_method(e, e.op, "equals", lhs_type, rhs_type)
            }

            _ => self.check_expr_bin_method(e, e.op, "compareTo", lhs_type, rhs_type),
        }

        self.src.set_ty(e.id, BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_type(&mut self,
                  e: &'ast ExprBinType,
                  op: BinOp,
                  lhs_type: BuiltinType,
                  rhs_type: BuiltinType,
                  expected_type: BuiltinType) {
        if !expected_type.allows(self.ctxt, lhs_type) ||
           !expected_type.allows(self.ctxt, rhs_type) {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name(self.ctxt);
            let rhs_type = rhs_type.name(self.ctxt);
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType, in_try: bool) {
        if e.object.is_some() {
            self.check_method_call(e, in_try);
            return;
        }

        let call_types: Vec<BuiltinType> = e.args
            .iter()
            .map(|arg| {
                     self.visit_expr(arg);
                     self.expr_type
                 })
            .collect();

        let call_type = if e.path.len() > 1 {
            match self.ctxt.sym.borrow().get(e.path[0]) {
                Some(SymClass(cls_id)) => {
                    let cls = self.ctxt.classes[cls_id].borrow();

                    assert_eq!(2, e.path.len());

                    if let Some((_, fct_id, _)) =
                        self.find_method(e.pos, cls.ty, true, e.path[1], &call_types, None) {
                        let call_type = CallType::Fct(fct_id);
                        self.src.map_calls.insert(e.id, call_type);

                        call_type

                    } else {
                        self.expr_type = BuiltinType::Unit;
                        return;
                    }
                }

                _ => {
                    let name = self.ctxt.interner.str(e.path[0]).to_string();
                    let msg = Msg::ClassExpected(name);
                    self.ctxt.diag.borrow_mut().report(e.pos, msg);

                    self.expr_type = BuiltinType::Unit;
                    return;
                }
            }
        } else {
            *self.src.map_calls.get(e.id).unwrap()
        };

        match call_type {
            CallType::CtorNew(cls_id, _) => {
                self.check_expr_call_ctor(e, cls_id, call_types);
            }

            CallType::Fct(callee_id) => {
                self.check_expr_call_fct(e, callee_id, call_types);
            }

            _ => panic!("invocation of method"),
        }

        if !in_try {
            let fct_id = call_type.fct_id();
            let throws = self.ctxt.fcts[fct_id].borrow().throws;

            if throws {
                let msg = Msg::ThrowingCallWithoutTry;
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }
        }
    }

    fn check_expr_call_ctor(&mut self,
                            e: &'ast ExprCallType,
                            mut cls_id: ClassId,
                            call_types: Vec<BuiltinType>) {
        let mut ty = BuiltinType::Class(cls_id);

        if let Some(ref type_params) = e.type_params {
            let mut types = Vec::new();

            for type_param in type_params {
                let ty = self.src.ty(type_param.id());

                let ty = match self.fct.parent {
                    FctParent::Class(cls_id) => {
                        let cls = self.ctxt.classes[cls_id].borrow();

                        if cls.specialization_for.is_some() {
                            specialize::specialize_type(self.ctxt,
                                                        ty,
                                                        SpecializeFor::Class,
                                                        &cls.specialization_params)
                        } else {
                            ty
                        }
                    }

                    FctParent::None => {
                        if self.fct.specialization_for.is_some() {
                            specialize::specialize_type(self.ctxt,
                                                        ty,
                                                        SpecializeFor::Fct,
                                                        &self.fct.specialization_params)
                        } else {
                            ty
                        }
                    }

                    _ => ty,
                };

                types.push(ty);
            }

            let cls = self.ctxt.classes[cls_id].borrow();

            if cls.type_params.len() != types.len() {
                let msg = Msg::WrongNumberTypeParams(cls.type_params.len(), types.len());
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }

            let (specialized_cls_id, type_id) =
                specialize::specialize_class(self.ctxt, &*cls, types.clone());

            cls_id = specialized_cls_id;
            ty = BuiltinType::Generic(type_id);

        } else {
            let cls = self.ctxt.classes[cls_id].borrow();

            if cls.type_params.len() > 0 {
                let msg = Msg::WrongNumberTypeParams(cls.type_params.len(), 0);
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }
        }

        let cls = self.ctxt.classes[cls_id].borrow();
        let mut found = false;

        if cls.is_abstract {
            let msg = Msg::NewAbstractClass;
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        for &ctor in &cls.ctors {
            let ctor = self.ctxt.fcts[ctor].borrow();

            if args_compatible(self.ctxt, &ctor.params_without_self(), &call_types) {
                let call_type = CallType::CtorNew(cls_id, ctor.id);
                self.src.map_calls.replace(e.id, call_type);

                found = true;
                break;
            }
        }

        if !found {
            let call_types = call_types
                .iter()
                .map(|a| a.name(self.ctxt))
                .collect::<Vec<_>>();
            let name = self.ctxt.interner.str(cls.name).to_string();
            let msg = Msg::UnknownCtor(name, call_types);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_call_fct(&mut self,
                           e: &'ast ExprCallType,
                           mut callee_id: FctId,
                           call_types: Vec<BuiltinType>) {
        let callee_type_params_len = if self.fct.id == callee_id {
            self.fct.type_params.len()
        } else {
            let callee = self.ctxt.fcts[callee_id].borrow();

            callee.type_params.len()
        };

        if let Some(ref type_params) = e.type_params {
            let mut types = Vec::new();

            for type_param in type_params {
                let ty = self.src.ty(type_param.id());
                types.push(ty);
            }

            if callee_type_params_len != types.len() {
                let msg = Msg::WrongNumberTypeParams(callee_type_params_len, types.len());
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }

            let mut callee = self.ctxt.fcts[callee_id].borrow_mut();
            callee_id = specialize::specialize_fct(self.ctxt,
                                                   FctParent::None,
                                                   &mut *callee,
                                                   SpecializeFor::Fct,
                                                   &types);

            self.src
                .map_calls
                .insert_or_replace(e.id, CallType::Fct(callee_id));
        } else {
            if callee_type_params_len > 0 {
                let msg = Msg::WrongNumberTypeParams(callee_type_params_len, 0);
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
            }
        }

        let callee = self.ctxt.fcts[callee_id].borrow();

        self.src.set_ty(e.id, callee.return_type);
        self.expr_type = callee.return_type;

        if !args_compatible(self.ctxt, &callee.params_without_self(), &call_types) {
            let callee_name = self.ctxt.interner.str(callee.name).to_string();
            let callee_params = callee
                .params_without_self()
                .iter()
                .map(|a| a.name(self.ctxt))
                .collect::<Vec<_>>();
            let call_types = call_types
                .iter()
                .map(|a| a.name(self.ctxt))
                .collect::<Vec<_>>();
            let msg = Msg::ParamTypesIncompatible(callee_name, callee_params, call_types);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }
    }

    fn check_expr_delegation(&mut self, e: &'ast ExprDelegationType) {
        let arg_types: Vec<BuiltinType> = e.args
            .iter()
            .map(|arg| {
                     self.visit_expr(arg);
                     self.expr_type
                 })
            .collect();

        let owner = self.ctxt.classes[self.fct.cls_id()].borrow();

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

        let cls = self.ctxt.classes[cls_id].borrow();

        for &ctor_id in &cls.ctors {
            let ctor = self.ctxt.fcts[ctor_id].borrow();

            if args_compatible(self.ctxt, &ctor.params_without_self(), &arg_types) {
                self.src.map_cls.insert(e.id, cls.id);

                let call_type = CallType::Ctor(cls.id, ctor.id);
                self.src.map_calls.insert(e.id, call_type);
                return;
            }
        }

        let name = self.ctxt.interner.str(cls.name).to_string();
        let arg_types = arg_types.iter().map(|t| t.name(self.ctxt)).collect();
        let msg = Msg::UnknownCtor(name, arg_types);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
    }

    fn check_method_call(&mut self, e: &'ast ExprCallType, in_try: bool) {
        let object = e.object.as_ref().unwrap();

        let object_type = if object.is_super() {
            self.super_type(e.pos)

        } else {
            self.visit_expr(object);

            self.expr_type
        };

        let call_types: Vec<BuiltinType> = e.args
            .iter()
            .map(|arg| {
                     self.visit_expr(arg);
                     self.expr_type
                 })
            .collect();

        if let Some((cls_id, fct_id, return_type)) =
            self.find_method(e.pos, object_type, false, e.path.name(), &call_types, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            self.src.map_calls.insert(e.id, call_type);
            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;

            if !in_try {
                let throws = self.ctxt.fcts[fct_id].borrow().throws;

                if throws {
                    let msg = Msg::ThrowingCallWithoutTry;
                    self.ctxt.diag.borrow_mut().report(e.pos, msg);
                }
            }
        } else {
            self.src.set_ty(e.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn super_type(&self, pos: Position) -> BuiltinType {
        if let FctParent::Class(clsid) = self.fct.parent {
            let cls = self.ctxt.classes[clsid].borrow();

            if let Some(superid) = cls.parent_class {
                return self.ctxt.classes[superid].borrow().ty;
            }
        }

        let msg = Msg::SuperUnavailable;
        self.ctxt.diag.borrow_mut().report(pos, msg);

        BuiltinType::Unit
    }

    fn check_expr_field(&mut self, e: &'ast ExprFieldType) {
        self.visit_expr(&e.object);

        let ty = self.expr_type.to_specialized(self.ctxt);

        if let BuiltinType::Class(cls_id) = ty {
            let cls = self.ctxt.classes[cls_id].borrow();

            if let Some((cls_id, field_id)) = cls.find_field(self.ctxt, e.name) {
                let cls = self.ctxt.classes[cls_id].borrow();
                let ident_type = IdentType::Field(cls_id, field_id);
                self.src.map_idents.insert_or_replace(e.id, ident_type);

                let field = &cls.fields[field_id];
                self.src.set_ty(e.id, field.ty);
                self.expr_type = field.ty;
                return;
            }
        }

        // field not found, report error
        let field_name = self.ctxt.interner.str(e.name).to_string();
        let expr_name = ty.name(self.ctxt);
        let msg = Msg::UnknownField(field_name, expr_name);
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        // we don't know the type of the field, just assume ()
        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_this(&mut self, e: &'ast ExprSelfType) {
        match self.fct.parent {
            FctParent::Class(clsid) => {
                let ty = self.ctxt.classes[clsid].borrow().ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.ctxt.impls[impl_id].borrow();
                let ty = self.ctxt.classes[ximpl.cls_id()].borrow().ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            _ => {
                let msg = Msg::ThisUnavailable;
                self.ctxt.diag.borrow_mut().report(e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
            }
        }
    }

    fn check_expr_super(&mut self, e: &'ast ExprSuperType) {
        let msg = Msg::SuperNeedsMethodCall;
        self.ctxt.diag.borrow_mut().report(e.pos, msg);
        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_nil(&mut self, e: &'ast ExprNilType) {
        self.src.set_ty(e.id, BuiltinType::Nil);
        self.expr_type = BuiltinType::Nil;
    }

    fn check_expr_array(&mut self, e: &'ast ExprArrayType) {
        self.visit_expr(&e.object);
        let object_type = self.expr_type;

        self.visit_expr(&e.index);
        let index_type = self.expr_type;

        let name = self.ctxt.interner.intern("get");
        let args = vec![index_type];

        if let Some((cls_id, fct_id, return_type)) =
            self.find_method(e.pos, object_type, false, name, &args, None) {
            let call_type = CallType::Method(cls_id, fct_id);
            self.src.map_calls.insert(e.id, call_type);

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            self.src.set_ty(e.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
        }
    }

    fn check_expr_try(&mut self, e: &'ast ExprTryType) {
        if let Some(call) = e.expr.to_call() {
            self.check_expr_call(call, true);
            let e_type = self.expr_type;
            self.src.set_ty(e.id, e_type);

            if let Some(call_type) = self.src.map_calls.get(call.id) {
                let fct_id = call_type.fct_id();
                let throws = self.ctxt.fcts[fct_id].borrow().throws;

                if !throws {
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(e.pos, Msg::TryCallNonThrowing);
                }
            }


            match e.mode {
                TryMode::Normal => {}
                TryMode::Else(ref alt_expr) => {
                    self.visit_expr(alt_expr);
                    let alt_type = self.expr_type;

                    if !e_type.allows(self.ctxt, alt_type) {
                        let e_type = e_type.name(self.ctxt);
                        let alt_type = alt_type.name(self.ctxt);
                        let msg = Msg::TypesIncompatible(e_type, alt_type);
                        self.ctxt.diag.borrow_mut().report(e.pos, msg);
                    }
                }

                TryMode::Force => {}
                TryMode::Opt => panic!("unsupported"),
            }

            self.expr_type = e_type;
        } else {
            self.ctxt
                .diag
                .borrow_mut()
                .report(e.pos, Msg::TryNeedsCall);

            self.expr_type = BuiltinType::Unit;
            self.src.set_ty(e.id, BuiltinType::Unit);
        }
    }

    fn check_expr_lambda(&mut self, e: &'ast ExprLambdaType) {
        let ret = if let Some(ref ty) = e.ret {
            self.src.ty(ty.id())
        } else {
            BuiltinType::Unit
        };

        let params = e.params.iter().map(|p| self.src.ty(p.data_type.id())).collect::<Vec<_>>();

        let ty = self.ctxt.lambda_types.borrow_mut().insert(params, ret);
        let ty = BuiltinType::Lambda(ty);

        self.expr_type = ty;
        self.src.set_ty(e.id, ty);
    }

    fn check_expr_conv(&mut self, e: &'ast ExprConvType) {
        self.visit_expr(&e.object);
        let object_type = self.expr_type;
        self.src.set_ty(e.object.id(), self.expr_type);

        let check_type = self.src.ty(e.data_type.id());

        if !check_type.reference_type() {
            let name = check_type.name(self.ctxt);
            self.ctxt
                .diag
                .borrow_mut()
                .report(e.pos, Msg::ReferenceTypeExpected(name));
            return;
        }

        let mut valid = false;

        if object_type.subclass_from(self.ctxt, check_type) {
            // open class A { } class B: A { }
            // (b is A) is valid

            valid = true;

        } else if check_type.subclass_from(self.ctxt, object_type) {
            // normal check

        } else {
            let object_type = object_type.name(self.ctxt);
            let check_type = check_type.name(self.ctxt);
            let msg = Msg::TypesIncompatible(object_type, check_type);
            self.ctxt.diag.borrow_mut().report(e.pos, msg);
        }

        self.src
            .map_convs
            .insert(e.id,
                    ConvInfo {
                        cls_id: check_type.cls_id(),
                        valid: valid,
                    });

        self.expr_type = if e.is { BuiltinType::Bool } else { check_type };
    }

    fn check_expr_lit_struct(&mut self, e: &'ast ExprLitStructType) {
        let sid = self.src.map_idents.get(e.id).unwrap().struct_id();

        let ty = BuiltinType::Struct(sid);
        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_int(&mut self, e: &'ast ExprLitIntType) {
        let (ty, _) = check_lit_int(self.ctxt, e, self.negative_expr_id);

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_float(&mut self, e: &'ast ExprLitFloatType) {
        let (ty, _) = check_lit_float(self.ctxt, e, self.negative_expr_id);

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitChar(ExprLitCharType { id, .. }) => {
                self.src.set_ty(id, BuiltinType::Char);
                self.expr_type = BuiltinType::Char;
            }
            ExprLitInt(ref expr) => self.check_expr_lit_int(expr),
            ExprLitFloat(ref expr) => self.check_expr_lit_float(expr),
            ExprLitStr(ExprLitStrType { id, .. }) => {
                let str_ty = BuiltinType::Class(self.ctxt.primitive_classes.str_class);
                self.src.set_ty(id, str_ty);
                self.expr_type = str_ty;
            }
            ExprLitBool(ExprLitBoolType { id, .. }) => {
                self.src.set_ty(id, BuiltinType::Bool);
                self.expr_type = BuiltinType::Bool;
            }
            ExprLitStruct(ref expr) => self.check_expr_lit_struct(expr),
            ExprIdent(ref expr) => self.check_expr_ident(expr),
            ExprAssign(ref expr) => self.check_expr_assign(expr),
            ExprUn(ref expr) => self.check_expr_un(expr),
            ExprBin(ref expr) => self.check_expr_bin(expr),
            ExprCall(ref expr) => self.check_expr_call(expr, false),
            ExprDelegation(ref expr) => self.check_expr_delegation(expr),
            ExprField(ref expr) => self.check_expr_field(expr),
            ExprSelf(ref expr) => self.check_expr_this(expr),
            ExprSuper(ref expr) => self.check_expr_super(expr),
            ExprNil(ref expr) => self.check_expr_nil(expr),
            ExprArray(ref expr) => self.check_expr_array(expr),
            ExprConv(ref expr) => self.check_expr_conv(expr),
            ExprTry(ref expr) => self.check_expr_try(expr),
            ExprLambda(ref expr) => self.check_expr_lambda(expr),
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtIf(ref stmt) => self.check_stmt_if(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),
            StmtThrow(ref stmt) => self.check_stmt_throw(stmt),
            StmtSpawn(_) => unimplemented!(),
            StmtDefer(ref stmt) => self.check_stmt_defer(stmt),
            StmtDo(ref stmt) => self.check_stmt_do(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(_) => visit::walk_stmt(self, s),
            StmtContinue(_) => visit::walk_stmt(self, s),
            StmtLoop(_) => visit::walk_stmt(self, s),
            StmtExpr(_) => visit::walk_stmt(self, s),
            StmtBlock(_) => visit::walk_stmt(self, s),
        }
    }
}

fn args_compatible(ctxt: &SemContext, def: &[BuiltinType], expr: &[BuiltinType]) -> bool {
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

fn check_lit_int<'ast>(ctxt: &SemContext<'ast>,
                       e: &'ast ExprLitIntType,
                       negative_expr_id: NodeId)
                       -> (BuiltinType, i64) {
    let ty = match e.suffix {
        IntSuffix::Byte => BuiltinType::Byte,
        IntSuffix::Int => BuiltinType::Int,
        IntSuffix::Long => BuiltinType::Long,
    };

    let ty_name = match e.suffix {
        IntSuffix::Byte => "byte",
        IntSuffix::Int => "int",
        IntSuffix::Long => "long",
    };

    let val = e.value;
    let negative = e.suffix != IntSuffix::Byte && negative_expr_id == e.id;

    if e.base == IntBase::Dec {
        let max = match e.suffix {
            IntSuffix::Byte => 256,
            IntSuffix::Int => (1u64 << 31),
            IntSuffix::Long => (1u64 << 63),
        };

        if (negative && val > max) || (!negative && val >= max) {
            ctxt.diag
                .borrow_mut()
                .report(e.pos, Msg::NumberOverflow(ty_name.into()));
        }
    } else {
        let max = match e.suffix {
            IntSuffix::Byte => 256 as u64,
            IntSuffix::Int => u32::max_value() as u64,
            IntSuffix::Long => u64::max_value() as u64,
        };

        if val > max {
            ctxt.diag
                .borrow_mut()
                .report(e.pos, Msg::NumberOverflow(ty_name.into()));
        }
    }

    let val = if negative {
        (!val + 1) as i64
    } else {
        val as i64
    };

    (ty, val)
}

fn check_lit_float<'ast>(ctxt: &SemContext<'ast>,
                         e: &'ast ExprLitFloatType,
                         negative_expr_id: NodeId)
                         -> (BuiltinType, f64) {
    let ty = match e.suffix {
        FloatSuffix::Float => BuiltinType::Float,
        FloatSuffix::Double => BuiltinType::Double,
    };

    let (min, max) = match e.suffix {
        FloatSuffix::Float => (f32::MIN as f64, f32::MAX as f64),
        FloatSuffix::Double => (f64::MIN, f64::MAX),
    };

    let value = if negative_expr_id == e.id {
        -e.value
    } else {
        e.value
    };

    if value < min || value > max {
        let ty = match e.suffix {
            FloatSuffix::Float => "float",
            FloatSuffix::Double => "double",
        };

        ctxt.diag
            .borrow_mut()
            .report(e.pos, Msg::NumberOverflow(ty.into()));
    }

    (ty, value)
}

struct ConstCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    xconst: &'a ConstData<'ast>,
    negative_expr_id: NodeId,
}

impl<'a, 'ast> ConstCheck<'a, 'ast> {
    fn check_expr(&mut self, expr: &'ast Expr) -> (BuiltinType, ConstValue) {
        let (ty, lit) = match expr {
            &ExprLitChar(ref expr) => (BuiltinType::Char, ConstValue::Char(expr.value)),
            &ExprLitInt(ref expr) => {
                let (ty, val) = check_lit_int(self.ctxt, expr, self.negative_expr_id);
                (ty, ConstValue::Int(val))
            }
            &ExprLitFloat(ref expr) => {
                let (ty, val) = check_lit_float(self.ctxt, expr, self.negative_expr_id);
                (ty, ConstValue::Float(val))
            }
            &ExprLitBool(ref expr) => (BuiltinType::Bool, ConstValue::Bool(expr.value)),

            &ExprUn(ref expr) if expr.op == UnOp::Neg => {
                if self.negative_expr_id != expr.id {
                    self.negative_expr_id = expr.opnd.id();
                }

                let (ty, val) = self.check_expr(&expr.opnd);
                let name = self.ctxt.interner.intern("unaryMinus");

                if lookup_method(self.ctxt, ty, false, name, &[], Some(ty)).is_none() {
                    let ty = ty.name(self.ctxt);
                    let msg = Msg::UnOpType(expr.op.as_str().into(), ty);

                    self.ctxt.diag.borrow_mut().report(expr.pos, msg);
                }

                return (ty, val);
            }

            _ => {
                let msg = Msg::ConstValueExpected;
                self.ctxt.diag.borrow_mut().report(expr.pos(), msg);
                return (BuiltinType::Unit, ConstValue::None);
            }
        };

        if !self.xconst.ty.allows(self.ctxt, ty) {
            let name = self.ctxt.interner.str(self.xconst.name).to_string();
            let const_ty = self.xconst.ty.name(self.ctxt);
            let ty = ty.name(self.ctxt);
            let msg = Msg::AssignType(name, const_ty, ty);
            self.ctxt.diag.borrow_mut().report(expr.pos(), msg);
        }

        (ty, lit)
    }
}

fn lookup_method<'ast>(ctxt: &SemContext<'ast>,
                       object_type: BuiltinType,
                       is_static: bool,
                       name: Name,
                       args: &[BuiltinType],
                       return_type: Option<BuiltinType>)
                       -> Option<(ClassId, FctId, BuiltinType)> {
    let cls_id = match object_type.to_specialized(ctxt) {
        BuiltinType::Class(cls_id) => Some(cls_id),
        _ => ctxt.primitive_classes.find_class(object_type),
    };

    if let Some(cls_id) = cls_id {
        let cls = ctxt.classes[cls_id].borrow();

        let candidates = cls.find_methods(ctxt, name, is_static);

        if candidates.len() == 1 {
            let candidate = candidates[0];
            let method = ctxt.fcts[candidate].borrow();

            let cls_id = match method.parent {
                FctParent::Class(cls_id) => cls_id,
                FctParent::Impl(impl_id) => {
                    let ximpl = ctxt.impls[impl_id].borrow();
                    ximpl.cls_id()
                }

                _ => unreachable!(),
            };

            if args_compatible(ctxt, &method.params_without_self(), args) &&
               (return_type.is_none() || method.return_type == return_type.unwrap()) {
                return Some((cls_id, candidate, method.return_type));
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use ctxt::ConstValue;
    use dora_parser::error::msg::Msg;
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
            pos(1, 46),
            Msg::ReturnType("bool".into(), "int".into()));

        parse_with_errors("class Foo(let a:int) fun f(x: Foo) -> int { return x.b; }",
                          |ctxt| {
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
        err("class Foo(let a: int) { fun f() -> int { return a; } }",
            pos(1, 49),
            Msg::UnknownIdentifier("a".into()));
        err("class Foo(var a: int) { fun set(x: int) { a = x; } }",
            pos(1, 43),
            Msg::UnknownIdentifier("a".into()));
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
            pos(5, 37),
            Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_method_defined_twice() {
        err("class Foo {
                 fun bar() {}
                 fun bar() {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)));

        err("class Foo {
                 fun bar() {}
                 fun bar() -> int {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)));

        err("class Foo {
                 fun bar(a: int) {}
                 fun bar(a: int) -> int {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)));

        err("class Foo {
                fun bar(a: int) {}
                fun bar(a: Str) {}
            }",
            pos(3, 17),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 17)));
    }

    #[test]
    fn type_self() {
        ok("class Foo { fun me() -> Foo { return self; } }");
        err("class Foo fun me() { return self; }",
            pos(1, 29),
            Msg::ThisUnavailable);

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
            Msg::UnknownMethod("Foo".into(), "bar".into(), vec!["int".into()]));
    }

    #[test]
    fn type_ctor() {
        ok("class Foo fun f() -> Foo { return Foo(); }");
        ok("class Foo(let a: int) fun f() -> Foo { return Foo(1); }");
        err("class Foo fun f() -> Foo { return 1; }",
            pos(1, 28),
            Msg::ReturnType("Foo".into(), "int".into()));
    }

    #[test]
    fn type_def_for_return_type() {
        ok("fun a() -> int { return 1; }");
        err("fun a() -> unknown {}",
            pos(1, 12),
            Msg::UnknownType("unknown".into()));
    }

    #[test]
    fn type_def_for_param() {
        ok("fun a(b: int) {}");
        err("fun a(b: foo) {}",
            pos(1, 10),
            Msg::UnknownType("foo".into()));
    }

    #[test]
    fn type_def_for_var() {
        ok("fun a() { let a : int = 1; }");
        err("fun a() { let a : test = 1; }",
            pos(1, 19),
            Msg::UnknownType("test".into()));
    }

    #[test]
    fn type_var_needs_expr_or_definition() {
        err("fun a() { let a; }",
            pos(1, 11),
            Msg::VarNeedsTypeInfo("a".into()));
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fun f() { let a : int = 1; }");
        ok("fun f() { let a : bool = false; }");
        ok("fun f() { let a : Str = \"f\"; }");

        err("fun f() { let a : int = true; }",
            pos(1, 11),
            Msg::AssignType("a".into(), "int".into(), "bool".into()));
        err("fun f() { let b : bool = 2; }",
            pos(1, 11),
            Msg::AssignType("b".into(), "bool".into(), "int".into()));
    }

    #[test]
    fn type_while() {
        ok("fun x() { while true { } }");
        ok("fun x() { while false { } }");
        err("fun x() { while 2 { } }",
            pos(1, 11),
            Msg::WhileCondType("int".into()));
    }

    #[test]
    fn type_if() {
        ok("fun x() { if true { } }");
        ok("fun x() { if false { } }");
        err("fun x() { if 4 { } }",
            pos(1, 11),
            Msg::IfCondType("int".into()));
    }

    #[test]
    fn type_return_unit() {
        ok("fun f() { return; }");
        err("fun f() { return 1; }",
            pos(1, 11),
            Msg::ReturnType("()".into(), "int".into()));
    }

    #[test]
    fn type_return() {
        ok("fun f() -> int { let a = 1; return a; }");
        ok("fun f() -> int { return 1; }");
        err("fun f() -> int { return; }",
            pos(1, 18),
            Msg::ReturnType("int".into(), "()".into()));

        ok("fun f() -> int { return 0; }
            fun g() -> int { return f(); }");
        err("fun f() { }
             fun g() -> int { return f(); }",
            pos(2, 31),
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
        ok("fun f(a: int) { !a; -a; +a; }");
        err("fun f(a: bool) { -a; }",
            pos(1, 18),
            Msg::UnOpType("-".into(), "bool".into()));
        err("fun f(a: bool) { +a; }",
            pos(1, 18),
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

        err("class A class B fun f(a: A, b: B) { a === b; }",
            pos(1, 39),
            Msg::TypesIncompatible("A".into(), "B".into()));
        err("class A class B fun f(a: A, b: B) { b !== a; }",
            pos(1, 39),
            Msg::TypesIncompatible("B".into(), "A".into()));
        err("fun f(a: bool) { a+a; }",
            pos(1, 19),
            Msg::BinOpType("+".into(), "bool".into(), "bool".into()));
        err("fun f(a: bool) { a^a; }",
            pos(1, 19),
            Msg::BinOpType("^".into(), "bool".into(), "bool".into()));
        err("fun f(a: int) { a||a; }",
            pos(1, 18),
            Msg::BinOpType("||".into(), "int".into(), "int".into()));
        err("fun f(a: int) { a&&a; }",
            pos(1, 18),
            Msg::BinOpType("&&".into(), "int".into(), "int".into()));
        err("fun f(a: Str) { a-a; }",
            pos(1, 18),
            Msg::BinOpType("-".into(), "Str".into(), "Str".into()));
        err("fun f(a: Str) { a*a; }",
            pos(1, 18),
            Msg::BinOpType("*".into(), "Str".into(), "Str".into()));
        err("fun f(a: Str) { a%a; }",
            pos(1, 18),
            Msg::BinOpType("%".into(), "Str".into(), "Str".into()));
    }

    #[test]
    fn type_function_return_type() {
        ok("fun foo() -> int { return 1; }\nfun f() { let i: int = foo(); }");
        err("fun foo() -> int { return 1; }\nfun f() { let i: bool = foo(); }",
            pos(2, 11),
            Msg::AssignType("i".into(), "bool".into(), "int".into()));
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

        err("fun foo() {}\nfun f() { foo(1); }",
            pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(), vec![], vec!["int".into()]));
        err("fun foo(a: int) {}\nfun f() { foo(true); }",
            pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(), vec!["int".into()], vec!["bool".into()]));
        err("fun foo(a: int, b: bool) {}\nfun f() { foo(1, 2); }",
            pos(2, 11),
            Msg::ParamTypesIncompatible("foo".into(),
                                        vec!["int".into(), "bool".into()],
                                        vec!["int".into(), "int".into()]));
    }

    #[test]
    fn type_return_nil() {
        ok("fun foo() -> Str { return nil; }");
        ok("class Foo fun foo() -> Foo { return nil; }");
        err("fun foo() -> int { return nil; }",
            pos(1, 20),
            Msg::IncompatibleWithNil("int".into()));
    }

    #[test]
    fn type_nil_as_argument() {
        ok("fun foo(a: Str) {} fun test() { foo(nil); }");
        err("fun foo(a: int) {} fun test() { foo(nil); }",
            pos(1, 33),
            Msg::ParamTypesIncompatible("foo".into(), vec!["int".into()], vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_ctor() {
        ok("class Foo(let a: Str) fun test() { Foo(nil); }");
        err("class Foo(let a: int) fun test() { Foo(nil); }",
            pos(1, 36),
            Msg::UnknownCtor("Foo".into(), vec!["nil".into()]));
    }

    #[test]
    fn type_nil_for_local_variable() {
        ok("fun f() { let x: Str = nil; }");
        err("fun f() { let x: int = nil; }",
            pos(1, 11),
            Msg::AssignType("x".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_for_field() {
        ok("class Foo(var a: Str) fun f() { Foo(nil).a = nil; }");
        err("class Foo(var a: int) fun f() { Foo(1).a = nil; }",
            pos(1, 42),
            Msg::AssignField("a".into(), "Foo".into(), "int".into(), "nil".into()));
    }

    #[test]
    fn type_nil_method() {
        err("fun f() { nil.test(); }",
            pos(1, 14),
            Msg::UnknownMethod("nil".into(), "test".into(), Vec::new()));
    }

    #[test]
    fn type_nil_as_method_argument() {
        ok("class Foo {
            fun f(a: Str) {}
        } fun f() { Foo().f(nil); }");
    }

    #[test]
    fn type_array() {
        ok("fun f(a: Array<int>) -> int { return a[1]; }");
        err("fun f(a: Array<int>) -> Str { return a[1]; }",
            pos(1, 31),
            Msg::ReturnType("Str".into(), "int".into()));
    }

    #[test]
    fn type_array_assign() {
        err("fun f(a: Array<int>) -> int { return a[3] = 4; }",
            pos(1, 31),
            Msg::ReturnType("int".into(), "()".into()));
        err("fun f(a: Array<int>) { a[3] = \"b\"; }",
            pos(1, 29),
            Msg::UnknownMethod("Array<int>".into(),
                               "set".into(),
                               vec!["int".into(), "Str".into()]));
    }

    #[test]
    fn type_throw() {
        ok("fun f() { throw \"abc\"; }");
        ok("fun f() { throw Array::<int>(); }");
        err("fun f() { throw 1; }",
            pos(1, 11),
            Msg::ReferenceTypeExpected("int".into()));
        err("fun f() { throw nil; }", pos(1, 11), Msg::ThrowNil);
    }

    #[test]
    fn type_defer() {
        ok("fun foo() { }
            fun f() { defer foo(); }");

        err("fun foo(a: int) {} fun f() { defer foo();}",
            pos(1, 36),
            Msg::ParamTypesIncompatible("foo".into(), vec!["int".into()], vec![]));

        err("fun f() { defer 1; }", pos(1, 11), Msg::FctCallExpected);
    }

    #[test]
    fn type_catch_variable() {
        ok("fun f() { do {} catch a: Str { print(a); } }");
        ok("fun f() { var x = 0; do {} catch a: Array<int> { x=a.len(); } }");
    }

    #[test]
    fn try_value_type() {
        err("fun f() { do {} catch a: int {} }",
            pos(1, 26),
            Msg::ReferenceTypeExpected("int".into()));
    }

    #[test]
    fn try_missing_catch() {
        err("fun f() { do {} }", pos(1, 11), Msg::CatchOrFinallyExpected);
    }

    #[test]
    fn try_check_blocks() {
        err("fun f() { do {} catch a: Array<int> {} a.len(); }",
            pos(1, 40),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { do {} catch a: Array<int> {} finally { a.len(); } }",
            pos(1, 50),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { do { return a; } catch a: Array<int> {} }",
            pos(1, 23),
            Msg::UnknownIdentifier("a".into()));
        err("fun f() { do { } catch a: Array<int> { return a; } }",
            pos(1, 40),
            Msg::ReturnType("()".into(), "Array<int>".into()));
    }

    #[test]
    fn let_without_initialization() {
        err("fun f() { let x: int; }",
            pos(1, 11),
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
            pos(1, 49),
            Msg::LetReassigned);
    }

    #[test]
    fn reassign_catch() {
        err("fun f() {
               do {
                 throw \"test\";
               } catch x: Array<int> {
                 x = Array::<int>();
               }
             }",
            pos(5, 20),
            Msg::LetReassigned);
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
        }",
            pos(2, 28),
            Msg::LvalueExpected);
    }

    #[test]
    fn super_class() {
        ok("open class A class B: A");
        ok("open class A class B: A()");
        ok("open class A(a: int) class B: A(1)");
        err("open class A(a: int) class B: A(true)",
            pos(1, 31),
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
             fun f(a: A) -> bool { return a is Str; }",
            pos(2, 45),
            Msg::TypesIncompatible("A".into(), "Str".into()));
        err("open class A class B: A class C
             fun f(a: A) -> bool { return a is C; }",
            pos(2, 45),
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
             fun f(a: A) -> Str { return a as Str; }",
            pos(2, 44),
            Msg::TypesIncompatible("A".into(), "Str".into()));
        err("open class A class B: A class C
             fun f(a: A) -> C { return a as C; }",
            pos(2, 42),
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
            class B: A { fun me() { let x = super; } }",
            pos(2, 45),
            Msg::SuperNeedsMethodCall);
    }

    #[test]
    fn try_with_non_call() {
        err("fun me() { try 1; }", pos(1, 12), Msg::TryNeedsCall);
    }

    #[test]
    fn try_fct() {
        ok("fun one() throws -> int { return 1; } fun me() -> int { return try one(); }");
    }

    #[test]
    fn throws_fct_without_try() {
        err("fun one() throws -> int { return 1; } fun me() -> int { return one(); }",
            pos(1, 64),
            Msg::ThrowingCallWithoutTry);
    }

    #[test]
    fn try_fct_non_throwing() {
        err("fun one() -> int { return 1; }
             fun me() -> int { return try one(); }",
            pos(2, 39),
            Msg::TryCallNonThrowing);
    }

    #[test]
    fn try_method() {
        ok("class Foo { fun one() throws -> int { return 1; } }
            fun me() -> int { return try Foo().one(); }");
    }

    #[test]
    fn throws_method_without_try() {
        err("class Foo { fun one() throws -> int { return 1; } }
             fun me() -> int { return Foo().one(); }",
            pos(2, 44),
            Msg::ThrowingCallWithoutTry);
    }

    #[test]
    fn try_method_non_throwing() {
        err("class Foo { fun one() -> int { return 1; } }
             fun me() -> int { return try Foo().one(); }",
            pos(2, 39),
            Msg::TryCallNonThrowing);
    }

    #[test]
    fn try_else() {
        ok("fun one() throws -> int { return 1; }
            fun me() -> int { return try one() else 0; }");
        err("fun one() throws -> int { return 1; }
             fun me() -> int { return try one() else \"bla\"; }",
            pos(2, 39),
            Msg::TypesIncompatible("int".into(), "Str".into()));
        err("fun one() throws -> int { return 1; }
             fun me() -> int { return try one() else false; }",
            pos(2, 39),
            Msg::TypesIncompatible("int".into(), "bool".into()));
    }

    #[test]
    fn struct_lit() {
        ok("struct Foo {} fun foo() -> Foo { return Foo; }");
        ok("struct Foo {} fun foo() { let x = Foo; }");
        ok("struct Foo {} fun foo() { let x: Foo = Foo; }");
        err("struct Foo {} fun foo() { let x: int = Foo; }",
            pos(1, 27),
            Msg::AssignType("x".into(), "int".into(), "Foo".into()));
        err("struct Foo {} fun foo() -> int { return Foo; }",
            pos(1, 34),
            Msg::ReturnType("int".into(), "Foo".into()));
    }

    #[test]
    fn lit_long() {
        ok("fun f() -> long { return 1L; }");
        ok("fun f() -> int { return 1; }");

        let ret = Msg::ReturnType("int".into(), "long".into());
        err("fun f() -> int { return 1L; }", pos(1, 18), ret);

        let ret = Msg::ReturnType("long".into(), "int".into());
        err("fun f() -> long { return 1; }", pos(1, 19), ret);
    }

    #[test]
    fn overload_plus() {
        ok("class A { fun plus(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() + A(); }");
    }

    #[test]
    fn overload_minus() {
        ok("class A { fun minus(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() - A(); }");
    }

    #[test]
    fn overload_times() {
        ok("class A { fun times(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() * A(); }");
    }

    #[test]
    fn overload_div() {
        ok("class A { fun div(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() / A(); }");
    }

    #[test]
    fn overload_mod() {
        ok("class A { fun mod(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() % A(); }");
    }

    #[test]
    fn overload_bitwise_or() {
        ok("class A { fun bitwiseOr(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() | A(); }");
    }

    #[test]
    fn overload_bitwise_and() {
        ok("class A { fun bitwiseAnd(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() & A(); }");
    }

    #[test]
    fn overload_bitwise_xor() {
        ok("class A { fun bitwiseXor(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() ^ A(); }");
    }

    #[test]
    fn overload_shl() {
        ok("class A { fun shiftLeft(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() << A(); }");
    }

    #[test]
    fn overload_sar() {
        ok("class A { fun shiftRight(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() >> A(); }");
    }

    #[test]
    fn overload_shr() {
        ok("class A { fun unsignedShiftRight(rhs: A) -> int { return 0; } }
            fun f() -> int { return A() >>> A(); }");
    }

    #[test]
    fn overload_equals() {
        ok("class A { fun equals(rhs: A) -> bool { return true; } }
            fun f1() -> bool { return A() == A(); }
            fun f2() -> bool { return A() != A(); }");
    }

    #[test]
    fn overload_compare_to() {
        ok("class A { fun compareTo(rhs: A) -> int { return 0; } }
            fun f1() -> bool { return A() < A(); }
            fun f2() -> bool { return A() <= A(); }
            fun f3() -> bool { return A() > A(); }
            fun f4() -> bool { return A() >= A(); }");
    }

    #[test]
    fn long_operations() {
        ok("fun f(a: long, b: long) -> long { return a + b; }");
        ok("fun f(a: long, b: long) -> long { return a - b; }");
        ok("fun f(a: long, b: long) -> long { return a * b; }");
        ok("fun f(a: long, b: long) -> long { return a / b; }");
        ok("fun f(a: long, b: long) -> long { return a % b; }");
        ok("fun f(a: long, b: long) -> long { return a | b; }");
        ok("fun f(a: long, b: long) -> long { return a & b; }");
        ok("fun f(a: long, b: long) -> long { return a ^ b; }");
        ok("fun f(a: long, b: long) -> long { return a << b; }");
        ok("fun f(a: long, b: long) -> long { return a >> b; }");
        ok("fun f(a: long, b: long) -> long { return a >>> b; }");
        ok("fun f(a: long, b: long) -> bool { return a == b; }");
        ok("fun f(a: long, b: long) -> bool { return a != b; }");
        ok("fun f(a: long, b: long) -> bool { return a < b; }");
        ok("fun f(a: long, b: long) -> bool { return a <= b; }");
        ok("fun f(a: long, b: long) -> bool { return a > b; }");
        ok("fun f(a: long, b: long) -> bool { return a >= b; }");
        ok("fun f(a: long) -> long { return !a; }");
        ok("fun f(a: long) -> long { return -a; }");
        ok("fun f(a: long) -> long { return +a; }");
    }

    #[test]
    fn test_literal_int_overflow() {
        err("fun f() { let x = 2147483648; }",
            pos(1, 19),
            Msg::NumberOverflow("int".into()));
        ok("fun f() { let x = 2147483647; }");
        err("fun f() { let x = -2147483649; }",
            pos(1, 20),
            Msg::NumberOverflow("int".into()));
        ok("fun f() { let x = -2147483648; }");
    }

    #[test]
    fn test_literal_hex_int_overflow() {
        err("fun f() { let x = 0x1_FF_FF_FF_FF; }",
            pos(1, 19),
            Msg::NumberOverflow("int".into()));
        ok("fun f() { let x: int = 0xFF_FF_FF_FF; }");
    }

    #[test]
    fn test_literal_bin_int_overflow() {
        err("fun f() { let x = 0b1_11111111_11111111_11111111_11111111; }",
            pos(1, 19),
            Msg::NumberOverflow("int".into()));
        ok("fun f() { let x: int = 0b11111111_11111111_11111111_11111111; }");
    }

    #[test]
    fn test_literal_long_overflow() {
        err("fun f() { let x = 9223372036854775808L; }",
            pos(1, 19),
            Msg::NumberOverflow("long".into()));
        ok("fun f() { let x = 9223372036854775807L; }");
        err("fun f() { let x = -9223372036854775809L; }",
            pos(1, 20),
            Msg::NumberOverflow("long".into()));
        ok("fun f() { let x = -9223372036854775808L; }");
    }

    #[test]
    fn test_literal_float_overflow() {
        err("fun f() { let x = -340282350000000000000000000000000000000F; }",
            pos(1, 20),
            Msg::NumberOverflow("float".into()));
        ok("fun f() { let x = -340282340000000000000000000000000000000F; }");
        err("fun f() { let x = 340282350000000000000000000000000000001F; }",
            pos(1, 19),
            Msg::NumberOverflow("float".into()));
        ok("fun f() { let x = 340282340000000000000000000000000000000F; }");
    }

    #[test]
    fn test_char() {
        ok("fun foo() -> char { return 'c'; }");
        ok("fun foo(a: char) -> char { return a; }");
        err("fun foo() -> char { return false; }",
            pos(1, 21),
            Msg::ReturnType("char".into(), "bool".into()));
        err("fun foo() -> char { return 10; }",
            pos(1, 21),
            Msg::ReturnType("char".into(), "int".into()));
    }

    #[test]
    fn test_generic_arguments_mismatch() {
        err("class A<T>
            fun foo() {
                let a = A::<int, int>();
            }",
            pos(3, 25),
            Msg::WrongNumberTypeParams(1, 2));

        err("class A<T>
            fun foo() {
                let a = A();
            }",
            pos(3, 25),
            Msg::WrongNumberTypeParams(1, 0));

        err("class A
            fun foo() {
                let a = A::<int>();
            }",
            pos(3, 25),
            Msg::WrongNumberTypeParams(0, 1));
    }

    #[test]
    fn test_invoke_static_method_as_instance_method() {
        err("class A {
                static fun foo() {}
                fun test() { self.foo(); }
            }",
            pos(3, 34),
            Msg::UnknownMethod("A".into(), "foo".into(), vec![]));
    }

    #[test]
    fn test_invoke_method_as_static() {
        err("class A {
                fun foo() {}
                static fun test() { A::foo(); }
            }",
            pos(3, 37),
            Msg::UnknownStaticMethod("A".into(), "foo".into(), vec![]));
    }

    #[test]
    fn test_fct_with_type_params() {
        err("fun f() {} fun g() { f::<int>(); }",
            pos(1, 22),
            Msg::WrongNumberTypeParams(0, 1));
        err("fun f<T>() {} fun g() { f(); }",
            pos(1, 25),
            Msg::WrongNumberTypeParams(1, 0));
        ok("fun f<T>() {} fun g() { f::<int>(); }");
        ok("fun f<T1, T2>() {} fun g() { f::<int, Str>(); }");
    }

    #[test]
    fn test_const_check() {
        err("const one: int = 1;
            fun f() -> long { return one; }",
            pos(2, 31),
            Msg::ReturnType("long".into(), "int".into()));

        err("const one: int = 1;
            fun f() { let x: Str = one; }",
            pos(2, 23),
            Msg::AssignType("x".into(), "Str".into(), "int".into()));
    }

    #[test]
    fn test_const() {
        ok_with_test("  const yes: bool = true;
                        const x: byte = 255Y;
                        const a: int = 100;
                        const b: long = 200L;
                        const c: char = 'A';
                        const d: float = 3.0F;
                        const e: double = 6.0;",
                     |ctxt| {
            assert_eq!(ConstValue::Bool(true), ctxt.consts[0].borrow().value);
            assert_eq!(ConstValue::Int(255), ctxt.consts[1].borrow().value);
            assert_eq!(ConstValue::Int(100), ctxt.consts[2].borrow().value);
            assert_eq!(ConstValue::Int(200), ctxt.consts[3].borrow().value);
            assert_eq!(ConstValue::Char('A'), ctxt.consts[4].borrow().value);
            assert_eq!(ConstValue::Float(3.0), ctxt.consts[5].borrow().value);
            assert_eq!(ConstValue::Float(6.0), ctxt.consts[6].borrow().value);
        });
    }

    #[test]
    fn test_assignment_to_const() {
        err("const one: int = 1;
            fun f() { one = 2; }",
            pos(2, 27),
            Msg::AssignmentToConst);
    }

    #[test]
    fn test_unary_minus_byte() {
        err("const m1: byte = -1Y;",
            pos(1, 18),
            Msg::UnOpType("-".into(), "byte".into()));
        ok("const m1: int = -1;");
        ok("const m1: long = -1L;");
    }

    #[test]
    fn test_generic_class_bounds() {
        ok("class Foo
            class A<T: Foo>
            fun f() -> A<Foo> { return nil; }");

        ok("open class Foo
            class Bar: Foo
            class A<T: Foo>
            fun f() -> A<Bar> { return nil; }");

        err("class Foo
            class Bar
            class A<T: Foo>
            fun f() -> A<Bar> { return nil; }",
            pos(1, 1),
            Msg::ClassBoundNotSatisfied("Bar".into(), "Foo".into()));
    }

    #[test]
    fn test_generic_trait_bounds() {
        ok("trait Foo {}
            class X
            impl Foo for X {}
            class A<T: Foo>
            fun f() -> A<X> { return nil; }");

        err("trait Foo {}
            class X
            class A<T: Foo>
            fun f() -> A<X> { return nil; }",
            pos(1, 1),
            Msg::TraitBoundNotSatisfied("X".into(), "Foo".into()));
    }

    #[test]
    fn test_operator_on_generic_type() {
        err("fun f<T>(a: T, b: T) { a + b; }",
            pos(1, 26),
            Msg::BinOpType("+".into(), "T".into(), "T".into()));
    }

    #[test]
    fn test_find_class_method_precedence() {
        // finding class method should have precedence over
        // trait methods
        ok("class A { fun foo() {} }
            trait Foo { fun foo(); }
            impl Foo for A { fun foo() {} }
            fun test(a: A) { a.foo(); }");

        err("class A { fun foo() {} }
            trait Foo { fun foo(a: int); }
            impl Foo for A { fun foo(a:  int) {} }
            fun test(a: A) { a.foo(1); }",
            pos(4, 31),
            Msg::UnknownMethod("A".into(), "foo".into(), vec!["int".into()]));

        ok("class A { static fun foo() {} }
            trait Foo { fun foo(a: int); }
            impl Foo for A { fun foo(a:  int) {} }
            fun test(a: A) { a.foo(1); }");
    }

    #[test]
    fn test_invoke_abstract_class_ctor() {
        err("abstract class A
            fun test() -> A { return A(); }",
            pos(2, 38),
            Msg::NewAbstractClass);
    }

    #[test]
    fn test_global_get() {
        ok("var x: int; fun foo() -> int { return x; }");
    }

    #[test]
    fn test_global_set() {
        ok("var x: int; fun foo(a: int) { x = a; }");
        err("let x: int; fun foo(a: int) { x = a; }",
            pos(1, 33),
            Msg::LetReassigned);
    }

    #[test]
    fn lambda_assignment() {
        ok("fun f() { let x = || {}; }");
        ok("fun f() { let x = || -> int { return 2; }; }");
        ok("fun f() { let x: () -> () = || {}; }");
        ok("fun f() { let x: () -> () = || -> () {}; }");
        ok("fun f() { let x: () -> int = || -> int { return 2; }; }");
        err("fun f() { let x: () -> int = || {}; }",
            pos(1, 11),
            Msg::AssignType("x".into(), "() -> int".into(), "() -> ()".into()));
    }

    /*#[test]
    fn test_generic_trait_method_call() {
        ok("trait Foo { fun one() -> int; }
           class X
           impl Foo for X { fun one() -> int { return 1; } }
           class A<T: Foo>
           fun f(a: A<X>) {
             return a.one();
           }");
    }*/
}
