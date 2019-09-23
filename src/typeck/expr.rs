use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::{f32, f64};

use crate::class::{ClassId, TypeParams};
use crate::sym::Sym::SymClass;
use crate::ty::BuiltinType;
use crate::vm;
use crate::vm::{
    CallType, ConvInfo, Fct, FctId, FctParent, FctSrc, ForTypeInfo, IdentType, TraitId, VM,
};
use dora_parser::error::msg::Msg;

use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntBase, IntSuffix};

pub struct TypeCheck<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub src: &'a mut FctSrc,
    pub ast: &'ast Function,
    pub expr_type: BuiltinType,
    pub negative_expr_id: NodeId,
    pub used_in_call: HashSet<NodeId>,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    pub fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    pub fn check_stmt_var(&mut self, s: &'ast StmtVarType) {
        let var = *self.src.map_vars.get(s.id).unwrap();

        let expr_type = s.expr.as_ref().map(|expr| {
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
                let tyname = self.vm.interner.str(s.name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report_without_path(s.pos, Msg::VarNeedsTypeInfo(tyname));

                return;
            }
        };

        // update type of variable, necessary when variable is only initialized with
        // an expression
        self.src.vars[var].ty = defined_type;

        if let Some(expr_type) = expr_type {
            if !expr_type.is_error() && !defined_type.allows(self.vm, expr_type) {
                let name = self.vm.interner.str(s.name).to_string();
                let defined_type = defined_type.name(self.vm);
                let expr_type = expr_type.name(self.vm);
                let msg = Msg::AssignType(name, defined_type, expr_type);
                self.vm.diag.lock().report_without_path(s.pos, msg);
            }

        // let variable binding needs to be assigned
        } else if !s.reassignable {
            self.vm
                .diag
                .lock()
                .report_without_path(s.pos, Msg::LetMissingInitialization);
        }
    }

    fn check_stmt_for(&mut self, s: &'ast StmtForType) {
        self.visit_expr(&s.expr);
        let object_type = self.expr_type;

        let name = self.vm.interner.intern("makeIterator");

        let mut lookup = MethodLookup::new(self.vm)
            .method(object_type)
            .pos(s.pos)
            .name(name)
            .args(&[]);

        if lookup.find() {
            let make_iterator_id = lookup.found_fct_id().unwrap();
            let make_iterator_ret = lookup.found_ret().unwrap();
            let iterator_trait_id = self.vm.vips.iterator();

            if make_iterator_ret.implements_trait(self.vm, iterator_trait_id) {
                // find fct next() & hasNext() in iterator-trait
                let has_next_name = self.vm.interner.intern("hasNext");
                let next_name = self.vm.interner.intern("next");
                let trai = self.vm.traits[iterator_trait_id].read();
                let next_id = trai
                    .find_method(self.vm, false, next_name, None, &[])
                    .expect("next() not found");
                let has_next_id = trai
                    .find_method(self.vm, false, has_next_name, None, &[])
                    .expect("hasNext() not found");

                // find impl for ret that implements Iterator
                let cls_id = make_iterator_ret.cls_id(self.vm).unwrap();
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let impl_id = cls
                    .find_impl_for_trait(self.vm, iterator_trait_id)
                    .expect("impl not found for Iterator");

                // find method in impl that implements next()
                let ximpl = self.vm.impls[impl_id].read();
                let impl_next_id = ximpl
                    .find_implements(self.vm, next_id)
                    .expect("next() impl not found");

                // find method in impl that implements hasNext();
                let impl_has_next_id = ximpl
                    .find_implements(self.vm, has_next_id)
                    .expect("hasNext() impl not found");

                // get return type of next() in impl
                let fct = self.vm.fcts.idx(impl_next_id);
                let fct = fct.read();
                let ret = fct.return_type;

                // set variable type to return type of next
                let var_id = *self.src.map_vars.get(s.id).unwrap();
                self.src.vars[var_id].ty = ret;

                // store fct ids for `for-in` loop
                self.src.map_fors.insert(
                    s.id,
                    ForTypeInfo {
                        make_iterator: make_iterator_id,
                        has_next: impl_has_next_id,
                        next: impl_next_id,
                        iterator_type: make_iterator_ret,
                    },
                );
            } else {
                let ret = make_iterator_ret.name(self.vm);
                let msg = Msg::MakeIteratorReturnType(ret);
                self.vm.diag.lock().report_without_path(s.expr.pos(), msg);
            }
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_while(&mut self, s: &'ast StmtWhileType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool {
            let expr_type = self.expr_type.name(self.vm);
            let msg = Msg::WhileCondType(expr_type);
            self.vm.diag.lock().report_without_path(s.pos, msg);
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_if(&mut self, s: &'ast StmtIfType) {
        self.visit_expr(&s.cond);

        if self.expr_type != BuiltinType::Bool && !self.expr_type.is_error() {
            let expr_type = self.expr_type.name(self.vm);
            let msg = Msg::IfCondType(expr_type);
            self.vm.diag.lock().report_without_path(s.pos, msg);
        }

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            self.visit_stmt(else_block);
        }
    }

    fn check_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| {
                self.visit_expr(&expr);

                self.expr_type
            })
            .unwrap_or(BuiltinType::Unit);

        let fct_type = self.fct.return_type;

        if !expr_type.is_error() && !fct_type.allows(self.vm, expr_type) {
            let msg = if expr_type.is_nil() {
                let fct_type = fct_type.name(self.vm);

                Msg::IncompatibleWithNil(fct_type)
            } else {
                let fct_type = fct_type.name(self.vm);
                let expr_type = expr_type.name(self.vm);

                Msg::ReturnType(fct_type, expr_type)
            };

            self.vm.diag.lock().report_without_path(s.pos, msg);
        }
    }

    fn check_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        self.visit_expr(&s.expr);
        let ty = self.expr_type;

        if ty.is_nil() {
            self.vm
                .diag
                .lock()
                .report_without_path(s.pos, Msg::ThrowNil);
        } else if !ty.reference_type() {
            let tyname = ty.name(self.vm);
            self.vm
                .diag
                .lock()
                .report_without_path(s.pos, Msg::ReferenceTypeExpected(tyname));
        }
    }

    fn check_stmt_defer(&mut self, s: &'ast StmtDeferType) {
        self.visit_expr(&s.expr);

        if !s.expr.is_call() && !s.expr.is_call2() {
            self.vm
                .diag
                .lock()
                .report_without_path(s.pos, Msg::FctCallExpected);
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
        let ident_type = self.src.map_idents.get(e.id).unwrap();

        match ident_type {
            &IdentType::Var(varid) => {
                let ty = self.src.vars[varid].ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            &IdentType::Global(globalid) => {
                let glob = self.vm.globals.idx(globalid);
                let ty = glob.lock().ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            &IdentType::Field(ty, fieldid) => {
                let clsid = ty.cls_id(self.vm).unwrap();
                let cls = self.vm.classes.idx(clsid);
                let cls = cls.read();
                let field = &cls.fields[fieldid];

                self.src.set_ty(e.id, field.ty);
                self.expr_type = field.ty;
            }

            &IdentType::Struct(sid) => {
                let list_id = self.vm.lists.lock().insert(TypeParams::empty());
                let ty = BuiltinType::Struct(sid, list_id);
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            &IdentType::Const(const_id) => {
                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.lock();

                self.src.set_ty(e.id, xconst.ty);
                self.expr_type = xconst.ty;
            }

            &IdentType::Fct(_) => {
                if !self.used_in_call.contains(&e.id) {
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(e.pos, Msg::FctUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::Class(_) => {
                if !self.used_in_call.contains(&e.id) {
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(e.pos, Msg::ClsUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::FctType(_, _) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn check_expr_assign(&mut self, e: &'ast ExprAssignType) {
        if e.lhs.is_call2() {
            self.check_expr_assign_call(e);
        } else if e.lhs.is_dot() {
            self.check_expr_assign_field(e);
        } else if e.lhs.is_ident() {
            let lhs_type;

            self.visit_expr(&e.rhs);
            let rhs_type = self.expr_type;

            self.src.set_ty(e.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;

            if let Some(ident_type) = self.src.map_idents.get(e.lhs.id()) {
                match ident_type {
                    &IdentType::Var(varid) => {
                        if !self.src.vars[varid].reassignable {
                            self.vm
                                .diag
                                .lock()
                                .report_without_path(e.pos, Msg::LetReassigned);
                        }

                        lhs_type = self.src.vars[varid].ty;
                    }

                    &IdentType::Global(gid) => {
                        let glob = self.vm.globals.idx(gid);
                        let glob = glob.lock();

                        if !glob.reassignable {
                            self.vm
                                .diag
                                .lock()
                                .report_without_path(e.pos, Msg::LetReassigned);
                        }

                        lhs_type = glob.ty;
                    }

                    &IdentType::Field(_, _) => {
                        unreachable!();
                    }

                    &IdentType::Struct(_) => {
                        unimplemented!();
                    }

                    &IdentType::Const(_) => {
                        self.vm
                            .diag
                            .lock()
                            .report_without_path(e.pos, Msg::AssignmentToConst);

                        return;
                    }

                    &IdentType::Fct(_) | &IdentType::FctType(_, _) => {
                        self.vm
                            .diag
                            .lock()
                            .report_without_path(e.pos, Msg::FctReassigned);

                        return;
                    }

                    &IdentType::Class(_) | &IdentType::ClassType(_, _) => {
                        self.vm
                            .diag
                            .lock()
                            .report_without_path(e.pos, Msg::ClassReassigned);

                        return;
                    }

                    &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
                    &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                        unreachable!()
                    }
                }

                if !lhs_type.allows(self.vm, rhs_type) {
                    let ident = e.lhs.to_ident().unwrap();
                    let name = self.vm.interner.str(ident.name).to_string();
                    let lhs_type = lhs_type.name(self.vm);
                    let rhs_type = rhs_type.name(self.vm);

                    self.src.set_ty(e.id, BuiltinType::Unit);
                    self.expr_type = BuiltinType::Unit;

                    let msg = Msg::AssignType(name, lhs_type, rhs_type);
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }

                return;
            }
        } else {
            self.vm
                .diag
                .lock()
                .report_without_path(e.pos, Msg::LvalueExpected);
        }

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
    }

    fn check_expr_assign_call(&mut self, e: &'ast ExprAssignType) {
        let call = e.lhs.to_call2().unwrap();

        self.visit_expr(&call.callee);
        let expr_type = self.expr_type;

        let mut arg_types: Vec<BuiltinType> = call
            .args
            .iter()
            .map(|arg| {
                self.visit_expr(arg);
                self.expr_type
            })
            .collect();

        self.visit_expr(&e.rhs);
        let value_type = self.expr_type;

        let name = self.vm.interner.intern("set");
        arg_types.push(value_type);
        let ret_type = Some(BuiltinType::Unit);

        if let Some((_, fct_id, return_type)) = self.find_method(
            e.pos,
            expr_type,
            false,
            name,
            &arg_types,
            &TypeParams::empty(),
            ret_type,
        ) {
            let call_type = CallType::Expr(expr_type, fct_id);
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            // let fct = self.vm.fcts.idx(fct_id);
            // let fct = fct.read();
            // let element_type = *fct.params_without_self().last().unwrap();
            // let element_type_params = expr_type.type_params(self.vm);
            // let element_type = specialize_type(
            //     self.vm,
            //     element_type,
            //     &element_type_params,
            //     &TypeParams::empty(),
            // );
            // self.src.set_ty(call.id, element_type);

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_assign_field(&mut self, e: &'ast ExprAssignType) {
        let field_expr = e.lhs.to_dot().unwrap();
        let name = field_expr.name;

        self.visit_expr(&field_expr.object);
        let object_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        let cls_id = object_type.cls_id(self.vm);

        if let Some(cls_id) = cls_id {
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            if let Some((cls_id, field_id)) = cls.find_field(self.vm, name) {
                let ty = match object_type {
                    BuiltinType::Class(_, list_id) => BuiltinType::Class(cls_id, list_id),

                    _ => unreachable!(),
                };

                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let ident_type = IdentType::Field(ty, field_id);
                self.src
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let field = &cls.fields[field_id];
                let class_type_params = ty.type_params(self.vm);
                let fty =
                    replace_type_param(self.vm, field.ty, &class_type_params, &TypeParams::empty());

                if !self.fct.is_constructor && !field.reassignable {
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(e.pos, Msg::LetReassigned);
                }

                if !fty.allows(self.vm, rhs_type) && !rhs_type.is_error() {
                    let field = e.lhs.to_dot().unwrap();
                    let name = self.vm.interner.str(field.name).to_string();

                    let object_type = object_type.name(self.vm);
                    let lhs_type = fty.name(self.vm);
                    let rhs_type = rhs_type.name(self.vm);

                    let msg = Msg::AssignField(name, object_type, lhs_type, rhs_type);
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }

                self.src.set_ty(e.id, BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
                return;
            }
        }

        // field not found, report error
        let field_name = self.vm.interner.str(name).to_string();
        let expr_name = object_type.name(self.vm);
        let msg = Msg::UnknownField(field_name, expr_name);
        self.vm.diag.lock().report_without_path(field_expr.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn find_method(
        &mut self,
        pos: Position,
        object_type: BuiltinType,
        is_static: bool,
        name: Name,
        args: &[BuiltinType],
        fct_type_params: &TypeParams,
        return_type: Option<BuiltinType>,
    ) -> Option<(ClassId, FctId, BuiltinType)> {
        let result = lookup_method(
            self.vm,
            object_type,
            is_static,
            name,
            args,
            fct_type_params,
            return_type,
        );

        if result.is_none() {
            let type_name = object_type.name(self.vm);
            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<String>>();
            let msg = if is_static {
                Msg::UnknownStaticMethod(type_name, name, param_names)
            } else {
                Msg::UnknownMethod(type_name, name, param_names)
            };

            self.vm.diag.lock().report_without_path(pos, msg);
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

    fn check_expr_un_method(&mut self, e: &'ast ExprUnType, op: UnOp, name: &str, ty: BuiltinType) {
        let name = self.vm.interner.intern(name);
        let call_types = [];

        if !ty.is_error() {
            if let Some((_, fct_id, return_type)) = lookup_method(
                self.vm,
                ty,
                false,
                name,
                &call_types,
                &TypeParams::empty(),
                None,
            ) {
                let call_type = CallType::Method(ty, fct_id, TypeParams::empty());
                self.src.map_calls.insert(e.id, Arc::new(call_type));

                self.src.set_ty(e.id, return_type);
                self.expr_type = return_type;
                return;
            }

            let ty = ty.name(self.vm);
            let msg = Msg::UnOpType(op.as_str().into(), ty);

            self.vm.diag.lock().report_without_path(e.pos, msg);
        }

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
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
            BinOp::ArithShiftR => {
                self.check_expr_bin_method(e, e.op, "shiftRight", lhs_type, rhs_type)
            }
            BinOp::LogicalShiftR => {
                self.check_expr_bin_method(e, e.op, "unsignedShiftRight", lhs_type, rhs_type)
            }
        }
    }

    fn check_expr_bin_bool(
        &mut self,
        e: &'ast ExprBinType,
        op: BinOp,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Bool);
        self.src.set_ty(e.id, BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_expr_bin_method(
        &mut self,
        e: &'ast ExprBinType,
        op: BinOp,
        name: &str,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) {
        let name = self.vm.interner.intern(name);
        let call_types = [rhs_type];

        if let Some((_, fct_id, return_type)) = lookup_method(
            self.vm,
            lhs_type,
            false,
            name,
            &call_types,
            &TypeParams::empty(),
            None,
        ) {
            let call_type = CallType::Method(lhs_type, fct_id, TypeParams::empty());
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = Msg::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.vm.diag.lock().report_without_path(e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_bin_cmp(
        &mut self,
        e: &'ast ExprBinType,
        cmp: CmpOp,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) {
        match cmp {
            CmpOp::Is | CmpOp::IsNot => {
                if !(lhs_type.is_nil() || lhs_type.allows(self.vm, rhs_type))
                    && !(rhs_type.is_nil() || rhs_type.allows(self.vm, lhs_type))
                {
                    let lhs_type = lhs_type.name(self.vm);
                    let rhs_type = rhs_type.name(self.vm);
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(e.pos, Msg::TypesIncompatible(lhs_type, rhs_type));
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

    fn check_type(
        &mut self,
        e: &'ast ExprBinType,
        op: BinOp,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
        expected_type: BuiltinType,
    ) {
        if !expected_type.allows(self.vm, lhs_type) || !expected_type.allows(self.vm, rhs_type) {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = Msg::BinOpType(op, lhs_type, rhs_type);

            self.vm.diag.lock().report_without_path(e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType, in_try: bool) {
        let object_type = if e.object.is_some() {
            let object = e.object.as_ref().unwrap();

            let object_type = if object.is_super() {
                self.super_type(e.pos)
            } else {
                self.visit_expr(object);
                self.expr_type
            };

            Some(object_type)
        } else {
            None
        };

        let call_types: Vec<BuiltinType> = e
            .args
            .iter()
            .map(|arg| {
                self.visit_expr(arg);
                self.expr_type
            })
            .collect();

        let type_params: Vec<BuiltinType> = if let Some(ref type_params) = e.type_params {
            type_params.iter().map(|p| self.src.ty(p.id())).collect()
        } else {
            Vec::new()
        };

        let type_params: TypeParams = TypeParams::with(type_params);

        if let Some(object_type) = object_type {
            if object_type.is_type_param() {
                self.check_generic_method_call(e, in_try, object_type, &call_types);
                return;
            }

            if object_type.is_error() {
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
                return;
            }

            let mut lookup = MethodLookup::new(self.vm)
                .method(object_type)
                .pos(e.pos)
                .name(e.path.name())
                .args(&call_types);

            if lookup.find() {
                let fct_id = lookup.found_fct_id().unwrap();
                let return_type = lookup.found_ret().unwrap();

                let call_type = CallType::Method(object_type, fct_id, TypeParams::empty());
                self.src
                    .map_calls
                    .insert_or_replace(e.id, Arc::new(call_type));
                self.src.set_ty(e.id, return_type);
                self.expr_type = return_type;

                if !in_try {
                    let fct = self.vm.fcts.idx(fct_id);
                    let fct = fct.read();
                    let throws = fct.throws;

                    if throws {
                        let msg = Msg::ThrowingCallWithoutTry;
                        self.vm.diag.lock().report_without_path(e.pos, msg);
                    }
                }
            } else {
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            return;
        }

        let call_type = if e.path.len() > 1 {
            match self.vm.sym.lock().get(e.path[0]) {
                Some(SymClass(cls_id)) => {
                    assert_eq!(2, e.path.len());

                    let mut lookup = MethodLookup::new(self.vm)
                        .pos(e.pos)
                        .static_method(cls_id)
                        .name(e.path[1])
                        .args(&call_types)
                        .fct_type_params(&type_params);

                    if lookup.find() {
                        let fct_id = lookup.found_fct_id().unwrap();
                        let call_type = Arc::new(CallType::Fct(
                            fct_id,
                            TypeParams::empty(),
                            type_params.clone(),
                        ));
                        self.src.map_calls.insert(e.id, call_type.clone());

                        call_type
                    } else {
                        self.expr_type = BuiltinType::Error;
                        return;
                    }
                }

                _ => {
                    let name = self.vm.interner.str(e.path[0]).to_string();
                    let msg = Msg::ClassExpected(name);
                    self.vm.diag.lock().report_without_path(e.pos, msg);

                    self.expr_type = BuiltinType::Error;
                    return;
                }
            }
        } else {
            self.src.map_calls.get(e.id).unwrap().clone()
        };

        match *call_type {
            CallType::CtorNew(cls_id, _, _) => {
                let mut lookup = MethodLookup::new(self.vm)
                    .pos(e.pos)
                    .ctor(cls_id)
                    .args(&call_types)
                    .cls_type_params(&type_params);

                let ty = if lookup.find() {
                    let fct_id = lookup.found_fct_id().unwrap();
                    let cls_id = lookup.found_cls_id().unwrap();
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();

                    let call_type = CallType::CtorNew(cls_id, fct_id, type_params.clone());
                    self.src.map_calls.replace(e.id, Arc::new(call_type));

                    if cls.is_abstract {
                        let msg = Msg::NewAbstractClass;
                        self.vm.diag.lock().report_without_path(e.pos, msg);
                    }

                    lookup.found_ret().unwrap()
                } else {
                    BuiltinType::Error
                };

                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            CallType::Fct(callee_id, _, _) => {
                let mut lookup = MethodLookup::new(self.vm)
                    .pos(e.pos)
                    .callee(callee_id)
                    .args(&call_types)
                    .fct_type_params(&type_params);

                let ty = if lookup.find() {
                    let call_type =
                        CallType::Fct(callee_id, TypeParams::empty(), type_params.clone());
                    self.src.map_calls.replace(e.id, Arc::new(call_type));

                    lookup.found_ret().unwrap()
                } else {
                    BuiltinType::Error
                };

                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            _ => panic!("invocation of method"),
        }

        if !in_try {
            let fct_id = call_type.fct_id();
            let fct = self.vm.fcts.idx(fct_id);
            let fct = fct.read();
            let throws = fct.throws;

            if throws {
                let msg = Msg::ThrowingCallWithoutTry;
                self.vm.diag.lock().report_without_path(e.pos, msg);
            }
        }
    }

    fn check_expr_call2(&mut self, e: &'ast ExprCall2Type, in_try: bool) {
        self.used_in_call.insert(e.callee.id());

        self.visit_expr(&e.callee);
        let expr_type = self.expr_type;
        let ident_type = self.src.map_idents.get(e.callee.id()).cloned();

        let arg_types: Vec<BuiltinType> = e
            .args
            .iter()
            .map(|arg| {
                self.visit_expr(arg);
                self.expr_type
            })
            .collect();

        match ident_type {
            Some(IdentType::Fct(fct_id)) => {
                self.check_expr_call_ident(e, fct_id, TypeParams::empty(), &arg_types, in_try);
            }

            Some(IdentType::FctType(fct_id, type_params)) => {
                self.check_expr_call_ident(e, fct_id, type_params, &arg_types, in_try);
            }

            Some(IdentType::Class(cls_id)) => {
                self.check_expr_call_ctor(e, cls_id, TypeParams::empty(), &arg_types, in_try);
            }

            Some(IdentType::ClassType(cls_id, type_params)) => {
                self.check_expr_call_ctor(e, cls_id, type_params, &arg_types, in_try);
            }

            Some(IdentType::Method(object_type, method_name)) => {
                self.check_expr_call_method(
                    e,
                    object_type,
                    method_name,
                    TypeParams::empty(),
                    &arg_types,
                    in_try,
                );
            }

            Some(IdentType::MethodType(object_type, method_name, type_params)) => {
                self.check_expr_call_method(
                    e,
                    object_type,
                    method_name,
                    type_params,
                    &arg_types,
                    in_try,
                );
            }

            Some(IdentType::StaticMethod(object_type, method_name)) => self
                .check_expr_call_static_method(
                    e,
                    object_type,
                    method_name,
                    TypeParams::empty(),
                    &arg_types,
                    in_try,
                ),

            Some(IdentType::StaticMethodType(object_type, method_name, type_params)) => self
                .check_expr_call_static_method(
                    e,
                    object_type,
                    method_name,
                    type_params,
                    &arg_types,
                    in_try,
                ),

            _ => {
                if expr_type.is_error() {
                    self.src.set_ty(e.id, expr_type);
                    self.expr_type = expr_type;
                    return;
                }

                self.check_expr_call_expr(e, expr_type, &arg_types, in_try);
            }
        }
    }

    fn check_expr_call_expr(
        &mut self,
        e: &'ast ExprCall2Type,
        expr_type: BuiltinType,
        arg_types: &[BuiltinType],
        _in_try: bool,
    ) {
        let get = self.vm.interner.intern("get");

        if let Some((_, fct_id, return_type)) = self.find_method(
            e.pos,
            expr_type,
            false,
            get,
            arg_types,
            &TypeParams::empty(),
            None,
        ) {
            let call_type = CallType::Expr(expr_type, fct_id);
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_ident(
        &mut self,
        e: &'ast ExprCall2Type,
        fct_id: FctId,
        type_params: TypeParams,
        arg_types: &[BuiltinType],
        in_try: bool,
    ) {
        let mut lookup = MethodLookup::new(self.vm)
            .pos(e.pos)
            .callee(fct_id)
            .args(&arg_types)
            .fct_type_params(&type_params);

        let ty = if lookup.find() {
            let call_type = CallType::Fct(fct_id, TypeParams::empty(), type_params.clone());
            self.src.map_calls.insert(e.id, Arc::new(call_type));

            if !in_try {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();
                let throws = fct.throws;

                if throws {
                    let msg = Msg::ThrowingCallWithoutTry;
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }
            }

            lookup.found_ret().unwrap()
        } else {
            BuiltinType::Error
        };

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_call_static_method(
        &mut self,
        e: &'ast ExprCall2Type,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeParams,
        arg_types: &[BuiltinType],
        in_try: bool,
    ) {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls_type_params = object_type.type_params(self.vm);
        assert_eq!(cls_type_params.len(), 0);

        let mut lookup = MethodLookup::new(self.vm)
            .pos(e.pos)
            .static_method(cls_id)
            .name(method_name)
            .args(arg_types)
            .fct_type_params(&type_params);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();
            let call_type = Arc::new(CallType::Fct(
                fct_id,
                TypeParams::empty(),
                type_params.clone(),
            ));
            self.src.map_calls.insert(e.id, call_type.clone());

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;

            if !in_try {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();
                let throws = fct.throws;

                if throws {
                    let msg = Msg::ThrowingCallWithoutTry;
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }
            }
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_method(
        &mut self,
        e: &'ast ExprCall2Type,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeParams,
        arg_types: &[BuiltinType],
        in_try: bool,
    ) {
        if object_type.is_type_param() {
            assert_eq!(type_params.len(), 0);
            self.check_expr_call_generic(e, object_type, method_name, arg_types, in_try);
            return;
        }

        let mut lookup = MethodLookup::new(self.vm)
            .method(object_type)
            .pos(e.pos)
            .name(method_name)
            .fct_type_params(&type_params)
            .args(arg_types);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();

            let call_type = CallType::Method(object_type, fct_id, type_params.clone());
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;

            if !in_try {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();
                let throws = fct.throws;

                if throws {
                    let msg = Msg::ThrowingCallWithoutTry;
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }
            }
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_ctor(
        &mut self,
        e: &'ast ExprCall2Type,
        cls_id: ClassId,
        type_params: TypeParams,
        arg_types: &[BuiltinType],
        _in_try: bool,
    ) {
        let call_type = CallType::CtorNew(cls_id, FctId(0), TypeParams::empty());
        self.src.map_calls.insert(e.id, Arc::new(call_type));

        let mut lookup = MethodLookup::new(self.vm)
            .pos(e.pos)
            .ctor(cls_id)
            .args(arg_types)
            .cls_type_params(&type_params);

        let ty = if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let cls_id = lookup.found_cls_id().unwrap();
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            let call_type = CallType::CtorNew(cls_id, fct_id, type_params.clone());
            self.src.map_calls.replace(e.id, Arc::new(call_type));

            if cls.is_abstract {
                let msg = Msg::NewAbstractClass;
                self.vm.diag.lock().report_without_path(e.pos, msg);
            }

            lookup.found_ret().unwrap()
        } else {
            BuiltinType::Error
        };

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_call_generic(
        &mut self,
        e: &'ast ExprCall2Type,
        object_type: BuiltinType,
        name: Name,
        arg_types: &[BuiltinType],
        in_try: bool,
    ) {
        match object_type {
            BuiltinType::FctTypeParam(_, tpid) => {
                let tp = &self.fct.type_params[tpid.idx()];
                self.check_expr_call_generic_type_param(
                    e,
                    object_type,
                    tp,
                    name,
                    arg_types,
                    in_try,
                );
            }

            BuiltinType::ClassTypeParam(cls_id, tpid) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let tp = &cls.type_params[tpid.idx()];
                self.check_expr_call_generic_type_param(
                    e,
                    object_type,
                    tp,
                    name,
                    arg_types,
                    in_try,
                );
            }

            _ => unreachable!(),
        }
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &'ast ExprCall2Type,
        object_type: BuiltinType,
        tp: &vm::TypeParam,
        name: Name,
        args: &[BuiltinType],
        in_try: bool,
    ) {
        let mut found_fcts = Vec::new();

        for &trait_id in &tp.trait_bounds {
            let trai = self.vm.traits[trait_id].read();

            if let Some(fid) = trai.find_method(self.vm, false, name, None, args) {
                found_fcts.push(fid);
            }
        }

        if found_fcts.len() == 1 {
            let fid = found_fcts[0];
            let call_type = CallType::Method(object_type, fid, TypeParams::empty());
            self.src.map_calls.insert(e.id, Arc::new(call_type));

            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();
            let return_type = fct.return_type;

            if fct.throws && !in_try {
                let msg = Msg::ThrowingCallWithoutTry;
                self.vm.diag.lock().report_without_path(e.pos, msg);
            }

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            let type_name = object_type.name(self.vm);
            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<String>>();
            let msg = if found_fcts.len() == 0 {
                Msg::UnknownMethodForTypeParam(type_name, name, param_names)
            } else {
                Msg::MultipleCandidatesForTypeParam(type_name, name, param_names)
            };

            self.vm.diag.lock().report_without_path(e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_path(
        &mut self,
        e: &'ast ExprCall2Type,
        type_params: TypeParams,
        arg_types: &[BuiltinType],
    ) {
        let path = e.callee.to_path().unwrap();
        let class_expr = &path.lhs;
        let method_name_expr = &path.rhs;

        let class;
        let method_name;

        if let Some(class_expr) = class_expr.to_ident() {
            class = class_expr.name;
        } else {
            let msg = Msg::ExpectedSomeIdentifier;
            self.vm
                .diag
                .lock()
                .report_without_path(class_expr.pos(), msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        if let Some(method_name_expr) = method_name_expr.to_ident() {
            method_name = method_name_expr.name;
        } else {
            let msg = Msg::ExpectedSomeIdentifier;
            self.vm
                .diag
                .lock()
                .report_without_path(method_name_expr.pos(), msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        match self.vm.sym.lock().get(class) {
            Some(SymClass(cls_id)) => {
                let mut lookup = MethodLookup::new(self.vm)
                    .pos(e.pos)
                    .static_method(cls_id)
                    .name(method_name)
                    .args(arg_types)
                    .fct_type_params(&type_params);

                if lookup.find() {
                    let fct_id = lookup.found_fct_id().unwrap();
                    let call_type = Arc::new(CallType::Fct(
                        fct_id,
                        TypeParams::empty(),
                        type_params.clone(),
                    ));
                    self.src.map_calls.insert(e.id, call_type.clone());
                    let ty = lookup.found_ret().unwrap();
                    self.src.set_ty(e.id, ty);
                    self.expr_type = ty;
                } else {
                    self.src.set_ty(e.id, BuiltinType::Error);
                    self.expr_type = BuiltinType::Error;
                }

                return;
            }

            _ => {}
        }

        let name = self.vm.interner.str(class).to_string();
        let msg = Msg::ClassExpected(name);
        self.vm.diag.lock().report_without_path(e.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
    }

    fn check_expr_delegation(&mut self, e: &'ast ExprDelegationType) {
        let arg_types: Vec<BuiltinType> = e
            .args
            .iter()
            .map(|arg| {
                self.visit_expr(arg);
                self.expr_type
            })
            .collect();

        let owner = self.vm.classes.idx(self.fct.cls_id());
        let owner = owner.read();

        let cls_id = if e.ty.is_super() {
            owner.parent_class.unwrap()
        } else {
            owner.id
        };

        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        if let Some(ctor_id) = cls.constructor {
            let ctor = self.vm.fcts.idx(ctor_id);
            let ctor = ctor.read();

            if args_compatible(
                self.vm,
                &ctor.params_without_self(),
                &arg_types,
                Some(cls_id),
                None,
                &TypeParams::empty(),
                &TypeParams::empty(),
            ) {
                self.src.map_tys.insert(e.id, self.vm.cls(cls.id));

                let call_type = CallType::Ctor(cls.id, ctor.id, TypeParams::empty());
                self.src.map_calls.insert(e.id, Arc::new(call_type));
                return;
            }
        }

        let name = self.vm.interner.str(cls.name).to_string();
        let arg_types = arg_types.iter().map(|t| t.name(self.vm)).collect();
        let msg = Msg::UnknownCtor(name, arg_types);
        self.vm.diag.lock().report_without_path(e.pos, msg);
    }

    fn super_type(&self, pos: Position) -> BuiltinType {
        if let FctParent::Class(clsid) = self.fct.parent {
            let cls = self.vm.classes.idx(clsid);
            let cls = cls.read();

            if let Some(superid) = cls.parent_class {
                let cls = self.vm.classes.idx(superid);
                let cls = cls.read();
                return cls.ty;
            }
        }

        let msg = Msg::SuperUnavailable;
        self.vm.diag.lock().report_without_path(pos, msg);

        BuiltinType::Error
    }

    fn check_generic_method_call(
        &mut self,
        e: &'ast ExprCallType,
        in_try: bool,
        obj: BuiltinType,
        args: &[BuiltinType],
    ) {
        match obj {
            BuiltinType::FctTypeParam(_, tpid) => {
                let tp = &self.fct.type_params[tpid.idx()];
                self.check_generic_method_call_for_type_param(e, in_try, obj, args, tp);
            }

            BuiltinType::ClassTypeParam(cls_id, tpid) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let tp = &cls.type_params[tpid.idx()];
                self.check_generic_method_call_for_type_param(e, in_try, obj, args, tp);
            }

            _ => unreachable!(),
        }
    }

    fn check_generic_method_call_for_type_param(
        &mut self,
        e: &'ast ExprCallType,
        in_try: bool,
        obj: BuiltinType,
        args: &[BuiltinType],
        tp: &vm::TypeParam,
    ) {
        for &trait_id in &tp.trait_bounds {
            let trai = self.vm.traits[trait_id].read();

            if let Some(fid) = trai.find_method(self.vm, false, e.path.name(), None, args) {
                let call_type = CallType::Method(obj, fid, TypeParams::empty());
                self.src.map_calls.insert(e.id, Arc::new(call_type));

                let fct = self.vm.fcts.idx(fid);
                let fct = fct.read();
                let return_type = fct.return_type;

                if fct.throws && !in_try {
                    let msg = Msg::ThrowingCallWithoutTry;
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }

                self.src.set_ty(e.id, return_type);
                self.expr_type = return_type;
                return;
            }
        }

        let type_name = obj.name(self.vm);
        let name = self.vm.interner.str(e.path.name()).to_string();
        let param_names = args
            .iter()
            .map(|a| a.name(self.vm))
            .collect::<Vec<String>>();
        let msg = Msg::UnknownMethod(type_name, name, param_names);

        self.vm.diag.lock().report_without_path(e.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_path(&mut self, e: &'ast ExprPathType) {
        let ident_type = self.src.map_idents.get(e.lhs.id());

        let cls_ty = match ident_type {
            Some(&IdentType::Class(cls_id)) => {
                let list = self.vm.lists.lock().insert(TypeParams::empty());
                BuiltinType::Class(cls_id, list)
            }

            Some(&IdentType::ClassType(cls_id, ref type_params)) => {
                let list = self.vm.lists.lock().insert(type_params.clone());
                BuiltinType::Class(cls_id, list)
            }

            _ => {
                let msg = Msg::InvalidLeftSideOfSeparator;
                self.vm.diag.lock().report_without_path(e.lhs.pos(), msg);
                return;
            }
        };

        let name = if let Some(ident) = e.rhs.to_ident() {
            ident.name
        } else {
            let msg = Msg::NameOfStaticMethodExpected;
            self.vm.diag.lock().report_without_path(e.rhs.pos(), msg);
            return;
        };

        if self.used_in_call.contains(&e.id) {
            self.src
                .map_idents
                .insert(e.id, IdentType::StaticMethod(cls_ty, name));
            return;
        }

        self.vm
            .diag
            .lock()
            .report_without_path(e.pos, Msg::FctUsedAsIdentifier);
    }

    fn check_expr_type_param(&mut self, e: &'ast ExprTypeParamType) {
        if self.used_in_call.contains(&e.id) {
            self.used_in_call.insert(e.callee.id());
        }

        self.visit_expr(&e.callee);
        let ident_type = self.src.map_idents.get(e.callee.id()).cloned();

        let type_params: Vec<BuiltinType> = e.args.iter().map(|p| self.src.ty(p.id())).collect();
        let type_params: TypeParams = TypeParams::with(type_params);

        match ident_type {
            Some(IdentType::Class(cls_id)) => {
                self.src
                    .map_idents
                    .insert(e.id, IdentType::ClassType(cls_id, type_params));
            }

            Some(IdentType::Fct(fct_id)) => {
                self.src
                    .map_idents
                    .insert(e.id, IdentType::FctType(fct_id, type_params));
            }

            Some(IdentType::Method(ty, name)) => {
                self.src
                    .map_idents
                    .insert(e.id, IdentType::MethodType(ty, name, type_params));
            }

            Some(IdentType::StaticMethod(cls_ty, name)) => {
                self.src
                    .map_idents
                    .insert(e.id, IdentType::StaticMethodType(cls_ty, name, type_params));
            }

            _ => {
                let msg = Msg::InvalidUseOfTypeParams;
                self.vm.diag.lock().report_without_path(e.pos, msg);
                return;
            }
        }
    }

    fn check_expr_dot(&mut self, e: &'ast ExprDotType) {
        let object_type = if e.object.is_super() {
            self.super_type(e.object.pos())
        } else {
            self.visit_expr(&e.object);
            self.expr_type
        };

        if self.used_in_call.contains(&e.id) {
            self.src
                .map_idents
                .insert(e.id, IdentType::Method(object_type, e.name));
            return;
        }

        let cls_id = object_type.cls_id(self.vm);

        if let Some(cls_id) = cls_id {
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            if let Some((cls_id, field_id)) = cls.find_field(self.vm, e.name) {
                let ty = match object_type {
                    BuiltinType::Class(_, list_id) => BuiltinType::Class(cls_id, list_id),

                    _ => unreachable!(),
                };

                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let ident_type = IdentType::Field(ty, field_id);
                self.src.map_idents.insert_or_replace(e.id, ident_type);

                let field = &cls.fields[field_id];
                let class_type_params = ty.type_params(self.vm);
                let fty =
                    replace_type_param(self.vm, field.ty, &class_type_params, &TypeParams::empty());

                self.src.set_ty(e.id, fty);
                self.expr_type = fty;
                return;
            }
        }

        // field not found, report error
        let field_name = self.vm.interner.str(e.name).to_string();
        let expr_name = object_type.name(self.vm);
        let msg = Msg::UnknownField(field_name, expr_name);
        self.vm.diag.lock().report_without_path(e.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
    }

    fn check_expr_this(&mut self, e: &'ast ExprSelfType) {
        match self.fct.parent {
            FctParent::Class(clsid) => {
                let cls = self.vm.classes.idx(clsid);
                let cls = cls.read();
                let ty = cls.ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                let cls = self.vm.classes.idx(ximpl.cls_id());
                let cls = cls.read();
                let ty = cls.ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            _ => {
                let msg = Msg::ThisUnavailable;
                self.vm.diag.lock().report_without_path(e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
            }
        }
    }

    fn check_expr_super(&mut self, e: &'ast ExprSuperType) {
        let msg = Msg::SuperNeedsMethodCall;
        self.vm.diag.lock().report_without_path(e.pos, msg);
        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_nil(&mut self, e: &'ast ExprNilType) {
        self.src.set_ty(e.id, BuiltinType::Nil);
        self.expr_type = BuiltinType::Nil;
    }

    fn check_expr_try(&mut self, e: &'ast ExprTryType) {
        let expr_type;

        match *e.expr {
            ExprCall2(ref call) => {
                self.check_expr_call2(call, true);
                expr_type = self.expr_type;
            }

            ExprCall(ref call) => {
                self.check_expr_call(call, true);
                expr_type = self.expr_type;
            }

            _ => {
                self.vm
                    .diag
                    .lock()
                    .report_without_path(e.pos, Msg::TryNeedsCall);

                self.expr_type = BuiltinType::Unit;
                self.src.set_ty(e.id, BuiltinType::Unit);
                return;
            }
        }

        self.src.set_ty(e.id, expr_type);

        if let Some(call_type) = self.src.map_calls.get(e.expr.id()) {
            let fct_id = call_type.fct_id();
            let fct = self.vm.fcts.idx(fct_id);
            let fct = fct.read();
            let throws = fct.throws;

            if !throws {
                self.vm
                    .diag
                    .lock()
                    .report_without_path(e.pos, Msg::TryCallNonThrowing);
            }
        }

        match e.mode {
            TryMode::Normal => {}
            TryMode::Else(ref alt_expr) => {
                self.visit_expr(alt_expr);
                let alt_type = self.expr_type;

                if !expr_type.allows(self.vm, alt_type) {
                    let expr_type = expr_type.name(self.vm);
                    let alt_type = alt_type.name(self.vm);
                    let msg = Msg::TypesIncompatible(expr_type, alt_type);
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }
            }

            TryMode::Force => {}
            TryMode::Opt => panic!("unsupported"),
        }

        self.expr_type = expr_type;
    }

    fn check_expr_lambda(&mut self, e: &'ast ExprLambdaType) {
        let ret = if let Some(ref ty) = e.ret {
            self.src.ty(ty.id())
        } else {
            BuiltinType::Unit
        };

        let params = e
            .params
            .iter()
            .map(|p| self.src.ty(p.data_type.id()))
            .collect::<Vec<_>>();

        let ty = self.vm.lambda_types.lock().insert(params, ret);
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
            let name = check_type.name(self.vm);
            self.vm
                .diag
                .lock()
                .report_without_path(e.pos, Msg::ReferenceTypeExpected(name));
            return;
        }

        let mut valid = false;

        if object_type.subclass_from(self.vm, check_type) {
            // open class A { } class B: A { }
            // (b is A) is valid

            valid = true;
        } else if check_type.subclass_from(self.vm, object_type) {
            // normal check
        } else {
            let object_type = object_type.name(self.vm);
            let check_type = check_type.name(self.vm);
            let msg = Msg::TypesIncompatible(object_type, check_type);
            self.vm.diag.lock().report_without_path(e.pos, msg);
        }

        self.src.map_convs.insert(
            e.id,
            ConvInfo {
                cls_id: check_type.cls_id(self.vm).unwrap(),
                valid: valid,
            },
        );

        self.expr_type = if e.is { BuiltinType::Bool } else { check_type };
    }

    fn check_expr_lit_struct(&mut self, e: &'ast ExprLitStructType) {
        let sid = self.src.map_idents.get(e.id).unwrap().struct_id();
        let struc = self.vm.structs.idx(sid);
        let struc = struc.lock();

        let mut initialized: HashMap<Name, BuiltinType> = Default::default();

        for arg in &e.args {
            self.visit_expr(&arg.expr);
            initialized.insert(arg.name, self.expr_type);
        }

        let struc_name = self.vm.interner.str(struc.name).to_string();

        for field in &struc.fields {
            if let Some(&vty) = initialized.get(&field.name) {
                initialized.remove(&field.name);

                if !field.ty.allows(self.vm, vty) {
                    let fname = self.vm.interner.str(field.name).to_string();
                    let fty = field.ty.name(self.vm);
                    let vty = vty.name(self.vm);
                    let msg = Msg::AssignField(fname, struc_name.clone(), fty, vty);
                    self.vm.diag.lock().report_without_path(e.pos, msg);
                }
            } else {
                let fname = self.vm.interner.str(field.name).to_string();
                self.vm.diag.lock().report_without_path(
                    e.pos,
                    Msg::StructFieldNotInitialized(struc_name.clone(), fname),
                );
            }
        }

        for &fname in initialized.keys() {
            let fname = self.vm.interner.str(fname).to_string();
            self.vm
                .diag
                .lock()
                .report_without_path(e.pos, Msg::UnknownStructField(struc_name.clone(), fname));
        }

        let list_id = self.vm.lists.lock().insert(TypeParams::empty());
        let ty = BuiltinType::Struct(sid, list_id);
        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_int(&mut self, e: &'ast ExprLitIntType) {
        let (ty, _) = check_lit_int(self.vm, e, self.negative_expr_id);

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_float(&mut self, e: &'ast ExprLitFloatType) {
        let (ty, _) = check_lit_float(self.vm, e, self.negative_expr_id);

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
                let str_ty = self.vm.cls(self.vm.vips.string_class);
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
            ExprCall2(ref expr) => self.check_expr_call2(expr, false),
            ExprTypeParam(ref expr) => self.check_expr_type_param(expr),
            ExprPath(ref expr) => self.check_expr_path(expr),
            ExprDelegation(ref expr) => self.check_expr_delegation(expr),
            ExprDot(ref expr) => self.check_expr_dot(expr),
            ExprSelf(ref expr) => self.check_expr_this(expr),
            ExprSuper(ref expr) => self.check_expr_super(expr),
            ExprNil(ref expr) => self.check_expr_nil(expr),
            ExprConv(ref expr) => self.check_expr_conv(expr),
            ExprTry(ref expr) => self.check_expr_try(expr),
            ExprLambda(ref expr) => self.check_expr_lambda(expr),
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtFor(ref stmt) => self.check_stmt_for(stmt),
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

fn args_compatible(
    vm: &VM,
    def: &[BuiltinType],
    expr: &[BuiltinType],
    cls_id: Option<ClassId>,
    fct_id: Option<FctId>,
    cls_tps: &TypeParams,
    fct_tps: &TypeParams,
) -> bool {
    if def.len() != expr.len() {
        return false;
    }

    for (ind, &arg) in def.iter().enumerate() {
        if !arg_allows(vm, arg, expr[ind], cls_id, fct_id, cls_tps, fct_tps) {
            return false;
        }
    }

    true
}

fn arg_allows(
    vm: &VM,
    def: BuiltinType,
    arg: BuiltinType,
    global_cls_id: Option<ClassId>,
    global_fct_id: Option<FctId>,
    cls_tps: &TypeParams,
    fct_tps: &TypeParams,
) -> bool {
    match def {
        BuiltinType::Error => panic!("error shouldn't occur in fct definition."),
        BuiltinType::Unit
        | BuiltinType::Bool
        | BuiltinType::Byte
        | BuiltinType::Char
        | BuiltinType::Struct(_, _)
        | BuiltinType::Int
        | BuiltinType::Long
        | BuiltinType::Float
        | BuiltinType::Double => def == arg,
        BuiltinType::Nil => panic!("nil should not occur in fct definition."),
        BuiltinType::Ptr => panic!("ptr should not occur in fct definition."),
        BuiltinType::This => panic!("this should not occur in fct definition."),
        BuiltinType::Trait(_) => panic!("trait should not occur in fct definition."),

        BuiltinType::ClassTypeParam(cls_id, tpid) => {
            if def == arg {
                return true;
            }

            if global_cls_id != Some(cls_id) || tpid.idx() >= cls_tps.len() {
                return false;
            }

            arg_allows(
                vm,
                cls_tps[tpid.idx()],
                arg,
                global_cls_id,
                global_fct_id,
                cls_tps,
                fct_tps,
            )
        }
        BuiltinType::FctTypeParam(fct_id, tpid) => {
            if def == arg {
                return true;
            }

            if global_fct_id != Some(fct_id) || tpid.idx() >= fct_tps.len() {
                return false;
            }

            arg_allows(
                vm,
                fct_tps[tpid.idx()],
                arg,
                global_cls_id,
                global_fct_id,
                cls_tps,
                fct_tps,
            )
        }

        BuiltinType::Class(cls_id, list_id) => {
            if def == arg || arg.is_nil() {
                return true;
            }

            let other_cls_id;
            let other_list_id;

            match arg {
                BuiltinType::Class(cls_id, list_id) => {
                    other_cls_id = cls_id;
                    other_list_id = list_id;
                }

                _ => {
                    return false;
                }
            };

            let params = vm.lists.lock().get(list_id);
            let other_params = vm.lists.lock().get(other_list_id);

            if params.len() == 0 && other_params.len() == 0 {
                return arg.subclass_from(vm, def);
            }

            if cls_id != other_cls_id || params.len() != other_params.len() {
                return false;
            }

            for (tp, op) in params.iter().zip(other_params.iter()) {
                if !arg_allows(vm, tp, op, global_cls_id, global_fct_id, cls_tps, fct_tps) {
                    return false;
                }
            }

            true
        }

        BuiltinType::Lambda(_) => {
            // for now expect the exact same params and return types
            // possible improvement: allow super classes for params,
            //                             sub class for return type
            def == arg
        }
    }
}

pub fn check_lit_int(vm: &VM, e: &ExprLitIntType, negative_expr_id: NodeId) -> (BuiltinType, i64) {
    let ty = match e.suffix {
        IntSuffix::Byte => BuiltinType::Byte,
        IntSuffix::Int => BuiltinType::Int,
        IntSuffix::Long => BuiltinType::Long,
    };

    let ty_name = match e.suffix {
        IntSuffix::Byte => "Byte",
        IntSuffix::Int => "Int",
        IntSuffix::Long => "Long",
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
            vm.diag
                .lock()
                .report_without_path(e.pos, Msg::NumberOverflow(ty_name.into()));
        }
    } else {
        let max = match e.suffix {
            IntSuffix::Byte => 256 as u64,
            IntSuffix::Int => u32::max_value() as u64,
            IntSuffix::Long => u64::max_value() as u64,
        };

        if val > max {
            vm.diag
                .lock()
                .report_without_path(e.pos, Msg::NumberOverflow(ty_name.into()));
        }
    }

    let val = if negative {
        (!val + 1) as i64
    } else {
        val as i64
    };

    (ty, val)
}

pub fn check_lit_float(
    vm: &VM,
    e: &ExprLitFloatType,
    negative_expr_id: NodeId,
) -> (BuiltinType, f64) {
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
            FloatSuffix::Float => "Float",
            FloatSuffix::Double => "Double",
        };

        vm.diag
            .lock()
            .report_without_path(e.pos, Msg::NumberOverflow(ty.into()));
    }

    (ty, value)
}

#[derive(Copy, Clone)]
enum LookupKind {
    Fct,
    Method(BuiltinType),
    Static(ClassId),
    Callee(FctId),
    Ctor(ClassId),
}

struct MethodLookup<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    kind: Option<LookupKind>,
    name: Option<Name>,
    args: Option<&'a [BuiltinType]>,
    cls_tps: Option<&'a TypeParams>,
    fct_tps: Option<&'a TypeParams>,
    ret: Option<BuiltinType>,
    pos: Option<Position>,

    found_fct_id: Option<FctId>,
    found_cls_id: Option<ClassId>,
    found_ret: Option<BuiltinType>,

    found_multiple_functions: bool,
}

impl<'a, 'ast> MethodLookup<'a, 'ast> {
    fn new(vm: &'a VM<'ast>) -> MethodLookup<'a, 'ast> {
        MethodLookup {
            vm: vm,
            kind: None,
            name: None,
            args: None,
            cls_tps: None,
            fct_tps: None,
            ret: None,
            pos: None,

            found_fct_id: None,
            found_cls_id: None,
            found_ret: None,

            found_multiple_functions: false,
        }
    }

    fn ctor(mut self, cls_id: ClassId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Ctor(cls_id));
        self
    }

    fn callee(mut self, fct_id: FctId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Callee(fct_id));
        self
    }

    fn method(mut self, obj: BuiltinType) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Method(obj));
        self
    }

    fn static_method(mut self, cls_id: ClassId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Static(cls_id));
        self
    }

    fn fct(mut self) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Fct);
        self
    }

    fn args(mut self, args: &'a [BuiltinType]) -> MethodLookup<'a, 'ast> {
        self.args = Some(args);
        self
    }

    fn pos(mut self, pos: Position) -> MethodLookup<'a, 'ast> {
        self.pos = Some(pos);
        self
    }

    fn cls_type_params(mut self, cls_tps: &'a TypeParams) -> MethodLookup<'a, 'ast> {
        self.cls_tps = Some(cls_tps);
        self
    }

    fn fct_type_params(mut self, fct_tps: &'a TypeParams) -> MethodLookup<'a, 'ast> {
        self.fct_tps = Some(fct_tps);
        self
    }

    fn name(mut self, name: Name) -> MethodLookup<'a, 'ast> {
        self.name = Some(name);
        self
    }

    fn return_type(mut self, ret: BuiltinType) -> MethodLookup<'a, 'ast> {
        self.ret = Some(ret);
        self
    }

    fn find(&mut self) -> bool {
        let kind = self.kind.expect("kind not set");
        let args = self.args.expect("args not set");

        let fct_id = match kind {
            LookupKind::Fct => {
                assert!(self.cls_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_fct(name)
            }

            LookupKind::Callee(fct_id) => Some(fct_id),

            LookupKind::Method(obj) => {
                if let Some(cls_id) = obj.cls_id(self.vm) {
                    let name = self.name.expect("name not set");
                    self.find_method(cls_id, name, false)
                } else {
                    None
                }
            }

            LookupKind::Static(cls_id) => {
                assert!(self.cls_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_method(cls_id, name, true)
            }

            LookupKind::Ctor(cls_id) => {
                assert!(self.cls_tps.is_some());
                self.find_ctor(cls_id)
            }
        };

        self.found_fct_id = fct_id;

        let fct_id = if let Some(fct_id) = fct_id {
            fct_id
        } else {
            let name = match kind {
                LookupKind::Ctor(cls_id) => {
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    cls.name
                }
                _ => self.name.expect("name not set"),
            };

            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<String>>();

            let msg = match kind {
                LookupKind::Fct => Msg::Unimplemented,
                LookupKind::Callee(_) => unreachable!(),
                LookupKind::Method(obj) => {
                    let type_name = obj.name(self.vm);

                    if self.found_multiple_functions {
                        Msg::MultipleCandidatesForMethod(type_name, name, param_names)
                    } else {
                        Msg::UnknownMethod(type_name, name, param_names)
                    }
                }

                LookupKind::Static(cls_id) => {
                    let type_name = self.vm.cls(cls_id).name(self.vm);
                    Msg::UnknownStaticMethod(type_name, name, param_names)
                }

                LookupKind::Ctor(cls_id) => {
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let name = self.vm.interner.str(cls.name).to_string();
                    Msg::UnknownCtor(name, param_names)
                }
            };

            self.vm
                .diag
                .lock()
                .report_without_path(self.pos.expect("pos not set"), msg);
            return false;
        };

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let cls_id = match fct.parent {
            FctParent::Class(cls_id) => Some(cls_id),
            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                Some(ximpl.cls_id())
            }
            _ => None,
        };

        self.found_cls_id = cls_id;

        let cls_tps: TypeParams = if let Some(cls_tps) = self.cls_tps {
            cls_tps.clone()
        } else if let LookupKind::Method(obj) = kind {
            obj.type_params(self.vm)
        } else {
            TypeParams::empty()
        };

        if cls_id.is_some() && !self.check_cls_tps(&cls_tps) {
            return false;
        }

        let fct_tps: TypeParams = if let Some(fct_tps) = self.fct_tps {
            if !self.check_fct_tps(fct_tps) {
                return false;
            }

            fct_tps.clone()
        } else {
            TypeParams::empty()
        };

        if !args_compatible(
            self.vm,
            &fct.params_without_self(),
            args,
            cls_id,
            Some(fct_id),
            &cls_tps,
            &fct_tps,
        ) {
            let fct_name = self.vm.interner.str(fct.name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let call_types = args.iter().map(|a| a.name(self.vm)).collect::<Vec<_>>();
            let msg = Msg::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.vm
                .diag
                .lock()
                .report_without_path(self.pos.expect("pos not set"), msg);
            return false;
        }

        let cmp_type = match kind {
            LookupKind::Ctor(cls_id) => {
                let list_id = self.vm.lists.lock().insert(cls_tps);
                BuiltinType::Class(cls_id, list_id)
            }

            _ => replace_type_param(self.vm, fct.return_type, &cls_tps, &fct_tps),
        };

        if self.ret.is_none() || self.ret.unwrap() == cmp_type {
            self.found_ret = Some(cmp_type);
            true
        } else {
            false
        }
    }

    fn find_fct(&self, _: Name) -> Option<FctId> {
        unimplemented!()
    }

    fn find_ctor(&self, cls_id: ClassId) -> Option<FctId> {
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        let type_params = self.cls_tps.as_ref().unwrap();
        let args = self.args.unwrap();

        if let Some(ctor_id) = cls.constructor {
            let ctor = self.vm.fcts.idx(ctor_id);
            let ctor = ctor.read();

            if args_compatible(
                self.vm,
                &ctor.params_without_self(),
                &args,
                Some(cls_id),
                None,
                type_params,
                &TypeParams::empty(),
            ) {
                return Some(ctor_id);
            }
        }

        None
    }

    fn find_method(&mut self, cls_id: ClassId, name: Name, is_static: bool) -> Option<FctId> {
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        let candidates = cls.find_methods(self.vm, name, is_static);

        self.found_multiple_functions = candidates.len() > 1;

        if candidates.len() == 1 {
            Some(candidates[0])
        } else {
            None
        }
    }

    fn check_cls_tps(&self, tps: &TypeParams) -> bool {
        let cls_tps = {
            let cls_id = self.found_cls_id.expect("found_cls_id not set");
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            cls.type_params.to_vec()
        };

        self.check_tps(&cls_tps, tps)
    }

    fn check_fct_tps(&self, tps: &TypeParams) -> bool {
        let fct_tps = {
            let fct_id = self.found_fct_id.expect("found_fct_id not set");

            let fct = self.vm.fcts.idx(fct_id);
            let fct = fct.read();
            fct.type_params.to_vec()
        };

        self.check_tps(&fct_tps, tps)
    }

    fn check_tps(&self, specified_tps: &[vm::TypeParam], tps: &TypeParams) -> bool {
        if specified_tps.len() != tps.len() {
            let msg = Msg::WrongNumberTypeParams(specified_tps.len(), tps.len());
            self.vm
                .diag
                .lock()
                .report_without_path(self.pos.expect("pos not set"), msg);
            return false;
        }

        let mut succeeded = true;

        for (tp, ty) in specified_tps.iter().zip(tps.iter()) {
            if ty.is_type_param() {
                let ok = match ty {
                    BuiltinType::ClassTypeParam(cls_id, tpid) => {
                        let cls = self.vm.classes.idx(cls_id);
                        let cls = cls.read();
                        self.check_tp_against_tp(tp, &cls.type_params[tpid.idx()], ty)
                    }

                    BuiltinType::FctTypeParam(fct_id, tpid) => {
                        let fct = self.vm.fcts.idx(fct_id);
                        let fct = fct.read();
                        self.check_tp_against_tp(tp, &fct.type_params[tpid.idx()], ty)
                    }

                    _ => unreachable!(),
                };

                if !ok {
                    succeeded = false;
                }
            } else if !self.check_tp(tp, ty) {
                succeeded = false;
            }
        }

        succeeded
    }

    fn check_tp(&self, tp: &vm::TypeParam, ty: BuiltinType) -> bool {
        let mut succeeded = true;

        if let Some(cls_id) = tp.class_bound {
            let cls = self.vm.cls(cls_id);
            if !ty.subclass_from(self.vm, cls) {
                self.fail_cls_bound(cls_id, ty);
                succeeded = false;
            }
        }

        let cls_id = ty.cls_id(self.vm).unwrap();
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        for &trait_bound in &tp.trait_bounds {
            if !cls.traits.contains(&trait_bound) {
                self.fail_trait_bound(trait_bound, ty);
                succeeded = false;
            }
        }

        succeeded
    }

    fn check_tp_against_tp(
        &self,
        tp: &vm::TypeParam,
        arg: &vm::TypeParam,
        arg_ty: BuiltinType,
    ) -> bool {
        let mut succeeded = true;

        if let Some(cls_id) = tp.class_bound {
            if tp.class_bound != arg.class_bound {
                self.fail_cls_bound(cls_id, arg_ty);
                succeeded = false;
            }
        }

        if tp.trait_bounds.len() == 0 {
            return succeeded;
        }

        let traits_set = arg.trait_bounds.iter().collect::<HashSet<_>>();

        for &trait_bound in &tp.trait_bounds {
            if !traits_set.contains(&trait_bound) {
                self.fail_trait_bound(trait_bound, arg_ty);
                succeeded = false;
            }
        }

        succeeded
    }

    fn fail_cls_bound(&self, cls_id: ClassId, ty: BuiltinType) {
        let name = ty.name(self.vm);
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();
        let cls = self.vm.interner.str(cls.name).to_string();

        let msg = Msg::ClassBoundNotSatisfied(name, cls);
        self.vm
            .diag
            .lock()
            .report_without_path(self.pos.expect("pos not set"), msg);
    }

    fn fail_trait_bound(&self, trait_id: TraitId, ty: BuiltinType) {
        let bound = self.vm.traits[trait_id].read();
        let name = ty.name(self.vm);
        let trait_name = self.vm.interner.str(bound.name).to_string();
        let msg = Msg::TraitBoundNotSatisfied(name, trait_name);
        self.vm
            .diag
            .lock()
            .report_without_path(self.pos.expect("pos not set"), msg);
    }

    fn found_fct_id(&self) -> Option<FctId> {
        self.found_fct_id
    }

    fn found_cls_id(&self) -> Option<ClassId> {
        self.found_cls_id
    }

    fn found_ret(&self) -> Option<BuiltinType> {
        self.found_ret
    }
}

pub fn lookup_method<'ast>(
    vm: &VM<'ast>,
    object_type: BuiltinType,
    is_static: bool,
    name: Name,
    args: &[BuiltinType],
    fct_tps: &TypeParams,
    return_type: Option<BuiltinType>,
) -> Option<(ClassId, FctId, BuiltinType)> {
    let values: Option<(ClassId, TypeParams)> = match object_type {
        BuiltinType::Class(cls_id, list_id) if !is_static => {
            let params = vm.lists.lock().get(list_id);
            Some((cls_id, params))
        }
        _ => vm
            .vips
            .find_class(object_type)
            .map(|c| (c, TypeParams::empty())),
    };

    if let Some((cls_id, ref cls_type_params)) = values {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        let candidates = cls.find_methods(vm, name, is_static);

        if candidates.len() == 1 {
            let candidate = candidates[0];
            let method = vm.fcts.idx(candidate);
            let method = method.read();

            let cls_id = match method.parent {
                FctParent::Class(cls_id) => cls_id,
                FctParent::Impl(impl_id) => {
                    let ximpl = vm.impls[impl_id].read();
                    ximpl.cls_id()
                }

                _ => unreachable!(),
            };

            if args_compatible(
                vm,
                &method.params_without_self(),
                args,
                Some(cls_id),
                Some(method.id),
                cls_type_params,
                fct_tps,
            ) {
                let cmp_type =
                    replace_type_param(vm, method.return_type, &cls_type_params, fct_tps);

                if return_type.is_none() || return_type.unwrap() == cmp_type {
                    return Some((cls_id, candidate, cmp_type));
                }
            }
        }
    }

    None
}

fn replace_type_param(
    vm: &VM,
    ty: BuiltinType,
    cls_tp: &TypeParams,
    fct_tp: &TypeParams,
) -> BuiltinType {
    if cls_tp.len() == 0 && fct_tp.len() == 0 {
        return ty;
    }

    match ty {
        BuiltinType::ClassTypeParam(_, tpid) => cls_tp[tpid.idx()],
        BuiltinType::FctTypeParam(_, tpid) => fct_tp[tpid.idx()],

        BuiltinType::Class(cls_id, list_id) => {
            let params = vm.lists.lock().get(list_id);

            let params: TypeParams = params
                .iter()
                .map(|p| replace_type_param(vm, p, cls_tp, fct_tp))
                .collect::<Vec<_>>()
                .into();

            let list_id = vm.lists.lock().insert(params);
            BuiltinType::Class(cls_id, list_id)
        }

        BuiltinType::Lambda(_) => unimplemented!(),

        _ => ty,
    }
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use crate::vm::ConstValue;
    use dora_parser::error::msg::Msg;

    #[test]
    fn type_method_len() {
        ok("fun f(a: String) -> Int { return a.length(); }");
        ok("fun f(a: String) -> Int { return \"abc\".length(); }");
    }

    #[test]
    fn type_object_field() {
        ok("class Foo(let a:Int) fun f(x: Foo) -> Int { return x.a; }");
        ok("class Foo(let a:String) fun f(x: Foo) -> String { return x.a; }");
        err(
            "class Foo(let a:Int) fun f(x: Foo) -> Bool { return x.a; }",
            pos(1, 46),
            Msg::ReturnType("Bool".into(), "Int".into()),
        );
        err(
            "class Foo(let a:Int) fun f(x: Foo) -> Int { return x.b; }",
            pos(1, 53),
            Msg::UnknownField("b".into(), "Foo".into()),
        );
    }

    #[test]
    fn type_object_set_field() {
        ok("class Foo(var a: Int) fun f(x: Foo) { x.a = 1; }");
        err(
            "class Foo(var a: Int) fun f(x: Foo) { x.a = false; }",
            pos(1, 43),
            Msg::AssignField("a".into(), "Foo".into(), "Int".into(), "Bool".into()),
        );
    }

    #[test]
    fn type_object_field_without_self() {
        err(
            "class Foo(let a: Int) { fun f() -> Int { return a; } }",
            pos(1, 49),
            Msg::UnknownIdentifier("a".into()),
        );
        err(
            "class Foo(var a: Int) { fun set(x: Int) { a = x; } }",
            pos(1, 43),
            Msg::UnknownIdentifier("a".into()),
        );
    }

    #[test]
    fn type_method_call() {
        ok("class Foo {
                fun bar() {}
                fun baz() -> Int { return 1; }
            }

            fun f(x: Foo) { x.bar(); }
            fun g(x: Foo) -> Int { return x.baz(); }");

        err(
            "class Foo {
                 fun bar() -> Int { return 0; }
             }

             fun f(x: Foo) -> String { return x.bar(); }",
            pos(5, 40),
            Msg::ReturnType("String".into(), "Int".into()),
        );
    }

    #[test]
    fn type_method_defined_twice() {
        err(
            "class Foo {
                 fun bar() {}
                 fun bar() {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)),
        );

        err(
            "class Foo {
                 fun bar() {}
                 fun bar() -> Int {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)),
        );

        err(
            "class Foo {
                 fun bar(a: Int) {}
                 fun bar(a: Int) -> Int {}
             }",
            pos(3, 18),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 18)),
        );

        err(
            "class Foo {
                fun bar(a: Int) {}
                fun bar(a: String) {}
            }",
            pos(3, 17),
            Msg::MethodExists("Foo".into(), "bar".into(), pos(2, 17)),
        );
    }

    #[test]
    fn type_self() {
        ok("class Foo { fun me() -> Foo { return self; } }");
        err(
            "class Foo fun me() { return self; }",
            pos(1, 29),
            Msg::ThisUnavailable,
        );

        ok("class Foo(let a: Int, let b: Int) {
            fun bar() -> Int { return self.a + self.b; }
        }");

        ok("class Foo(var a: Int) {
            fun setA(a: Int) { self.a = a; }
        }");

        ok("class Foo {
            fun zero() -> Int { return 0; }
            fun other() -> Int { return self.zero(); }
        }");

        ok("class Foo {
            fun bar() { self.bar(); }
        }");
    }

    #[test]
    fn type_unknown_method() {
        err(
            "class Foo {
                 fun bar(a: Int) { }
             }

             fun f(x: Foo) { x.bar(); }",
            pos(5, 35),
            Msg::ParamTypesIncompatible("bar".into(), vec!["Int".into()], Vec::new()),
        );

        err(
            "class Foo { }
              fun f(x: Foo) { x.bar(1); }",
            pos(2, 36),
            Msg::UnknownMethod("Foo".into(), "bar".into(), vec!["Int".into()]),
        );
    }

    #[test]
    fn type_ctor() {
        ok("class Foo fun f() -> Foo { return Foo(); }");
        ok("class Foo(let a: Int) fun f() -> Foo { return Foo(1); }");
        err(
            "class Foo fun f() -> Foo { return 1; }",
            pos(1, 28),
            Msg::ReturnType("Foo".into(), "Int".into()),
        );
    }

    #[test]
    fn type_def_for_return_type() {
        ok("fun a() -> Int { return 1; }");
        err(
            "fun a() -> unknown {}",
            pos(1, 12),
            Msg::UnknownType("unknown".into()),
        );
    }

    #[test]
    fn type_def_for_param() {
        ok("fun a(b: Int) {}");
        err(
            "fun a(b: foo) {}",
            pos(1, 10),
            Msg::UnknownType("foo".into()),
        );
    }

    #[test]
    fn type_def_for_var() {
        ok("fun a() { let a : Int = 1; }");
        err(
            "fun a() { let a : test = 1; }",
            pos(1, 19),
            Msg::UnknownType("test".into()),
        );
    }

    #[test]
    fn type_var_needs_expr_or_definition() {
        err(
            "fun a() { let a; }",
            pos(1, 11),
            Msg::VarNeedsTypeInfo("a".into()),
        );
    }

    #[test]
    fn type_var_wrong_type_defined() {
        ok("fun f() { let a : Int = 1; }");
        ok("fun f() { let a : Bool = false; }");
        ok("fun f() { let a : String = \"f\"; }");

        err(
            "fun f() { let a : Int = true; }",
            pos(1, 11),
            Msg::AssignType("a".into(), "Int".into(), "Bool".into()),
        );
        err(
            "fun f() { let b : Bool = 2; }",
            pos(1, 11),
            Msg::AssignType("b".into(), "Bool".into(), "Int".into()),
        );
    }

    #[test]
    fn type_while() {
        ok("fun x() { while true { } }");
        ok("fun x() { while false { } }");
        err(
            "fun x() { while 2 { } }",
            pos(1, 11),
            Msg::WhileCondType("Int".into()),
        );
    }

    #[test]
    fn type_if() {
        ok("fun x() { if true { } }");
        ok("fun x() { if false { } }");
        err(
            "fun x() { if 4 { } }",
            pos(1, 11),
            Msg::IfCondType("Int".into()),
        );
    }

    #[test]
    fn type_return_unit() {
        ok("fun f() { return; }");
        err(
            "fun f() { return 1; }",
            pos(1, 11),
            Msg::ReturnType("()".into(), "Int".into()),
        );
    }

    #[test]
    fn type_return() {
        ok("fun f() -> Int { let a = 1; return a; }");
        ok("fun f() -> Int { return 1; }");
        err(
            "fun f() -> Int { return; }",
            pos(1, 18),
            Msg::ReturnType("Int".into(), "()".into()),
        );

        ok("fun f() -> Int { return 0; }
            fun g() -> Int { return f(); }");
        err(
            "fun f() { }
             fun g() -> Int { return f(); }",
            pos(2, 31),
            Msg::ReturnType("Int".into(), "()".into()),
        );
    }

    #[test]
    fn type_variable() {
        ok("fun f(a: Int) { let b: Int = a; }");
    }

    #[test]
    fn type_assign_lvalue() {
        err("fun f() { 1 = 3; }", pos(1, 13), Msg::LvalueExpected);
    }

    #[test]
    fn type_un_op() {
        ok("fun f(a: Int) { !a; -a; +a; }");
        err(
            "fun f(a: Bool) { -a; }",
            pos(1, 18),
            Msg::UnOpType("-".into(), "Bool".into()),
        );
        err(
            "fun f(a: Bool) { +a; }",
            pos(1, 18),
            Msg::UnOpType("+".into(), "Bool".into()),
        );
    }

    #[test]
    fn type_bin_op() {
        ok("fun f(a: Int) { a+a; a-a; a*a; a/a; a%a; }");
        ok("fun f(a: Int) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fun f(a: String) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
        ok("fun f(a: String) { a===a; a!==a; a+a; }");
        ok("class Foo fun f(a: Foo) { a===a; a!==a; }");
        ok("fun f(a: Int) { a|a; a&a; a^a; }");
        ok("fun f(a: Bool) { a||a; a&&a; }");

        err(
            "class A class B fun f(a: A, b: B) { a === b; }",
            pos(1, 39),
            Msg::TypesIncompatible("A".into(), "B".into()),
        );
        err(
            "class A class B fun f(a: A, b: B) { b !== a; }",
            pos(1, 39),
            Msg::TypesIncompatible("B".into(), "A".into()),
        );
        err(
            "fun f(a: Bool) { a+a; }",
            pos(1, 19),
            Msg::BinOpType("+".into(), "Bool".into(), "Bool".into()),
        );
        err(
            "fun f(a: Bool) { a^a; }",
            pos(1, 19),
            Msg::BinOpType("^".into(), "Bool".into(), "Bool".into()),
        );
        err(
            "fun f(a: Int) { a||a; }",
            pos(1, 18),
            Msg::BinOpType("||".into(), "Int".into(), "Int".into()),
        );
        err(
            "fun f(a: Int) { a&&a; }",
            pos(1, 18),
            Msg::BinOpType("&&".into(), "Int".into(), "Int".into()),
        );
        err(
            "fun f(a: String) { a-a; }",
            pos(1, 21),
            Msg::BinOpType("-".into(), "String".into(), "String".into()),
        );
        err(
            "fun f(a: String) { a*a; }",
            pos(1, 21),
            Msg::BinOpType("*".into(), "String".into(), "String".into()),
        );
        err(
            "fun f(a: String) { a%a; }",
            pos(1, 21),
            Msg::BinOpType("%".into(), "String".into(), "String".into()),
        );
    }

    #[test]
    fn type_function_return_type() {
        ok("fun foo() -> Int { return 1; }\nfun f() { let i: Int = foo(); }");
        err(
            "fun foo() -> Int { return 1; }\nfun f() { let i: Bool = foo(); }",
            pos(2, 11),
            Msg::AssignType("i".into(), "Bool".into(), "Int".into()),
        );
    }

    #[test]
    fn type_ident_in_function_params() {
        ok("fun f(a: Int) {}\nfun g() { let a = 1; f(a); }");
    }

    #[test]
    fn type_recursive_function_call() {
        ok("fun f(a: Int) { f(a); }");
    }

    #[test]
    fn type_function_params() {
        ok("fun foo() {}\nfun f() { foo(); }");
        ok("fun foo(a: Int) {}\nfun f() { foo(1); }");
        ok("fun foo(a: Int, b: Bool) {}\nfun f() { foo(1, true); }");

        err(
            "fun foo() {}\nfun f() { foo(1); }",
            pos(2, 14),
            Msg::ParamTypesIncompatible("foo".into(), vec![], vec!["Int".into()]),
        );
        err(
            "fun foo(a: Int) {}\nfun f() { foo(true); }",
            pos(2, 14),
            Msg::ParamTypesIncompatible("foo".into(), vec!["Int".into()], vec!["Bool".into()]),
        );
        err(
            "fun foo(a: Int, b: Bool) {}\nfun f() { foo(1, 2); }",
            pos(2, 14),
            Msg::ParamTypesIncompatible(
                "foo".into(),
                vec!["Int".into(), "Bool".into()],
                vec!["Int".into(), "Int".into()],
            ),
        );
    }

    #[test]
    fn type_return_nil() {
        ok("fun foo() -> String { return nil; }");
        ok("class Foo fun foo() -> Foo { return nil; }");
        err(
            "fun foo() -> Int { return nil; }",
            pos(1, 20),
            Msg::IncompatibleWithNil("Int".into()),
        );
    }

    #[test]
    fn type_nil_as_argument() {
        ok("fun foo(a: String) {} fun test() { foo(nil); }");
        err(
            "fun foo(a: Int) {} fun test() { foo(nil); }",
            pos(1, 36),
            Msg::ParamTypesIncompatible("foo".into(), vec!["Int".into()], vec!["nil".into()]),
        );
    }

    #[test]
    fn type_nil_for_ctor() {
        ok("class Foo(let a: String) fun test() { Foo(nil); }");
        err(
            "class Foo(let a: Int) fun test() { Foo(nil); }",
            pos(1, 39),
            Msg::UnknownCtor("Foo".into(), vec!["nil".into()]),
        );
    }

    #[test]
    fn type_nil_for_local_variable() {
        ok("fun f() { let x: String = nil; }");
        err(
            "fun f() { let x: Int = nil; }",
            pos(1, 11),
            Msg::AssignType("x".into(), "Int".into(), "nil".into()),
        );
    }

    #[test]
    fn type_nil_for_field() {
        ok("class Foo(var a: String) fun f() { Foo(nil).a = nil; }");
        err(
            "class Foo(var a: Int) fun f() { Foo(1).a = nil; }",
            pos(1, 42),
            Msg::AssignField("a".into(), "Foo".into(), "Int".into(), "nil".into()),
        );
    }

    #[test]
    fn type_nil_method() {
        err(
            "fun f() { nil.test(); }",
            pos(1, 19),
            Msg::UnknownMethod("nil".into(), "test".into(), Vec::new()),
        );
    }

    #[test]
    fn type_nil_as_method_argument() {
        ok("class Foo {
            fun f(a: String) {}
        } fun f() { Foo().f(nil); }");
    }

    #[test]
    fn type_array() {
        ok("fun f(a: Array[Int]) -> Int { return a.get(1); }");
        err(
            "fun f(a: Array[Int]) -> String { return a.get(1); }",
            pos(1, 34),
            Msg::ReturnType("String".into(), "Int".into()),
        );
    }

    #[test]
    fn type_array_assign() {
        err(
            "fun f(a: Array[Int]) -> Int { return a.set(3, 4); }",
            pos(1, 31),
            Msg::ReturnType("Int".into(), "()".into()),
        );
        err(
            "fun f(a: Array[Int]) { a.set(3, \"b\"); }",
            pos(1, 29),
            Msg::ParamTypesIncompatible(
                "set".into(),
                vec!["Int".into(), "T".into()],
                vec!["Int".into(), "String".into()],
            ),
        );
    }

    #[test]
    fn type_throw() {
        ok("fun f() { throw \"abc\"; }");
        ok("fun f() { throw Array[Int](0); }");
        err(
            "fun f() { throw 1; }",
            pos(1, 11),
            Msg::ReferenceTypeExpected("Int".into()),
        );
        err("fun f() { throw nil; }", pos(1, 11), Msg::ThrowNil);
    }

    #[test]
    fn type_defer() {
        ok("fun foo() { }
            fun f() { defer foo(); }");

        err(
            "fun foo(a: Int) {} fun f() { defer foo();}",
            pos(1, 39),
            Msg::ParamTypesIncompatible("foo".into(), vec!["Int".into()], vec![]),
        );

        err("fun f() { defer 1; }", pos(1, 11), Msg::FctCallExpected);
    }

    #[test]
    fn type_catch_variable() {
        ok("fun f() { do {} catch a: String { print(a); } }");
        ok("fun f() { var x = 0; do {} catch a: Array[Int] { x=a.length(); } }");
    }

    #[test]
    fn try_value_type() {
        err(
            "fun f() { do {} catch a: Int {} }",
            pos(1, 26),
            Msg::ReferenceTypeExpected("Int".into()),
        );
    }

    #[test]
    fn try_missing_catch() {
        err("fun f() { do {} }", pos(1, 11), Msg::CatchOrFinallyExpected);
    }

    #[test]
    fn try_check_blocks() {
        err(
            "fun f() { do {} catch a: Array[Int] {} a.len(); }",
            pos(1, 40),
            Msg::UnknownIdentifier("a".into()),
        );
        err(
            "fun f() { do {} catch a: Array[Int] {} finally { a.len(); } }",
            pos(1, 50),
            Msg::UnknownIdentifier("a".into()),
        );
        err(
            "fun f() { do { return a; } catch a: Array[Int] {} }",
            pos(1, 23),
            Msg::UnknownIdentifier("a".into()),
        );
        err(
            "fun f() { do { } catch a: Array[Int] { return a; } }",
            pos(1, 40),
            Msg::ReturnType("()".into(), "Array[Int]".into()),
        );
    }

    #[test]
    fn let_without_initialization() {
        err(
            "fun f() { let x: Int; }",
            pos(1, 11),
            Msg::LetMissingInitialization,
        );
    }

    #[test]
    fn var_without_initialization() {
        ok("fun f() { var x: Int; }");
    }

    #[test]
    fn reassign_param() {
        ok("fun f(var a: Int) { a = 1; }");
        err("fun f(a: Int) { a = 1; }", pos(1, 19), Msg::LetReassigned);
    }

    #[test]
    fn reassign_field() {
        ok("class Foo(var x: Int) fun foo(var f: Foo) { f.x = 1; }");
        err(
            "class Foo(let x: Int) fun foo(var f: Foo) { f.x = 1; }",
            pos(1, 49),
            Msg::LetReassigned,
        );
    }

    #[test]
    fn reassign_catch() {
        err(
            "fun f() {
               do {
                 throw \"test\";
               } catch x: Array[Int] {
                 x = Array[Int](0);
               }
             }",
            pos(5, 20),
            Msg::LetReassigned,
        );
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
        err(
            "class Foo {
            fun f() { self = Foo(); }
        }",
            pos(2, 28),
            Msg::LvalueExpected,
        );
    }

    #[test]
    fn super_class() {
        ok("@open class A class B: A");
        ok("@open class A class B: A()");
        ok("@open class A(a: Int) class B: A(1)");
        err(
            "@open class A(a: Int) class B: A(true)",
            pos(1, 32),
            Msg::UnknownCtor("A".into(), vec!["Bool".into()]),
        );
    }

    #[test]
    fn access_super_class_field() {
        ok("@open class A(var a: Int) class B(x: Int): A(x*2)
            fun foo(b: B) { b.a = b.a + 10; }");
    }

    #[test]
    fn check_is() {
        ok("@open class A class B: A
            fun f(a: A) -> Bool { return a is B; }");
        ok("@open class A class B: A
            fun f(b: B) -> Bool { return b is A; }");
        ok("class A
            fun f(a: A) -> Bool { return a is A; }");
        err(
            "@open class A class B: A
             fun f(a: A) -> Bool { return a is String; }",
            pos(2, 45),
            Msg::TypesIncompatible("A".into(), "String".into()),
        );
        err(
            "@open class A class B: A class C
             fun f(a: A) -> Bool { return a is C; }",
            pos(2, 45),
            Msg::TypesIncompatible("A".into(), "C".into()),
        );

        ok("@open class A() class B(): A() fun f() -> A { return B(); }");
        ok("@open class A() class B(): A() fun f() { let a: A = B(); }");
    }

    #[test]
    fn check_as() {
        ok("@open class A class B: A
            fun f(a: A) -> B { return a as B; }");
        ok("class A
            fun f(a: A) -> A { return a as A; }");
        err(
            "@open class A class B: A
             fun f(a: A) -> String { return a as String; }",
            pos(2, 47),
            Msg::TypesIncompatible("A".into(), "String".into()),
        );
        err(
            "@open class A class B: A class C
             fun f(a: A) -> C { return a as C; }",
            pos(2, 42),
            Msg::TypesIncompatible("A".into(), "C".into()),
        );
    }

    #[test]
    fn check_upcast() {
        ok("@open class A class B: A
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
        ok("fun f(x: String) {
                let a = nil === x;
                let b = x === nil;
                let c = nil === nil;
            }");
    }

    #[test]
    fn super_delegation() {
        ok("@open class A { fun f() {} }
            class B: A { fun g() {} }

            fun foo(b: B) {
                b.f();
                b.g();
            }");
    }

    #[test]
    fn super_method_call() {
        ok("@open class A { @open fun f() -> Int { return 1; } }
            class B: A { @new_call @override fun f() -> Int { return super.f() + 1; } }");
    }

    #[test]
    fn super_as_normal_expression() {
        err(
            "@open class A { }
            class B: A { fun me() { let x = super; } }",
            pos(2, 45),
            Msg::SuperNeedsMethodCall,
        );
    }

    #[test]
    fn try_with_non_call() {
        err("fun me() { try 1; }", pos(1, 12), Msg::TryNeedsCall);
    }

    #[test]
    fn try_fct() {
        ok("fun one() throws -> Int { return 1; } fun me() -> Int { return try one(); }");
    }

    #[test]
    fn throws_fct_without_try() {
        err(
            "fun one() throws -> Int { return 1; } fun me() -> Int { return one(); }",
            pos(1, 67),
            Msg::ThrowingCallWithoutTry,
        );
    }

    #[test]
    fn try_fct_non_throwing() {
        err(
            "fun one() -> Int { return 1; }
             fun me() -> Int { return try one(); }",
            pos(2, 39),
            Msg::TryCallNonThrowing,
        );
    }

    #[test]
    fn try_method() {
        ok("class Foo { fun one() throws -> Int { return 1; } }
            fun me() -> Int { return try Foo().one(); }");
    }

    #[test]
    fn throws_method_without_try() {
        err(
            "class Foo { fun one() throws -> Int { return 1; } }
             fun me() -> Int { return Foo().one(); }",
            pos(2, 48),
            Msg::ThrowingCallWithoutTry,
        );
    }

    #[test]
    fn try_method_non_throwing() {
        err(
            "class Foo() { fun one() -> Int { return 1; } }
             fun me() -> Int { return try Foo().one(); }",
            pos(2, 39),
            Msg::TryCallNonThrowing,
        );
    }

    #[test]
    fn try_else() {
        ok("fun one() throws -> Int { return 1; }
            fun me() -> Int { return try one() else 0; }");
        err(
            "fun one() throws -> Int { return 1; }
             fun me() -> Int { return try one() else \"bla\"; }",
            pos(2, 39),
            Msg::TypesIncompatible("Int".into(), "String".into()),
        );
        err(
            "fun one() throws -> Int { return 1; }
             fun me() -> Int { return try one() else false; }",
            pos(2, 39),
            Msg::TypesIncompatible("Int".into(), "Bool".into()),
        );
    }

    #[test]
    fn struct_lit() {
        ok("struct Foo {} fun foo() -> Foo { return Foo; }");
        ok("struct Foo {} fun foo() { let x = Foo; }");
        ok("struct Foo {} fun foo() { let x: Foo = Foo; }");
        err(
            "struct Foo {} fun foo() { let x: Int = Foo; }",
            pos(1, 27),
            Msg::AssignType("x".into(), "Int".into(), "Foo".into()),
        );
        err(
            "struct Foo {} fun foo() -> Int { return Foo; }",
            pos(1, 34),
            Msg::ReturnType("Int".into(), "Foo".into()),
        );
    }

    #[test]
    fn lit_long() {
        ok("fun f() -> Long { return 1L; }");
        ok("fun f() -> Int { return 1; }");

        let ret = Msg::ReturnType("Int".into(), "Long".into());
        err("fun f() -> Int { return 1L; }", pos(1, 18), ret);

        let ret = Msg::ReturnType("Long".into(), "Int".into());
        err("fun f() -> Long { return 1; }", pos(1, 19), ret);
    }

    #[test]
    fn overload_plus() {
        ok("class A { fun plus(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() + A(); }");
    }

    #[test]
    fn overload_minus() {
        ok("class A { fun minus(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() - A(); }");
    }

    #[test]
    fn overload_times() {
        ok("class A { fun times(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() * A(); }");
    }

    #[test]
    fn overload_div() {
        ok("class A { fun div(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() / A(); }");
    }

    #[test]
    fn overload_mod() {
        ok("class A { fun mod(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() % A(); }");
    }

    #[test]
    fn overload_bitwise_or() {
        ok("class A { fun bitwiseOr(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() | A(); }");
    }

    #[test]
    fn overload_bitwise_and() {
        ok("class A { fun bitwiseAnd(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() & A(); }");
    }

    #[test]
    fn overload_bitwise_xor() {
        ok("class A { fun bitwiseXor(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() ^ A(); }");
    }

    #[test]
    fn overload_shl() {
        ok("class A { fun shiftLeft(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() << A(); }");
    }

    #[test]
    fn overload_sar() {
        ok("class A { fun shiftRight(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() >> A(); }");
    }

    #[test]
    fn overload_shr() {
        ok(
            "class A { fun unsignedShiftRight(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() >>> A(); }",
        );
    }

    #[test]
    fn overload_equals() {
        ok("class A { fun equals(rhs: A) -> Bool { return true; } }
            fun f1() -> Bool { return A() == A(); }
            fun f2() -> Bool { return A() != A(); }");
    }

    #[test]
    fn overload_compare_to() {
        ok("class A { fun compareTo(rhs: A) -> Int { return 0; } }
            fun f1() -> Bool { return A() < A(); }
            fun f2() -> Bool { return A() <= A(); }
            fun f3() -> Bool { return A() > A(); }
            fun f4() -> Bool { return A() >= A(); }");
    }

    #[test]
    fn long_operations() {
        ok("fun f(a: Long, b: Long) -> Long { return a + b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a - b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a * b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a / b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a % b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a | b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a & b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a ^ b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a << b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a >> b; }");
        ok("fun f(a: Long, b: Long) -> Long { return a >>> b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a == b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a != b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a === b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a !== b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a < b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a <= b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a > b; }");
        ok("fun f(a: Long, b: Long) -> Bool { return a >= b; }");
        ok("fun f(a: Long) -> Long { return !a; }");
        ok("fun f(a: Long) -> Long { return -a; }");
        ok("fun f(a: Long) -> Long { return +a; }");
    }

    #[test]
    fn test_literal_int_overflow() {
        err(
            "fun f() { let x = 2147483648; }",
            pos(1, 19),
            Msg::NumberOverflow("Int".into()),
        );
        ok("fun f() { let x = 2147483647; }");
        err(
            "fun f() { let x = -2147483649; }",
            pos(1, 20),
            Msg::NumberOverflow("Int".into()),
        );
        ok("fun f() { let x = -2147483648; }");
    }

    #[test]
    fn test_literal_hex_int_overflow() {
        err(
            "fun f() { let x = 0x1_FF_FF_FF_FF; }",
            pos(1, 19),
            Msg::NumberOverflow("Int".into()),
        );
        ok("fun f() { let x: Int = 0xFF_FF_FF_FF; }");
    }

    #[test]
    fn test_literal_bin_int_overflow() {
        err(
            "fun f() { let x = 0b1_11111111_11111111_11111111_11111111; }",
            pos(1, 19),
            Msg::NumberOverflow("Int".into()),
        );
        ok("fun f() { let x: Int = 0b11111111_11111111_11111111_11111111; }");
    }

    #[test]
    fn test_literal_long_overflow() {
        err(
            "fun f() { let x = 9223372036854775808L; }",
            pos(1, 19),
            Msg::NumberOverflow("Long".into()),
        );
        ok("fun f() { let x = 9223372036854775807L; }");
        err(
            "fun f() { let x = -9223372036854775809L; }",
            pos(1, 20),
            Msg::NumberOverflow("Long".into()),
        );
        ok("fun f() { let x = -9223372036854775808L; }");
    }

    #[test]
    fn test_literal_float_overflow() {
        err(
            "fun f() { let x = -340282350000000000000000000000000000000F; }",
            pos(1, 20),
            Msg::NumberOverflow("Float".into()),
        );
        ok("fun f() { let x = -340282340000000000000000000000000000000F; }");
        err(
            "fun f() { let x = 340282350000000000000000000000000000001F; }",
            pos(1, 19),
            Msg::NumberOverflow("Float".into()),
        );
        ok("fun f() { let x = 340282340000000000000000000000000000000F; }");
    }

    #[test]
    fn test_char() {
        ok("fun foo() -> Char { return 'c'; }");
        ok("fun foo(a: Char) -> Char { return a; }");
        err(
            "fun foo() -> Char { return false; }",
            pos(1, 21),
            Msg::ReturnType("Char".into(), "Bool".into()),
        );
        err(
            "fun foo() -> Char { return 10; }",
            pos(1, 21),
            Msg::ReturnType("Char".into(), "Int".into()),
        );
    }

    #[test]
    fn test_generic_arguments_mismatch() {
        err(
            "class A[T]
            fun foo() {
                let a = A[Int, Int]();
            }",
            pos(3, 36),
            Msg::WrongNumberTypeParams(1, 2),
        );

        err(
            "class A[T]
            fun foo() {
                let a = A();
            }",
            pos(3, 26),
            Msg::WrongNumberTypeParams(1, 0),
        );

        err(
            "class A
            fun foo() {
                let a = A[Int]();
            }",
            pos(3, 31),
            Msg::WrongNumberTypeParams(0, 1),
        );
    }

    #[test]
    fn test_invoke_static_method_as_instance_method() {
        err(
            "class A {
                @static fun foo() {}
                fun test() { self.foo(); }
            }",
            pos(3, 38),
            Msg::UnknownMethod("A".into(), "foo".into(), vec![]),
        );
    }

    #[test]
    fn test_invoke_method_as_static() {
        err(
            "class A {
                fun foo() {}
                @static fun test() { A::foo(); }
            }",
            pos(3, 44),
            Msg::UnknownStaticMethod("A".into(), "foo".into(), vec![]),
        );
    }

    #[test]
    fn test_fct_with_type_params() {
        err(
            "fun f() {} fun g() { f[Int](); }",
            pos(1, 28),
            Msg::WrongNumberTypeParams(0, 1),
        );
        err(
            "fun f[T]() {} fun g() { f(); }",
            pos(1, 26),
            Msg::WrongNumberTypeParams(1, 0),
        );
        ok("fun f[T]() {} fun g() { f[Int](); }");
        ok("fun f[T1, T2]() {} fun g() { f[Int, String](); }");
    }

    #[test]
    fn test_const_check() {
        err(
            "const one: Int = 1;
            fun f() -> Long { return one; }",
            pos(2, 31),
            Msg::ReturnType("Long".into(), "Int".into()),
        );

        err(
            "const one: Int = 1;
            fun f() { let x: String = one; }",
            pos(2, 23),
            Msg::AssignType("x".into(), "String".into(), "Int".into()),
        );
    }

    #[test]
    fn test_const() {
        ok_with_test(
            "  const yes: Bool = true;
                        const x: Byte = 255Y;
                        const a: Int = 100;
                        const b: Long = 200L;
                        const c: Char = 'A';
                        const d: Float = 3.0F;
                        const e: Double = 6.0;",
            |vm| {
                {
                    let xconst = vm.consts.idx_usize(0);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Bool(true), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(1);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Int(255), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(2);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Int(100), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(3);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Int(200), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(4);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Char('A'), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(5);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Float(3.0), xconst.value);
                }

                {
                    let xconst = vm.consts.idx_usize(6);
                    let xconst = xconst.lock();
                    assert_eq!(ConstValue::Float(6.0), xconst.value);
                }
            },
        );
    }

    #[test]
    fn test_assignment_to_const() {
        err(
            "const one: Int = 1;
            fun f() { one = 2; }",
            pos(2, 27),
            Msg::AssignmentToConst,
        );
    }

    #[test]
    fn test_unary_minus_byte() {
        err(
            "const m1: Byte = -1Y;",
            pos(1, 18),
            Msg::UnOpType("-".into(), "Byte".into()),
        );
        ok("const m1: Int = -1;");
        ok("const m1: Long = -1L;");
    }

    #[test]
    fn test_generic_class_bounds() {
        ok("class Foo
            class A[T: Foo]
            fun f() -> A[Foo] { return nil; }");

        ok("@open class Foo
            class Bar: Foo
            class A[T: Foo]
            fun f() -> A[Bar] { return nil; }");

        err(
            "class Foo
            class Bar
            class A[T: Foo]
            fun f() -> A[Bar] { return nil; }",
            pos(4, 24),
            Msg::ClassBoundNotSatisfied("Bar".into(), "Foo".into()),
        );

        err(
            "class Foo
            fun f[T: Foo]() {}
            fun t() { f[Int](); }",
            pos(3, 29),
            Msg::ClassBoundNotSatisfied("Int".into(), "Foo".into()),
        );
    }

    #[test]
    fn test_generic_trait_bounds() {
        ok("trait Foo {}
            class X
            impl Foo for X {}
            class A[T: Foo]
            fun f() -> A[X] { return nil; }");

        err(
            "trait Foo {}
            class X
            class A[T: Foo]
            fun f() -> A[X] { return nil; }",
            pos(1, 1),
            Msg::TraitBoundNotSatisfied("X".into(), "Foo".into()),
        );

        err(
            "trait Foo {}
            fun f[T: Foo]() {}
            fun t() { f[Int](); }",
            pos(3, 29),
            Msg::TraitBoundNotSatisfied("Int".into(), "Foo".into()),
        );
    }

    #[test]
    fn test_operator_on_generic_type() {
        err(
            "fun f[T](a: T, b: T) { a + b; }",
            pos(1, 26),
            Msg::BinOpType("+".into(), "T".into(), "T".into()),
        );
    }

    #[test]
    fn test_find_class_method_precedence() {
        // finding class method should have precedence over
        // trait methods
        ok("class A { fun foo() {} }
            trait Foo { fun foo(); }
            impl Foo for A { fun foo() {} }
            fun test(a: A) { a.foo(); }");

        err(
            "class A { fun foo() {} }
            trait Foo { fun foo(a: Int); }
            impl Foo for A { fun foo(a:  Int) {} }
            fun test(a: A) { a.foo(1); }",
            pos(4, 35),
            Msg::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int".into()]),
        );

        ok("class A { @static fun foo() {} }
            trait Foo { fun foo(a: Int); }
            impl Foo for A { fun foo(a:  Int) {} }
            fun test(a: A) { a.foo(1); }");
    }

    #[test]
    fn test_invoke_abstract_class_ctor() {
        err(
            "@abstract class A
            fun test() -> A { return A(); }",
            pos(2, 39),
            Msg::NewAbstractClass,
        );
    }

    #[test]
    fn test_global_get() {
        ok("var x: Int; fun foo() -> Int { return x; }");
    }

    #[test]
    fn test_global_set() {
        ok("var x: Int; fun foo(a: Int) { x = a; }");
        err(
            "let x: Int; fun foo(a: Int) { x = a; }",
            pos(1, 33),
            Msg::LetReassigned,
        );
    }

    #[test]
    fn lambda_assignment() {
        ok("fun f() { let x = || {}; }");
        ok("fun f() { let x = || -> Int { return 2; }; }");
        ok("fun f() { let x: () -> () = || {}; }");
        ok("fun f() { let x: () -> () = || -> () {}; }");
        ok("fun f() { let x: () -> Int = || -> Int { return 2; }; }");
        err(
            "fun f() { let x: () -> Int = || {}; }",
            pos(1, 11),
            Msg::AssignType("x".into(), "() -> Int".into(), "() -> ()".into()),
        );
    }

    #[test]
    fn method_call_with_multiple_matching_traits() {
        err(
            "class A
            trait X { fun f(); }
            trait Y { fun f(); }

            impl X for A { fun f() {} }
            impl Y for A { fun f() {} }

            fun g(a: A) { a.f(); }",
            pos(8, 30),
            Msg::MultipleCandidatesForMethod("A".into(), "f".into(), Vec::new()),
        );
    }

    #[test]
    fn generic_trait_method_call() {
        ok("trait Foo { fun bar(); }
            fun f[T: Foo](t: T) { t.bar(); }");
        ok("trait Foo { fun bar(); }
            class A[T: Foo](let t: T) {
                fun baz() { self.t.bar(); }
            }");

        err(
            "trait Foo { fun bar() throws; }
            fun f[T: Foo](t: T) { t.bar(); }",
            pos(2, 40),
            Msg::ThrowingCallWithoutTry,
        );
        err(
            "trait Foo { fun bar() throws; }
            class A[T: Foo](let t: T) {
                fun baz() { self.t.bar(); }
            }",
            pos(3, 39),
            Msg::ThrowingCallWithoutTry,
        );

        err(
            "trait Foo { fun bar(); }
            fun f[T: Foo](t: T) { try t.bar(); }",
            pos(2, 35),
            Msg::TryCallNonThrowing,
        );
        err(
            "trait Foo { fun bar(); }
            class A[T: Foo](let t: T) {
                fun baz() { try self.t.bar(); }
            }",
            pos(3, 29),
            Msg::TryCallNonThrowing,
        );
    }

    #[test]
    fn test_generic_ctor_without_type_params() {
        err(
            "class Foo[A, B]()
            fun test() { Foo(); }",
            pos(2, 29),
            Msg::WrongNumberTypeParams(2, 0),
        );
    }

    #[test]
    fn test_generic_argument_with_trait_bound() {
        err(
            "fun f[X: Comparable](x: X) {}
            fun g[T](t: T) { f[T](t); }",
            pos(2, 34),
            Msg::TraitBoundNotSatisfied("T".into(), "Comparable".into()),
        );
    }

    #[test]
    fn test_for_supports_make_iterator() {
        err(
            "fun f() { for i in 1 {} }",
            pos(1, 11),
            Msg::UnknownMethod("Int".into(), "makeIterator".into(), Vec::new()),
        );

        err(
            "
            class Foo() { fun makeIterator() -> Bool { return true; } }
            fun f() { for i in Foo() {} }",
            pos(3, 35),
            Msg::MakeIteratorReturnType("Bool".into()),
        );

        ok(
            "class Foo { fun makeIterator() -> FooIter { return FooIter(); } }
            class FooIter
            impl Iterator for FooIter {
                fun hasNext() -> Bool { return false; }
                fun next() -> Int { return 0; }
            }
            fun f() -> Int { for i in Foo() { return i; } return 0; }",
        );
    }

    #[test]
    fn test_ctor_with_type_param() {
        err(
            "
            class Foo[T] {
                fun foo(a: Int) {
                    Bar[T](a);
                }
            }

            class Bar[T](a: T)
            ",
            pos(4, 27),
            Msg::UnknownCtor("Bar".into(), vec!["Int".into()]),
        );
    }

    #[test]
    fn test_fct_used_as_identifier() {
        err(
            "fun foo() {} fun bar() { foo; }",
            pos(1, 26),
            Msg::FctUsedAsIdentifier,
        );
    }

    #[test]
    fn test_cls_used_as_identifier() {
        err(
            "class X fun f() { X; }",
            pos(1, 19),
            Msg::ClsUsedAsIdentifier,
        );
    }

    #[test]
    fn test_assign_fct() {
        err(
            "fun foo() {} fun bar() { foo = 1; }",
            pos(1, 30),
            Msg::FctReassigned,
        );
    }

    #[test]
    fn test_assign_class() {
        err(
            "class X fun foo() { X = 2; }",
            pos(1, 23),
            Msg::ClassReassigned,
        );
    }

    #[test]
    fn test_new_call_fct() {
        ok("fun g() {} @new_call fun f() { g(); }");
    }

    #[test]
    fn test_new_call_fct_throws() {
        ok("fun g() throws {} @new_call fun f() { try g(); }");
    }

    #[test]
    fn test_new_call_fct_without_try() {
        err(
            "fun g() throws {} @new_call fun f() { g(); }",
            pos(1, 40),
            Msg::ThrowingCallWithoutTry,
        );
    }

    #[test]
    fn test_new_call_fct_wrong_params() {
        err(
            "fun g() {} @new_call fun f() { g(1); }",
            pos(1, 33),
            Msg::ParamTypesIncompatible("g".into(), Vec::new(), vec!["Int".into()]),
        );
    }

    #[test]
    fn test_new_call_fct_with_type_params() {
        ok("fun g[T]() {} @new_call fun f() { g[Int](); }");
    }

    #[test]
    fn test_new_call_fct_with_wrong_type_params() {
        err(
            "fun g() {} @new_call fun f() { g[Int](); }",
            pos(1, 38),
            Msg::WrongNumberTypeParams(0, 1),
        );
    }

    #[test]
    fn test_new_call_static_method() {
        ok("class Foo { @static fun bar() {} }
            @new_call fun f() { Foo::bar(); }");
    }

    #[test]
    fn test_new_call_static_method_wrong_params() {
        err(
            "class Foo { @static fun bar() {} }
            @new_call fun f() { Foo::bar(1); }",
            pos(2, 41),
            Msg::ParamTypesIncompatible("bar".into(), Vec::new(), vec!["Int".into()]),
        );
    }

    #[test]
    fn test_new_call_static_method_type_params() {
        ok("class Foo { @static fun bar[T]() {} }
            @new_call fun f() { Foo::bar[Int](); }");
    }

    #[test]
    fn test_new_call_class() {
        ok("class X @new_call fun f() { X(); }");
    }

    #[test]
    fn test_new_call_class_wrong_params() {
        err(
            "class X @new_call fun f() { X(1); }",
            pos(1, 30),
            Msg::UnknownCtor("X".into(), vec!["Int".into()]),
        );
    }

    #[test]
    fn test_new_call_class_with_type_params() {
        ok("class X[T] @new_call fun f() { X[Int](); }");
    }

    #[test]
    fn test_new_call_class_with_wrong_type_params() {
        err(
            "class X @new_call fun f() { X[Int](); }",
            pos(1, 35),
            Msg::WrongNumberTypeParams(0, 1),
        );
    }

    #[test]
    fn test_new_call_method() {
        ok("class X { fun f() {} }
            @new_call fun f(x: X) { x.f(); }");
    }

    #[test]
    fn test_new_call_method_type_param() {
        ok("class X { fun f[T]() {} }
            @new_call fun f(x: X) { x.f[Int](); }");
    }

    #[test]
    fn test_new_call_method_wrong_params() {
        err(
            "class X { fun f() {} } @new_call fun f(x: X) { x.f(1); }",
            pos(1, 51),
            Msg::ParamTypesIncompatible("f".into(), Vec::new(), vec!["Int".into()]),
        );
    }

    #[test]
    fn test_new_call_method_without_try() {
        err(
            "class X { fun f() throws {} } @new_call fun f(x: X) { x.f(); }",
            pos(1, 58),
            Msg::ThrowingCallWithoutTry,
        );
    }

    #[test]
    fn test_new_call_method_generic() {
        ok("@new_call fun f[T: Hash](t: T) { t.hash(); }");
    }

    #[test]
    fn test_new_call_method_generic_error() {
        err(
            "@new_call fun f[T](t: T) { t.hash(); }",
            pos(1, 34),
            Msg::UnknownMethodForTypeParam("T".into(), "hash".into(), Vec::new()),
        );
    }

    #[test]
    fn test_new_call_method_generic_error_multiple() {
        err(
            "
            trait TraitA { fun id(); }
            trait TraitB { fun id(); }
            @new_call fun f[T: TraitA + TraitB](t: T) { t.id(); }",
            pos(4, 61),
            Msg::MultipleCandidatesForTypeParam("T".into(), "id".into(), Vec::new()),
        );
    }

    #[test]
    fn test_array_syntax_get() {
        ok("@new_call fun f(t: Array[Int]) -> Int { return t(0); }");
    }

    #[test]
    fn test_array_syntax_set() {
        ok("@new_call fun f(t: Array[Int]){ t(0) = 10; }");
    }

    #[test]
    fn test_array_syntax_set_wrong_value() {
        err(
            "@new_call fun f(t: Array[Int]){ t(0) = true; }",
            pos(1, 38),
            Msg::UnknownMethod(
                "Array[Int]".into(),
                "set".into(),
                vec!["Int".into(), "Bool".into()],
            ),
        );
    }

    #[test]
    fn test_array_syntax_set_wrong_index() {
        err(
            "@new_call fun f(t: Array[Int]){ t(\"bla\") = 9; }",
            pos(1, 42),
            Msg::UnknownMethod(
                "Array[Int]".into(),
                "set".into(),
                vec!["String".into(), "Int".into()],
            ),
        );
    }
}
