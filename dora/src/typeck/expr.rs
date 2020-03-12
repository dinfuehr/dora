use std::collections::HashSet;
use std::convert::TryFrom;
use std::sync::Arc;
use std::{f32, f64};

use crate::error::msg::SemError;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck;
use crate::semck::{always_returns, expr_always_returns};
use crate::sym::Sym::SymClass;
use crate::ty::{BuiltinType, TypeList, TypeParamId};
use crate::typeck::lookup::MethodLookup;
use crate::vm::{
    self, find_field_in_class, find_methods_in_class, CallType, ClassId, ConvInfo, Fct, FctId,
    FctParent, FctSrc, FileId, ForTypeInfo, IdentType, Intrinsic, VM,
};

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
    pub file: FileId,
    pub src: &'a mut FctSrc,
    pub ast: &'ast Function,
    pub expr_type: BuiltinType,
    pub negative_expr_id: NodeId,
    pub used_in_call: HashSet<NodeId>,
}

impl<'a, 'ast> TypeCheck<'a, 'ast> {
    pub fn check(&mut self) {
        let block = self.ast.block.as_ref().expect("missing block");
        let mut returns = false;

        for stmt in &block.stmts {
            self.visit_stmt(stmt);

            if always_returns(stmt) {
                returns = true;
            }
        }

        let return_type = if let Some(ref value) = &block.expr {
            if expr_always_returns(value) {
                returns = true;
            }

            self.visit_expr(value);
            self.expr_type
        } else {
            BuiltinType::Unit
        };

        if !returns {
            self.check_fct_return_type(block.pos, return_type);
        }
    }

    pub fn check_stmt_var(&mut self, s: &'ast StmtVarType) {
        let var = *self.src.map_vars.get(s.id).unwrap();

        let expr_type = s.expr.as_ref().map(|expr| {
            self.visit_expr(&expr);
            self.expr_type
        });

        let defined_type = if let Some(ref data_type) = s.data_type {
            let ty = self.src.ty(data_type.id());
            Some(ty)
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
                    .report(self.file, s.pos, SemError::VarNeedsTypeInfo(tyname));

                return;
            }
        };

        // update type of variable, necessary when variable is only initialized with
        // an expression
        self.src.vars[var].ty = defined_type;

        if let Some(expr_type) = expr_type {
            if !expr_type.is_error()
                && !defined_type.is_error()
                && !defined_type.allows(self.vm, expr_type)
            {
                let name = self.vm.interner.str(s.name).to_string();
                let defined_type = defined_type.name(self.vm);
                let expr_type = expr_type.name(self.vm);
                let msg = SemError::AssignType(name, defined_type, expr_type);
                self.vm.diag.lock().report(self.file, s.pos, msg);
            }

        // let variable binding needs to be assigned
        } else if !s.reassignable {
            self.vm
                .diag
                .lock()
                .report(self.file, s.pos, SemError::LetMissingInitialization);
        }
    }

    fn check_stmt_for(&mut self, s: &'ast StmtForType) {
        self.visit_expr(&s.expr);
        let object_type = self.expr_type;

        let name = self.vm.interner.intern("makeIterator");

        let mut lookup = MethodLookup::new(self.vm, self.file)
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
                    .find_method_with_replace(self.vm, false, next_name, None, &[])
                    .expect("next() not found");
                let has_next_id = trai
                    .find_method_with_replace(self.vm, false, has_next_name, None, &[])
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
                let msg = SemError::MakeIteratorReturnType(ret);
                self.vm.diag.lock().report(self.file, s.expr.pos(), msg);

                // set invalid error type
                let var_id = *self.src.map_vars.get(s.id).unwrap();
                self.src.vars[var_id].ty = BuiltinType::Error;
            }
        } else {
            // set invalid error type
            let var_id = *self.src.map_vars.get(s.id).unwrap();
            self.src.vars[var_id].ty = BuiltinType::Error;
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_while(&mut self, s: &'ast StmtWhileType) {
        self.visit_expr(&s.cond);

        if !self.expr_type.is_error() && !self.expr_type.is_bool() {
            let expr_type = self.expr_type.name(self.vm);
            let msg = SemError::WhileCondType(expr_type);
            self.vm.diag.lock().report(self.file, s.pos, msg);
        }

        self.visit_stmt(&s.block);
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

        self.check_fct_return_type(s.pos, expr_type);
    }

    fn check_fct_return_type(&mut self, pos: Position, expr_type: BuiltinType) {
        let fct_type = self.fct.return_type;

        if !expr_type.is_error() && !fct_type.allows(self.vm, expr_type) {
            let msg = if expr_type.is_nil() {
                let fct_type = fct_type.name(self.vm);

                SemError::IncompatibleWithNil(fct_type)
            } else {
                let fct_type = fct_type.name(self.vm);
                let expr_type = expr_type.name(self.vm);

                SemError::ReturnType(fct_type, expr_type)
            };

            self.vm.diag.lock().report(self.file, pos, msg);
        }
    }

    fn check_stmt_defer(&mut self, s: &'ast StmtDeferType) {
        self.visit_expr(&s.expr);

        if !s.expr.is_call() {
            self.vm
                .diag
                .lock()
                .report(self.file, s.pos, SemError::FctCallExpected);
        }
    }

    fn check_expr_block(&mut self, block: &'ast ExprBlockType) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let ty = if let Some(ref expr) = block.expr {
            self.visit_expr(expr);
            self.expr_type
        } else {
            BuiltinType::Unit
        };

        self.src.set_ty(block.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_tuple(&mut self, tuple: &'ast ExprTupleType) {
        let mut subtypes = Vec::new();

        if tuple.values.is_empty() {
            self.src.set_ty(tuple.id, BuiltinType::Unit);
            self.expr_type = BuiltinType::Unit;
            return;
        }

        for value in &tuple.values {
            self.visit_expr(value);
            subtypes.push(self.expr_type);
        }

        let tuple_id = self.vm.tuples.lock().insert(self.vm, subtypes);

        let ty = BuiltinType::Tuple(tuple_id);
        self.src.set_ty(tuple.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_if(&mut self, expr: &'ast ExprIfType) {
        self.visit_expr(&expr.cond);

        if !self.expr_type.is_bool() && !self.expr_type.is_error() {
            let expr_type = self.expr_type.name(self.vm);
            let msg = SemError::IfCondType(expr_type);
            self.vm.diag.lock().report(self.file, expr.pos, msg);
        }

        self.visit_expr(&expr.then_block);
        let then_type = self.expr_type;

        let merged_type = if let Some(ref else_block) = expr.else_block {
            self.visit_expr(else_block);
            let else_type = self.expr_type;

            if expr_always_returns(&expr.then_block) {
                else_type
            } else if expr_always_returns(else_block) {
                then_type
            } else if then_type.is_error() {
                else_type
            } else if else_type.is_error() {
                then_type
            } else if !then_type.allows(self.vm, else_type) {
                let then_type_name = then_type.name(self.vm);
                let else_type_name = else_type.name(self.vm);
                let msg = SemError::IfBranchTypesIncompatible(then_type_name, else_type_name);
                self.vm.diag.lock().report(self.file, expr.pos, msg);
                then_type
            } else {
                then_type
            }
        } else {
            BuiltinType::Unit
        };

        self.src.set_ty(expr.id, merged_type);
        self.expr_type = merged_type;
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
                let list_id = self.vm.lists.lock().insert(TypeList::empty());
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
                        .report(self.file, e.pos, SemError::FctUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::Class(_) => {
                if !self.used_in_call.contains(&e.id) {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file, e.pos, SemError::ClsUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::Module(module_id) => {
                let module = self.vm.modules.idx(module_id);
                let ty = module.read().ty;
                self.src.set_ty(e.id, ty);
                self.expr_type = ty;
            }

            &IdentType::TypeParam(_) => {
                let msg = if self.used_in_call.contains(&e.id) {
                    SemError::TypeParamUsedAsCallee
                } else {
                    SemError::TypeParamUsedAsIdentifier
                };

                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::Enum(_) => {
                let msg = SemError::EnumUsedAsIdentifier;
                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            &IdentType::EnumValue(_, _) => unreachable!(),
            &IdentType::FctType(_, _) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn check_expr_assign(&mut self, e: &'ast ExprBinType) {
        if e.lhs.is_call() {
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
                                .report(self.file, e.pos, SemError::LetReassigned);
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
                                .report(self.file, e.pos, SemError::LetReassigned);
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
                            .report(self.file, e.pos, SemError::AssignmentToConst);

                        return;
                    }

                    &IdentType::Fct(_) | &IdentType::FctType(_, _) => {
                        self.vm
                            .diag
                            .lock()
                            .report(self.file, e.pos, SemError::FctReassigned);

                        return;
                    }

                    &IdentType::Class(_) | &IdentType::ClassType(_, _) => {
                        self.vm
                            .diag
                            .lock()
                            .report(self.file, e.pos, SemError::ClassReassigned);

                        return;
                    }

                    &IdentType::Module(_) => unreachable!(),

                    &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => {
                        self.vm
                            .diag
                            .lock()
                            .report(self.file, e.pos, SemError::TypeParamReassigned);

                        return;
                    }

                    &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => {
                        self.vm.diag.lock().report(
                            self.file,
                            e.pos,
                            SemError::InvalidLhsAssignment,
                        );

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

                    let msg = SemError::AssignType(name, lhs_type, rhs_type);
                    self.vm.diag.lock().report(self.file, e.pos, msg);
                }

                return;
            }
        } else {
            self.vm
                .diag
                .lock()
                .report(self.file, e.pos, SemError::LvalueExpected);
        }

        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_assign_call(&mut self, e: &'ast ExprBinType) {
        let call = e.lhs.to_call().unwrap();

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

        if let Some((_, fct_id, _)) = self.find_method(
            e.pos,
            expr_type,
            false,
            name,
            &arg_types,
            &TypeList::empty(),
        ) {
            let call_type = CallType::Expr(expr_type, fct_id);
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
        }
    }

    fn check_expr_assign_field(&mut self, e: &'ast ExprBinType) {
        let field_expr = e.lhs.to_dot().unwrap();

        let name = match field_expr.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = SemError::NameExpected;
                self.vm.diag.lock().report(self.file, e.pos, msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
                return;
            }
        };

        self.visit_expr(&field_expr.lhs);
        let object_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id)) = find_field_in_class(self.vm, object_type, name) {
                let ident_type = IdentType::Field(cls_ty, field_id);
                self.src
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let cls = self
                    .vm
                    .classes
                    .idx(cls_ty.cls_id(self.vm).expect("no class"));
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let class_type_params = cls_ty.type_params(self.vm);

                let fty = replace_type_param(
                    self.vm,
                    field.ty,
                    &class_type_params,
                    &TypeList::empty(),
                    None,
                );

                if !self.fct.is_constructor && !field.reassignable {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file, e.pos, SemError::LetReassigned);
                }

                if !fty.allows(self.vm, rhs_type) && !rhs_type.is_error() {
                    let name = self.vm.interner.str(name).to_string();

                    let object_type = object_type.name(self.vm);
                    let lhs_type = fty.name(self.vm);
                    let rhs_type = rhs_type.name(self.vm);

                    let msg = SemError::AssignField(name, object_type, lhs_type, rhs_type);
                    self.vm.diag.lock().report(self.file, e.pos, msg);
                }

                self.src.set_ty(e.id, BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
                return;
            }
        }

        // field not found, report error
        let field_name = self.vm.interner.str(name).to_string();
        let expr_name = object_type.name(self.vm);
        let msg = SemError::UnknownField(field_name, expr_name);
        self.vm.diag.lock().report(self.file, field_expr.pos, msg);

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
        fct_type_params: &TypeList,
    ) -> Option<(ClassId, FctId, BuiltinType)> {
        let result = lookup_method(
            self.vm,
            object_type,
            is_static,
            name,
            args,
            fct_type_params,
            None,
        );

        if result.is_none() {
            let type_name = object_type.name(self.vm);
            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<String>>();
            let msg = if is_static {
                SemError::UnknownStaticMethod(type_name, name, param_names)
            } else {
                SemError::UnknownMethod(type_name, name, param_names)
            };

            self.vm.diag.lock().report(self.file, pos, msg);
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
                &TypeList::empty(),
                None,
            ) {
                let call_type = CallType::Method(ty, fct_id, TypeList::empty());
                self.src.map_calls.insert(e.id, Arc::new(call_type));

                self.src.set_ty(e.id, return_type);
                self.expr_type = return_type;
                return;
            }

            let ty = ty.name(self.vm);
            let msg = SemError::UnOpType(op.as_str().into(), ty);

            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
    }

    fn check_expr_bin(&mut self, e: &'ast ExprBinType) {
        if e.op.is_any_assign() {
            self.check_expr_assign(e);
            return;
        }

        self.visit_expr(&e.lhs);
        let lhs_type = self.expr_type;

        self.visit_expr(&e.rhs);
        let rhs_type = self.expr_type;

        if lhs_type.is_error() || rhs_type.is_error() {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

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
                self.check_expr_bin_method(e, e.op, "shiftRightSigned", lhs_type, rhs_type)
            }
            BinOp::LogicalShiftR => {
                self.check_expr_bin_method(e, e.op, "shiftRight", lhs_type, rhs_type)
            }
            BinOp::Assign => unreachable!(),
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
            &TypeList::empty(),
            None,
        ) {
            let call_type = CallType::Method(lhs_type, fct_id, TypeList::empty());
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = SemError::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file, e.pos, msg);

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
                    self.vm.diag.lock().report(
                        self.file,
                        e.pos,
                        SemError::TypesIncompatible(lhs_type, rhs_type),
                    );
                }

                self.src.set_ty(e.id, BuiltinType::Bool);
                self.expr_type = BuiltinType::Bool;
                return;
            }

            CmpOp::Eq | CmpOp::Ne => {
                if lhs_type.is_enum() {
                    self.check_expr_cmp_enum(e, cmp, lhs_type, rhs_type)
                } else {
                    self.check_expr_bin_method(e, e.op, "equals", lhs_type, rhs_type)
                }
            }

            _ => self.check_expr_bin_method(e, e.op, "compareTo", lhs_type, rhs_type),
        }

        self.src.set_ty(e.id, BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_expr_cmp_enum(
        &mut self,
        e: &'ast ExprBinType,
        op: CmpOp,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) {
        if lhs_type.allows(self.vm, rhs_type) {
            let intrinsic = match op {
                CmpOp::Eq => Intrinsic::EnumEq,
                CmpOp::Ne => Intrinsic::EnumNe,
                _ => unreachable!(),
            };
            let call_type = CallType::Intrinsic(intrinsic);
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.src.set_ty(e.id, BuiltinType::Bool);
            self.expr_type = BuiltinType::Bool;
        } else {
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = SemError::BinOpType("equals".into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
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
            let msg = SemError::BinOpType(op, lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file, e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &'ast ExprCallType) {
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
                self.check_expr_call_ident(e, fct_id, TypeList::empty(), &arg_types);
            }

            Some(IdentType::FctType(fct_id, type_params)) => {
                self.check_expr_call_ident(e, fct_id, type_params, &arg_types);
            }

            Some(IdentType::Class(cls_id)) => {
                self.check_expr_call_ctor(e, cls_id, TypeList::empty(), &arg_types);
            }

            Some(IdentType::ClassType(cls_id, type_params)) => {
                self.check_expr_call_ctor(e, cls_id, type_params, &arg_types);
            }

            Some(IdentType::Method(object_type, method_name)) => {
                self.check_expr_call_method(
                    e,
                    object_type,
                    method_name,
                    TypeList::empty(),
                    &arg_types,
                );
            }

            Some(IdentType::MethodType(object_type, method_name, type_params)) => {
                self.check_expr_call_method(e, object_type, method_name, type_params, &arg_types);
            }

            Some(IdentType::StaticMethod(object_type, method_name)) => self
                .check_expr_call_static_method(
                    e,
                    object_type,
                    method_name,
                    TypeList::empty(),
                    &arg_types,
                ),

            Some(IdentType::StaticMethodType(object_type, method_name, type_params)) => self
                .check_expr_call_static_method(
                    e,
                    object_type,
                    method_name,
                    type_params,
                    &arg_types,
                ),

            Some(IdentType::TypeParamStaticMethod(ty, name)) => {
                self.check_expr_call_generic_static_method(e, ty, name, &arg_types)
            }

            Some(IdentType::TypeParam(_)) => {
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            Some(IdentType::Enum(_)) => {
                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
            }

            _ => {
                if expr_type.is_error() {
                    self.src.set_ty(e.id, expr_type);
                    self.expr_type = expr_type;
                    return;
                }

                self.check_expr_call_expr(e, expr_type, &arg_types);
            }
        }
    }

    fn check_expr_call_generic_static_method(
        &mut self,
        e: &'ast ExprCallType,
        tp: BuiltinType,
        name: Name,
        arg_types: &[BuiltinType],
    ) {
        let mut fcts = Vec::new();

        let (type_param, tp_id) = match tp {
            BuiltinType::FctTypeParam(fct_id, tp_id) => {
                assert_eq!(self.fct.id, fct_id);
                (self.fct.type_params[tp_id.idx()].clone(), tp_id)
            }

            BuiltinType::ClassTypeParam(cls_id, tp_id) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                (cls.type_params[tp_id.idx()].clone(), tp_id)
            }

            _ => unreachable!(),
        };

        for &trait_id in &type_param.trait_bounds {
            let xtrait = self.vm.traits[trait_id].read();

            if let Some(fct_id) = xtrait.find_method(self.vm, name, true) {
                fcts.push((trait_id, fct_id));
            }
        }

        if fcts.len() != 1 {
            let msg = if fcts.len() > 1 {
                SemError::MultipleCandidatesForStaticMethodWithTypeParam
            } else {
                SemError::UnknownStaticMethodWithTypeParam
            };

            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        if arg_types.contains(&BuiltinType::Error) {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        let (trait_id, fct_id) = fcts[0];
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        if !args_compatible(
            self.vm,
            fct.params_without_self(),
            arg_types,
            None,
            Some(fct_id),
            &TypeList::empty(),
            &TypeList::empty(),
            Some(tp),
        ) {
            let fct_name = self.vm.interner.str(name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, arg_types);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        let call_type = CallType::TraitStatic(TypeParamId::Fct(tp_id), trait_id, fct_id);
        self.src.map_calls.insert(e.id, Arc::new(call_type));

        let return_type = replace_type_param(
            self.vm,
            fct.return_type,
            &TypeList::empty(),
            &TypeList::empty(),
            Some(tp),
        );

        self.src.set_ty(e.id, return_type);
        self.expr_type = return_type;
    }

    fn check_expr_call_expr(
        &mut self,
        e: &'ast ExprCallType,
        expr_type: BuiltinType,
        arg_types: &[BuiltinType],
    ) {
        let get = self.vm.interner.intern("get");

        if let Some((_, fct_id, return_type)) =
            self.find_method(e.pos, expr_type, false, get, arg_types, &TypeList::empty())
        {
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
        e: &'ast ExprCallType,
        fct_id: FctId,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) {
        let mut lookup = MethodLookup::new(self.vm, self.file)
            .pos(e.pos)
            .callee(fct_id)
            .args(&arg_types)
            .fct_type_params(&type_params);

        let ty = if lookup.find() {
            let call_type = CallType::Fct(fct_id, TypeList::empty(), type_params.clone());
            self.src.map_calls.insert(e.id, Arc::new(call_type));

            lookup.found_ret().unwrap()
        } else {
            BuiltinType::Error
        };

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_call_static_method(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls_type_params = object_type.type_params(self.vm);
        assert_eq!(cls_type_params.len(), 0);

        let mut lookup = MethodLookup::new(self.vm, self.file)
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
                TypeList::empty(),
                type_params.clone(),
            ));
            self.src.map_calls.insert(e.id, call_type.clone());

            self.src.set_ty(e.id, return_type);
            self.expr_type = return_type;
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_method(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) {
        if object_type.is_type_param() {
            assert_eq!(type_params.len(), 0);
            self.check_expr_call_generic(e, object_type, method_name, arg_types);
            return;
        }

        if object_type.is_error() {
            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;

            return;
        }

        let mut lookup = MethodLookup::new(self.vm, self.file)
            .method(object_type)
            .pos(e.pos)
            .name(method_name)
            .fct_type_params(&type_params)
            .args(arg_types);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();

            let call_type = if let BuiltinType::Trait(trait_id) = object_type {
                CallType::Trait(trait_id, fct_id)
            } else {
                let method_type = lookup.found_class_type().unwrap();
                CallType::Method(method_type, fct_id, type_params.clone())
            };

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

    fn check_expr_call_ctor(
        &mut self,
        e: &'ast ExprCallType,
        cls_id: ClassId,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) {
        let mut lookup = MethodLookup::new(self.vm, self.file)
            .pos(e.pos)
            .ctor(cls_id)
            .args(arg_types)
            .cls_type_params(&type_params);

        let ty = if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            let cls_ty = self.vm.cls_with_type_list(cls_id, type_params.clone());
            let call_type = CallType::CtorNew(cls_ty, fct_id);
            self.src.map_calls.insert(e.id, Arc::new(call_type));

            if cls.is_abstract {
                let msg = SemError::NewAbstractClass;
                self.vm.diag.lock().report(self.file, e.pos, msg);
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
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        name: Name,
        arg_types: &[BuiltinType],
    ) {
        match object_type {
            BuiltinType::FctTypeParam(_, tpid) => {
                let tp = &self.fct.type_params[tpid.idx()];
                self.check_expr_call_generic_type_param(e, object_type, tp, name, arg_types);
            }

            BuiltinType::ClassTypeParam(cls_id, tpid) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let tp = &cls.type_params[tpid.idx()];
                self.check_expr_call_generic_type_param(e, object_type, tp, name, arg_types);
            }

            _ => unreachable!(),
        }
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        tp: &vm::TypeParam,
        name: Name,
        args: &[BuiltinType],
    ) {
        let mut found_fcts = Vec::new();

        for &trait_id in &tp.trait_bounds {
            let trai = self.vm.traits[trait_id].read();

            if let Some(fid) = trai.find_method_with_replace(self.vm, false, name, None, args) {
                found_fcts.push(fid);
            }
        }

        if found_fcts.len() == 1 {
            let fid = found_fcts[0];
            let call_type = CallType::Method(object_type, fid, TypeList::empty());
            self.src.map_calls.insert(e.id, Arc::new(call_type));

            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();
            let return_type = fct.return_type;

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
                SemError::UnknownMethodForTypeParam(type_name, name, param_names)
            } else {
                SemError::MultipleCandidatesForTypeParam(type_name, name, param_names)
            };

            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
        }
    }

    fn check_expr_call_path(
        &mut self,
        e: &'ast ExprCallType,
        type_params: TypeList,
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
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm.diag.lock().report(self.file, class_expr.pos(), msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        if let Some(method_name_expr) = method_name_expr.to_ident() {
            method_name = method_name_expr.name;
        } else {
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm
                .diag
                .lock()
                .report(self.file, method_name_expr.pos(), msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        match self.vm.sym.lock().get(class) {
            Some(SymClass(cls_id)) => {
                let mut lookup = MethodLookup::new(self.vm, self.file)
                    .pos(e.pos)
                    .static_method(cls_id)
                    .name(method_name)
                    .args(arg_types)
                    .fct_type_params(&type_params);

                if lookup.find() {
                    let fct_id = lookup.found_fct_id().unwrap();
                    let call_type = Arc::new(CallType::Fct(
                        fct_id,
                        TypeList::empty(),
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
        let msg = SemError::ClassExpected(name);
        self.vm.diag.lock().report(self.file, e.pos, msg);

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

        let parent_class = owner.parent_class.unwrap();
        let cls_id = parent_class.cls_id(self.vm).expect("no class");
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        if let Some(ctor_id) = cls.constructor {
            let ctor = self.vm.fcts.idx(ctor_id);
            let ctor = ctor.read();

            let parent_class_type_params = parent_class.type_params(self.vm);

            if args_compatible(
                self.vm,
                &ctor.params_without_self(),
                &arg_types,
                Some(cls_id),
                None,
                &parent_class_type_params,
                &TypeList::empty(),
                None,
            ) {
                self.src.map_tys.insert(e.id, parent_class);

                let cls_ty = self.vm.cls_with_type_list(cls_id, parent_class_type_params);
                let call_type = CallType::Ctor(cls_ty, ctor.id);
                self.src.map_calls.insert(e.id, Arc::new(call_type));
                return;
            }
        }

        let name = self.vm.interner.str(cls.name).to_string();
        let arg_types = arg_types.iter().map(|t| t.name(self.vm)).collect();
        let msg = SemError::UnknownCtor(name, arg_types);
        self.vm.diag.lock().report(self.file, e.pos, msg);
    }

    fn super_type(&self, pos: Position) -> BuiltinType {
        if let FctParent::Class(clsid) = self.fct.parent {
            let cls = self.vm.classes.idx(clsid);
            let cls = cls.read();

            if let Some(parent_class) = cls.parent_class {
                return parent_class;
            }
        }

        let msg = SemError::SuperUnavailable;
        self.vm.diag.lock().report(self.file, pos, msg);

        BuiltinType::Error
    }

    fn check_expr_path(&mut self, e: &'ast ExprPathType) {
        let ident_type = self.src.map_idents.get(e.lhs.id());

        let name = if let Some(ident) = e.rhs.to_ident() {
            ident.name
        } else {
            let msg = SemError::NameOfStaticMethodExpected;
            self.vm.diag.lock().report(self.file, e.rhs.pos(), msg);
            return;
        };

        let ident_type = match ident_type {
            Some(&IdentType::Class(cls_id)) => {
                let list = self.vm.lists.lock().insert(TypeList::empty());
                let cls_ty = BuiltinType::Class(cls_id, list);

                IdentType::StaticMethod(cls_ty, name)
            }

            Some(&IdentType::Module(module_id)) => {
                let module_ty = BuiltinType::Module(module_id);

                IdentType::Method(module_ty, name)
            }

            Some(&IdentType::ClassType(cls_id, ref type_params)) => {
                let list = self.vm.lists.lock().insert(type_params.clone());
                let cls_ty = BuiltinType::Class(cls_id, list);

                IdentType::StaticMethod(cls_ty, name)
            }

            Some(&IdentType::TypeParam(ty)) => IdentType::TypeParamStaticMethod(ty, name),

            Some(&IdentType::Enum(id)) => {
                let xenum = self.vm.enums[id].read();

                if let Some(&value) = xenum.name_to_value.get(&name) {
                    self.src
                        .map_idents
                        .insert(e.id, IdentType::EnumValue(id, value));
                } else {
                    let name = self.vm.interner.str(name).to_string();
                    self.vm
                        .diag
                        .lock()
                        .report(self.file, e.pos, SemError::UnknownEnumValue(name));
                }

                self.src.set_ty(e.id, BuiltinType::Enum(id));
                self.expr_type = BuiltinType::Enum(id);
                return;
            }

            _ => {
                let msg = SemError::InvalidLeftSideOfSeparator;
                self.vm.diag.lock().report(self.file, e.lhs.pos(), msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
                return;
            }
        };

        if self.used_in_call.contains(&e.id) {
            self.src.map_idents.insert(e.id, ident_type);
            return;
        }

        self.vm
            .diag
            .lock()
            .report(self.file, e.pos, SemError::FctUsedAsIdentifier);
    }

    fn check_expr_type_param(&mut self, e: &'ast ExprTypeParamType) {
        if self.used_in_call.contains(&e.id) {
            self.used_in_call.insert(e.callee.id());
        }

        self.visit_expr(&e.callee);
        let ident_type = self.src.map_idents.get(e.callee.id()).cloned();

        let type_params: Vec<BuiltinType> = e.args.iter().map(|p| self.src.ty(p.id())).collect();
        let type_params: TypeList = TypeList::with(type_params);

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
                let msg = SemError::InvalidUseOfTypeParams;
                self.vm.diag.lock().report(self.file, e.pos, msg);
                return;
            }
        }
    }

    fn check_expr_dot(&mut self, e: &'ast ExprDotType) {
        let object_type = if e.lhs.is_super() {
            self.super_type(e.lhs.pos())
        } else {
            self.visit_expr(&e.lhs);
            self.expr_type
        };

        if object_type.is_tuple() {
            self.check_expr_dot_tuple(e, object_type);
            return;
        }

        let name = match e.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = SemError::NameExpected;
                self.vm.diag.lock().report(self.file, e.pos, msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
                return;
            }
        };

        if self.used_in_call.contains(&e.id) {
            self.src
                .map_idents
                .insert(e.id, IdentType::Method(object_type, name));
            return;
        }

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id)) = find_field_in_class(self.vm, object_type, name) {
                let ident_type = IdentType::Field(cls_ty, field_id);
                self.src.map_idents.insert_or_replace(e.id, ident_type);

                let cls = self
                    .vm
                    .classes
                    .idx(cls_ty.cls_id(self.vm).expect("no class"));
                let cls = cls.read();

                let field = &cls.fields[field_id];
                let class_type_params = cls_ty.type_params(self.vm);
                let fty = replace_type_param(
                    self.vm,
                    field.ty,
                    &class_type_params,
                    &TypeList::empty(),
                    None,
                );

                self.src.set_ty(e.id, fty);
                self.expr_type = fty;
                return;
            }
        }

        // field not found, report error
        if !object_type.is_error() {
            let field_name = self.vm.interner.str(name).to_string();
            let expr_name = object_type.name(self.vm);
            let msg = SemError::UnknownField(field_name, expr_name);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        self.src.set_ty(e.id, BuiltinType::Error);
        self.expr_type = BuiltinType::Error;
    }

    fn check_expr_dot_tuple(&mut self, e: &'ast ExprDotType, object_type: BuiltinType) {
        let index = match e.rhs.to_lit_int() {
            Some(ident) => ident.value,

            None => {
                let msg = SemError::IndexExpected;
                self.vm.diag.lock().report(self.file, e.pos, msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                self.expr_type = BuiltinType::Error;
                return;
            }
        };

        let tuple_id = match object_type {
            BuiltinType::Tuple(tuple_id) => tuple_id,
            _ => unreachable!(),
        };

        let tuple = self.vm.tuples.lock().get(tuple_id);

        if index >= tuple.len() as u64 {
            let msg = SemError::IllegalTupleIndex(index, object_type.name(self.vm));
            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
            self.expr_type = BuiltinType::Error;
            return;
        }

        let ty = tuple[usize::try_from(index).unwrap()];
        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
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
                let msg = SemError::ThisUnavailable;
                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Unit);
                self.expr_type = BuiltinType::Unit;
            }
        }
    }

    fn check_expr_super(&mut self, e: &'ast ExprSuperType) {
        let msg = SemError::SuperNeedsMethodCall;
        self.vm.diag.lock().report(self.file, e.pos, msg);
        self.src.set_ty(e.id, BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }

    fn check_expr_nil(&mut self, e: &'ast ExprNilType) {
        self.src.set_ty(e.id, BuiltinType::Nil);
        self.expr_type = BuiltinType::Nil;
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
        self.src.set_ty(e.object.id(), object_type);

        let check_type = self.src.ty(e.data_type.id());

        if !check_type.is_cls() {
            let name = check_type.name(self.vm);
            self.vm
                .diag
                .lock()
                .report(self.file, e.pos, SemError::ReferenceTypeExpected(name));
            let ty = if e.is {
                BuiltinType::Bool
            } else {
                BuiltinType::Error
            };
            self.src.set_ty(e.id, ty);
            self.expr_type = ty;
            return;
        }

        if !typeparamck::check_type(self.vm, self.file, e.data_type.pos(), check_type) {
            let ty = if e.is {
                BuiltinType::Bool
            } else {
                BuiltinType::Error
            };
            self.src.set_ty(e.id, ty);
            self.expr_type = ty;
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
            let msg = SemError::TypesIncompatible(object_type, check_type);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        self.src.map_convs.insert(
            e.id,
            ConvInfo {
                check_type: check_type,
                valid,
            },
        );

        let ty = if e.is { BuiltinType::Bool } else { check_type };
        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_int(&mut self, e: &'ast ExprLitIntType) {
        let (ty, _) = check_lit_int(self.vm, self.file, e, self.negative_expr_id);

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_float(&mut self, e: &'ast ExprLitFloatType) {
        let (ty, _) = check_lit_float(self.vm, self.file, e, self.negative_expr_id);

        self.src.set_ty(e.id, ty);
        self.expr_type = ty;
    }

    fn check_expr_lit_str(&mut self, e: &'ast ExprLitStrType) {
        let str_ty = self.vm.cls(self.vm.vips.string_class);
        self.src.set_ty(e.id, str_ty);
        self.expr_type = str_ty;
    }

    fn check_expr_lit_bool(&mut self, e: &'ast ExprLitBoolType) {
        self.src.set_ty(e.id, BuiltinType::Bool);
        self.expr_type = BuiltinType::Bool;
    }

    fn check_expr_lit_char(&mut self, e: &'ast ExprLitCharType) {
        self.src.set_ty(e.id, BuiltinType::Char);
        self.expr_type = BuiltinType::Char;
    }

    fn check_expr_template(&mut self, e: &'ast ExprTemplateType) {
        let stringable_trait = self.vm.vips.stringable_trait;

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                self.visit_expr(part);

                let implements_stringable = match self.expr_type {
                    BuiltinType::FctTypeParam(fct_id, tp_id) => {
                        assert_eq!(self.fct.id, fct_id);
                        self.fct.type_params[tp_id.idx()]
                            .trait_bounds
                            .contains(&stringable_trait)
                    }

                    BuiltinType::ClassTypeParam(cls_id, tp_id) => {
                        let cls = self.vm.classes.idx(cls_id);
                        let cls = cls.read();
                        cls.type_params[tp_id.idx()]
                            .trait_bounds
                            .contains(&stringable_trait)
                    }

                    _ => self.expr_type.implements_trait(self.vm, stringable_trait),
                };

                if implements_stringable {
                    continue;
                }

                let ty = self.expr_type.name(self.vm);
                self.vm
                    .diag
                    .lock()
                    .report(self.file, part.pos(), SemError::ExpectedStringable(ty));
            } else {
                assert!(part.is_lit_str());
            }
        }

        let str_ty = self.vm.cls(self.vm.vips.string_class);
        self.src.set_ty(e.id, str_ty);
        self.expr_type = str_ty;
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitChar(ref expr) => self.check_expr_lit_char(expr),
            ExprLitInt(ref expr) => self.check_expr_lit_int(expr),
            ExprLitFloat(ref expr) => self.check_expr_lit_float(expr),
            ExprLitStr(ref expr) => self.check_expr_lit_str(expr),
            ExprTemplate(ref expr) => self.check_expr_template(expr),
            ExprLitBool(ref expr) => self.check_expr_lit_bool(expr),
            ExprIdent(ref expr) => self.check_expr_ident(expr),
            ExprUn(ref expr) => self.check_expr_un(expr),
            ExprBin(ref expr) => self.check_expr_bin(expr),
            ExprCall(ref expr) => self.check_expr_call(expr),
            ExprTypeParam(ref expr) => self.check_expr_type_param(expr),
            ExprPath(ref expr) => self.check_expr_path(expr),
            ExprDelegation(ref expr) => self.check_expr_delegation(expr),
            ExprDot(ref expr) => self.check_expr_dot(expr),
            ExprSelf(ref expr) => self.check_expr_this(expr),
            ExprSuper(ref expr) => self.check_expr_super(expr),
            ExprNil(ref expr) => self.check_expr_nil(expr),
            ExprConv(ref expr) => self.check_expr_conv(expr),
            ExprLambda(ref expr) => self.check_expr_lambda(expr),
            ExprBlock(ref expr) => self.check_expr_block(expr),
            ExprIf(ref expr) => self.check_expr_if(expr),
            ExprTuple(ref expr) => self.check_expr_tuple(expr),
        }
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref stmt) => self.check_stmt_var(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtFor(ref stmt) => self.check_stmt_for(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),
            StmtDefer(ref stmt) => self.check_stmt_defer(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(_) => visit::walk_stmt(self, s),
            StmtContinue(_) => visit::walk_stmt(self, s),
            StmtExpr(_) => visit::walk_stmt(self, s),
        }

        self.src.set_ty(s.id(), BuiltinType::Unit);
        self.expr_type = BuiltinType::Unit;
    }
}

pub fn args_compatible(
    vm: &VM,
    def: &[BuiltinType],
    expr: &[BuiltinType],
    cls_id: Option<ClassId>,
    fct_id: Option<FctId>,
    cls_tps: &TypeList,
    fct_tps: &TypeList,
    self_ty: Option<BuiltinType>,
) -> bool {
    if def.len() != expr.len() {
        return false;
    }

    for (ind, &arg) in def.iter().enumerate() {
        if !arg_allows(
            vm, arg, expr[ind], cls_id, fct_id, cls_tps, fct_tps, self_ty,
        ) {
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
    cls_tps: &TypeList,
    fct_tps: &TypeList,
    self_ty: Option<BuiltinType>,
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
        | BuiltinType::Double
        | BuiltinType::Enum(_) => def == arg,
        BuiltinType::Nil => panic!("nil should not occur in fct definition."),
        BuiltinType::Ptr => panic!("ptr should not occur in fct definition."),
        BuiltinType::This => {
            let real = self_ty.expect("no Self type expected.");

            arg_allows(
                vm,
                real,
                arg,
                global_cls_id,
                global_fct_id,
                cls_tps,
                fct_tps,
                self_ty,
            )
        }
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
                None,
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
                self_ty,
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
                if !arg_allows(
                    vm,
                    tp,
                    op,
                    global_cls_id,
                    global_fct_id,
                    cls_tps,
                    fct_tps,
                    self_ty,
                ) {
                    return false;
                }
            }

            true
        }

        BuiltinType::Tuple(tuple_id) => match arg {
            BuiltinType::Tuple(other_tuple_id) => {
                if tuple_id == other_tuple_id {
                    return true;
                }

                let subtypes = vm.tuples.lock().get(tuple_id);
                let other_subtypes = vm.tuples.lock().get(other_tuple_id);

                if subtypes.len() != other_subtypes.len() {
                    return false;
                }

                let len = subtypes.len();

                for idx in 0..len {
                    let ty = subtypes[idx];
                    let other_ty = other_subtypes[idx];

                    if !arg_allows(
                        vm,
                        ty,
                        other_ty,
                        global_cls_id,
                        global_fct_id,
                        cls_tps,
                        fct_tps,
                        self_ty,
                    ) {
                        return false;
                    }
                }

                true
            }

            _ => false,
        },

        BuiltinType::Module(_) => def == arg,

        BuiltinType::Lambda(_) => {
            // for now expect the exact same params and return types
            // possible improvement: allow super classes for params,
            //                             sub class for return type
            def == arg
        }
    }
}

pub fn check_lit_int(
    vm: &VM,
    file: FileId,
    e: &ExprLitIntType,
    negative_expr_id: NodeId,
) -> (BuiltinType, i64) {
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
                .report(file, e.pos, SemError::NumberOverflow(ty_name.into()));
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
                .report(file, e.pos, SemError::NumberOverflow(ty_name.into()));
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
    file: FileId,
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
            .report(file, e.pos, SemError::NumberOverflow(ty.into()));
    }

    (ty, value)
}

pub fn lookup_method<'ast>(
    vm: &VM<'ast>,
    object_type: BuiltinType,
    is_static: bool,
    name: Name,
    args: &[BuiltinType],
    fct_tps: &TypeList,
    return_type: Option<BuiltinType>,
) -> Option<(ClassId, FctId, BuiltinType)> {
    let cls_id = object_type.cls_id(vm);

    if cls_id.is_some() {
        let candidates = find_methods_in_class(vm, object_type, name, is_static);

        if candidates.len() == 1 {
            let candidate = candidates[0].1;
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

            let cls_type_params = object_type.type_params(vm);

            if args_compatible(
                vm,
                &method.params_without_self(),
                args,
                Some(cls_id),
                Some(method.id),
                &cls_type_params,
                fct_tps,
                None,
            ) {
                let cmp_type =
                    replace_type_param(vm, method.return_type, &cls_type_params, fct_tps, None);

                if return_type.is_none() || return_type.unwrap() == cmp_type {
                    return Some((cls_id, candidate, cmp_type));
                }
            }
        }
    }

    None
}
