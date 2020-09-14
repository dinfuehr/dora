use std::collections::HashSet;
use std::convert::TryFrom;
use std::sync::Arc;
use std::{f32, f64};

use crate::error::msg::SemError;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck::{self, ErrorReporting};
use crate::semck::{always_returns, expr_always_returns};
use crate::sym::TypeSym::SymClass;
use crate::ty::{BuiltinType, TypeList};
use crate::typeck::lookup::MethodLookup;
use crate::vm::{
    self, ensure_tuple, find_field_in_class, find_methods_in_class, CallType, ClassId, ConvInfo,
    EnumId, Fct, FctId, FctParent, FctSrc, FileId, ForTypeInfo, IdentType, Intrinsic, VM,
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

            let return_type = self.fct.return_type;
            self.check_expr(value, return_type)
        } else {
            BuiltinType::Unit
        };

        if !returns {
            self.check_fct_return_type(block.pos, return_type);
        }
    }

    pub fn check_stmt_let(&mut self, s: &'ast StmtLetType) {
        let defined_type = if let Some(ref data_type) = s.data_type {
            self.src.ty(data_type.id())
        } else {
            BuiltinType::Any
        };

        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| self.check_expr(&expr, defined_type))
            .unwrap_or(BuiltinType::Any);

        let defined_type = if s.data_type.is_some() {
            defined_type
        } else {
            expr_type
        };

        if !defined_type.is_error() && !defined_type.is_defined_type(self.vm) {
            let tyname = self
                .vm
                .interner
                .str(s.pattern.to_name().unwrap())
                .to_string();
            self.vm
                .diag
                .lock()
                .report(self.file, s.pos, SemError::VarNeedsTypeInfo(tyname));

            return;
        }

        // update type of variable, necessary when variable is only initialized with
        // an expression
        self.check_stmt_let_pattern(&s.pattern, defined_type);

        if s.expr.is_some() {
            if !expr_type.is_error()
                && !defined_type.is_error()
                && !defined_type.allows(self.vm, expr_type)
            {
                let name = self
                    .vm
                    .interner
                    .str(s.pattern.to_name().unwrap())
                    .to_string();
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

    fn check_stmt_let_pattern(&mut self, pattern: &LetPattern, ty: BuiltinType) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var = *self.src.map_vars.get(ident.id).unwrap();
                self.src.vars[var].ty = ty;
            }

            LetPattern::Underscore(_) => {
                // nothing to do
            }

            LetPattern::Tuple(ref tuple) => {
                if !ty.is_tuple_or_unit() {
                    let ty_name = ty.name(self.vm);
                    self.vm.diag.lock().report(
                        self.file,
                        tuple.pos,
                        SemError::LetPatternExpectedTuple(ty_name),
                    );
                    return;
                }

                if ty.is_unit() {
                    // () doesn't have any subparts
                    if tuple.parts.len() != 0 {
                        self.vm.diag.lock().report(
                            self.file,
                            tuple.pos,
                            SemError::LetPatternShouldBeUnit,
                        );
                    }
                    return;
                }

                let tuple_id = ty.tuple_id().expect("type should be tuple");
                let parts = self.vm.tuples.lock().get(tuple_id).len();

                if parts != tuple.parts.len() {
                    let ty_name = ty.name(self.vm);
                    self.vm.diag.lock().report(
                        self.file,
                        tuple.pos,
                        SemError::LetPatternExpectedTupleWithLength(
                            ty_name,
                            parts,
                            tuple.parts.len(),
                        ),
                    );
                    return;
                }

                for (idx, part) in tuple.parts.iter().enumerate() {
                    let (ty, _) = self.vm.tuples.lock().get_at(tuple_id, idx);
                    self.check_stmt_let_pattern(part, ty);
                }
            }
        }
    }

    fn check_stmt_for(&mut self, stmt: &'ast StmtForType) {
        let object_type = self.check_expr(&stmt.expr, BuiltinType::Any);

        if object_type.is_error() {
            let var_id = *self.src.map_vars.get(stmt.id).unwrap();
            self.src.vars[var_id].ty = BuiltinType::Error;
            self.visit_stmt(&stmt.block);
            return;
        }

        if let Some(cls_id) = object_type.cls_id(self.vm) {
            if cls_id == self.vm.vips.array_class {
                let type_list = object_type.type_params(self.vm);
                let var_ty = type_list[0];

                self.check_stmt_let_pattern(&stmt.pattern, var_ty);

                self.visit_stmt(&stmt.block);
                return;
            }
        }

        if let Some((for_type_info, ret_type)) = self.type_supports_iterator_protocol(object_type) {
            // set variable type to return type of next
            self.check_stmt_let_pattern(&stmt.pattern, ret_type);

            // store fct ids for code generation
            self.src.map_fors.insert(stmt.id, for_type_info);

            self.visit_stmt(&stmt.block);
            return;
        }

        if let Some((make_iterator, iterator_type)) = self.type_supports_make_iterator(object_type)
        {
            if let Some((mut for_type_info, ret_type)) =
                self.type_supports_iterator_protocol(iterator_type)
            {
                // set variable type to return type of next
                self.check_stmt_let_pattern(&stmt.pattern, ret_type);

                // store fct ids for code generation
                for_type_info.make_iterator = Some(make_iterator);
                self.src.map_fors.insert(stmt.id, for_type_info);

                self.visit_stmt(&stmt.block);
                return;
            }
        }

        let name = object_type.name(self.vm);
        let msg = SemError::TypeNotUsableInForIn(name);
        self.vm.diag.lock().report(self.file, stmt.expr.pos(), msg);

        // set invalid error type
        let ident = stmt.pattern.to_ident().expect("ident");
        let var_id = *self.src.map_vars.get(ident.id).unwrap();
        self.src.vars[var_id].ty = BuiltinType::Error;

        self.visit_stmt(&stmt.block);
    }

    fn type_supports_make_iterator(
        &mut self,
        object_type: BuiltinType,
    ) -> Option<(FctId, BuiltinType)> {
        let make_iterator_name = self.vm.interner.intern("makeIterator");

        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type)
            .name(make_iterator_name)
            .args(&[]);

        if lookup.find() {
            let make_iterator_id = lookup.found_fct_id().unwrap();
            let make_iterator_ret = lookup.found_ret().unwrap();

            Some((make_iterator_id, make_iterator_ret))
        } else {
            None
        }
    }

    fn type_supports_iterator_protocol(
        &mut self,
        object_type: BuiltinType,
    ) -> Option<(ForTypeInfo, BuiltinType)> {
        let has_next_name = self.vm.interner.intern("hasNext");

        let mut has_next = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type)
            .name(has_next_name)
            .args(&[]);

        if !has_next.find() {
            return None;
        }
        if !has_next.found_ret().unwrap().is_bool() {
            return None;
        }

        let next_name = self.vm.interner.intern("next");

        let mut next = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type)
            .name(next_name)
            .args(&[]);

        if !next.find() {
            return None;
        }

        let next_type = next.found_ret().unwrap();

        Some((
            ForTypeInfo {
                make_iterator: None,
                has_next: has_next.found_fct_id().expect("fct_id missing"),
                next: next.found_fct_id().expect("fct_id missing"),
                iterator_type: object_type,
                next_type,
            },
            next_type,
        ))
    }

    fn check_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let expr_type = self.check_expr(&s.cond, BuiltinType::Any);

        if !expr_type.is_error() && !expr_type.is_bool() {
            let expr_type = expr_type.name(self.vm);
            let msg = SemError::WhileCondType(expr_type);
            self.vm.diag.lock().report(self.file, s.pos, msg);
        }

        self.visit_stmt(&s.block);
    }

    fn check_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| self.check_expr(&expr, BuiltinType::Any))
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

    fn check_expr_block(
        &mut self,
        block: &'ast ExprBlockType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let ty = if let Some(ref expr) = block.expr {
            self.check_expr(expr, BuiltinType::Any)
        } else {
            BuiltinType::Unit
        };

        self.src.set_ty(block.id, ty);

        ty
    }

    fn check_expr_tuple(
        &mut self,
        tuple: &'ast ExprTupleType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let mut subtypes = Vec::new();

        if tuple.values.is_empty() {
            self.src.set_ty(tuple.id, BuiltinType::Unit);
            return BuiltinType::Unit;
        }

        for value in &tuple.values {
            let subtype = self.check_expr(value, BuiltinType::Any);
            subtypes.push(subtype);
        }

        let tuple_id = ensure_tuple(self.vm, subtypes);

        let ty = BuiltinType::Tuple(tuple_id);
        self.src.set_ty(tuple.id, ty);

        ty
    }

    fn check_expr_paren(
        &mut self,
        paren: &'ast ExprParenType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let ty = self.check_expr(&paren.expr, BuiltinType::Any);
        self.src.set_ty(paren.id, ty);

        ty
    }

    fn check_expr_if(&mut self, expr: &'ast ExprIfType, _expected_ty: BuiltinType) -> BuiltinType {
        let expr_type = self.check_expr(&expr.cond, BuiltinType::Any);

        if !expr_type.is_bool() && !expr_type.is_error() {
            let expr_type = expr_type.name(self.vm);
            let msg = SemError::IfCondType(expr_type);
            self.vm.diag.lock().report(self.file, expr.pos, msg);
        }

        let then_type = self.check_expr(&expr.then_block, BuiltinType::Any);

        let merged_type = if let Some(ref else_block) = expr.else_block {
            let else_type = self.check_expr(else_block, BuiltinType::Any);

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

        merged_type
    }

    fn check_expr_ident(
        &mut self,
        e: &'ast ExprIdentType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let ident_type = self.src.map_idents.get(e.id).unwrap();

        match ident_type {
            &IdentType::Var(varid) => {
                let ty = self.src.vars[varid].ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            &IdentType::Global(globalid) => {
                let glob = self.vm.globals.idx(globalid);
                let ty = glob.read().ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            &IdentType::Field(ty, fieldid) => {
                let clsid = ty.cls_id(self.vm).unwrap();
                let cls = self.vm.classes.idx(clsid);
                let cls = cls.read();
                let field = &cls.fields[fieldid];

                self.src.set_ty(e.id, field.ty);

                field.ty
            }

            &IdentType::Struct(sid) => {
                let list_id = self.vm.lists.lock().insert(TypeList::empty());
                let ty = BuiltinType::Struct(sid, list_id);
                self.src.set_ty(e.id, ty);

                ty
            }

            &IdentType::Const(const_id) => {
                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.lock();

                self.src.set_ty(e.id, xconst.ty);

                xconst.ty
            }

            &IdentType::Fct(_) => {
                if !self.used_in_call.contains(&e.id) {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file, e.pos, SemError::FctUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);

                BuiltinType::Error
            }

            &IdentType::Class(_) => {
                if !self.used_in_call.contains(&e.id) {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file, e.pos, SemError::ClsUsedAsIdentifier);
                }

                self.src.set_ty(e.id, BuiltinType::Error);

                BuiltinType::Error
            }

            &IdentType::Module(module_id)
            | &IdentType::ClassAndModule(_, module_id)
            | &IdentType::StructAndModule(_, module_id) => {
                let module = self.vm.modules.idx(module_id);
                let ty = module.read().ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            &IdentType::TypeParam(_) => {
                let msg = if self.used_in_call.contains(&e.id) {
                    SemError::TypeParamUsedAsCallee
                } else {
                    SemError::TypeParamUsedAsIdentifier
                };

                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Error);

                BuiltinType::Error
            }

            &IdentType::Enum(_) => {
                let msg = SemError::EnumUsedAsIdentifier;
                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Error);

                BuiltinType::Error
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
            self.check_expr_assign_ident(e);
        } else {
            self.vm
                .diag
                .lock()
                .report(self.file, e.pos, SemError::LvalueExpected);
        }

        self.src.set_ty(e.id, BuiltinType::Unit);
    }

    fn check_expr_assign_ident(&mut self, e: &'ast ExprBinType) {
        let lhs_type;

        let rhs_type = self.check_expr(&e.rhs, BuiltinType::Any);

        self.src.set_ty(e.id, BuiltinType::Unit);

        let ident_type = self.src.map_idents.get(e.lhs.id());

        if ident_type.is_none() {
            return;
        }

        let ident_type = ident_type.unwrap();

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
                let glob = glob.read();

                if !e.initializer && !glob.reassignable {
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

            &IdentType::Struct(_) | &IdentType::StructAndModule(_, _) => {
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

            &IdentType::Class(_)
            | &IdentType::ClassType(_, _)
            | &IdentType::ClassAndModule(_, _) => {
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
                self.vm
                    .diag
                    .lock()
                    .report(self.file, e.pos, SemError::InvalidLhsAssignment);

                return;
            }

            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }

        if !lhs_type.is_error() && !rhs_type.is_error() && !lhs_type.allows(self.vm, rhs_type) {
            let ident = e.lhs.to_ident().unwrap();
            let name = self.vm.interner.str(ident.name).to_string();
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);

            self.src.set_ty(e.id, BuiltinType::Unit);

            let msg = SemError::AssignType(name, lhs_type, rhs_type);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        }
    }

    fn check_expr_assign_call(&mut self, e: &'ast ExprBinType) {
        let call = e.lhs.to_call().unwrap();
        let expr_type = self.check_expr(&call.callee, BuiltinType::Any);

        let mut arg_types: Vec<BuiltinType> = call
            .args
            .iter()
            .map(|arg| self.check_expr(arg, BuiltinType::Any))
            .collect();

        let value_type = self.check_expr(&e.rhs, BuiltinType::Any);

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
                return;
            }
        };

        let object_type = self.check_expr(&field_expr.lhs, BuiltinType::Any);
        let rhs_type = self.check_expr(&e.rhs, BuiltinType::Any);

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id, _)) = find_field_in_class(self.vm, object_type, name) {
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

                if !e.initializer && !field.reassignable {
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
                return;
            }
        }

        // field not found, report error
        let field_name = self.vm.interner.str(name).to_string();
        let expr_name = object_type.name(self.vm);
        let msg = SemError::UnknownField(field_name, expr_name);
        self.vm.diag.lock().report(self.file, field_expr.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Unit);
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

    fn check_expr_un(&mut self, e: &'ast ExprUnType, _expected_ty: BuiltinType) -> BuiltinType {
        if e.op == UnOp::Neg && e.opnd.is_lit_int() {
            let expr_type =
                self.check_expr_lit_int(e.opnd.to_lit_int().unwrap(), true, BuiltinType::Any);
            self.src.set_ty(e.id, expr_type);
            return expr_type;
        }

        let opnd = self.check_expr(&e.opnd, BuiltinType::Any);

        match e.op {
            UnOp::Plus => self.check_expr_un_method(e, e.op, "unaryPlus", opnd),
            UnOp::Neg => self.check_expr_un_method(e, e.op, "unaryMinus", opnd),
            UnOp::Not => self.check_expr_un_method(e, e.op, "not", opnd),
        }
    }

    fn check_expr_un_method(
        &mut self,
        e: &'ast ExprUnType,
        op: UnOp,
        name: &str,
        ty: BuiltinType,
    ) -> BuiltinType {
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
                return return_type;
            }

            let ty = ty.name(self.vm);
            let msg = SemError::UnOpType(op.as_str().into(), ty);

            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        self.src.set_ty(e.id, BuiltinType::Error);

        BuiltinType::Error
    }

    fn check_expr_bin(&mut self, e: &'ast ExprBinType, _expected_ty: BuiltinType) -> BuiltinType {
        if e.op.is_any_assign() {
            self.check_expr_assign(e);
            return BuiltinType::Unit;
        }

        let lhs_type = self.check_expr(&e.lhs, BuiltinType::Any);
        let rhs_type = self.check_expr(&e.rhs, BuiltinType::Any);

        if lhs_type.is_error() || rhs_type.is_error() {
            self.src.set_ty(e.id, BuiltinType::Error);
            return BuiltinType::Error;
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
    ) -> BuiltinType {
        self.check_type(e, op, lhs_type, rhs_type, BuiltinType::Bool);
        self.src.set_ty(e.id, BuiltinType::Bool);

        BuiltinType::Bool
    }

    fn check_expr_bin_method(
        &mut self,
        e: &'ast ExprBinType,
        op: BinOp,
        name: &str,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) -> BuiltinType {
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

            return_type
        } else {
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = SemError::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);

            BuiltinType::Error
        }
    }

    fn check_expr_bin_cmp(
        &mut self,
        e: &'ast ExprBinType,
        cmp: CmpOp,
        lhs_type: BuiltinType,
        rhs_type: BuiltinType,
    ) -> BuiltinType {
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
                return BuiltinType::Bool;
            }

            CmpOp::Eq | CmpOp::Ne => {
                if lhs_type.is_enum() {
                    self.check_expr_cmp_enum(e, cmp, lhs_type, rhs_type)
                } else {
                    self.check_expr_bin_method(e, e.op, "equals", lhs_type, rhs_type);
                }
            }

            _ => {
                self.check_expr_bin_method(e, e.op, "compareTo", lhs_type, rhs_type);
            }
        }

        self.src.set_ty(e.id, BuiltinType::Bool);

        BuiltinType::Bool
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
        } else {
            let lhs_type = lhs_type.name(self.vm);
            let rhs_type = rhs_type.name(self.vm);
            let msg = SemError::BinOpType("equals".into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file, e.pos, msg);

            self.src.set_ty(e.id, BuiltinType::Error);
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

    fn check_expr_call(&mut self, e: &'ast ExprCallType, _expected_ty: BuiltinType) -> BuiltinType {
        self.used_in_call.insert(e.callee.id());

        let expr_type = self.check_expr(&e.callee, BuiltinType::Any);
        let ident_type = self.src.map_idents.get(e.callee.id()).cloned();

        let arg_types: Vec<BuiltinType> = e
            .args
            .iter()
            .map(|arg| self.check_expr(arg, BuiltinType::Any))
            .collect();

        // Workaround to use .get() method on Arrays when used as field: (self.field)(idx)
        if e.callee.is_paren() {
            if expr_type.is_error() {
                self.src.set_ty(e.id, expr_type);
                return expr_type;
            }

            return self.check_expr_call_expr(e, expr_type, &arg_types);
        }

        match ident_type {
            Some(IdentType::Fct(fct_id)) => {
                self.check_expr_call_ident(e, fct_id, TypeList::empty(), &arg_types)
            }

            Some(IdentType::FctType(fct_id, type_params)) => {
                self.check_expr_call_ident(e, fct_id, type_params, &arg_types)
            }

            Some(IdentType::Class(cls_id)) | Some(IdentType::ClassAndModule(cls_id, _)) => {
                self.check_expr_call_ctor(e, cls_id, TypeList::empty(), &arg_types)
            }

            Some(IdentType::ClassType(cls_id, type_params)) => {
                self.check_expr_call_ctor(e, cls_id, type_params, &arg_types)
            }

            Some(IdentType::Method(object_type, method_name)) => self.check_expr_call_method(
                e,
                object_type,
                method_name,
                TypeList::empty(),
                &arg_types,
            ),

            Some(IdentType::MethodType(object_type, method_name, type_params)) => {
                self.check_expr_call_method(e, object_type, method_name, type_params, &arg_types)
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

                BuiltinType::Error
            }

            Some(IdentType::Enum(_)) => {
                self.src.set_ty(e.id, BuiltinType::Error);

                BuiltinType::Error
            }

            Some(IdentType::EnumValue(enum_id, variant_id)) => {
                self.check_expr_call_enum(e, enum_id, variant_id, &arg_types)
            }

            _ => {
                if expr_type.is_error() {
                    self.src.set_ty(e.id, expr_type);
                    return expr_type;
                }

                self.check_expr_call_expr(e, expr_type, &arg_types)
            }
        }
    }

    fn check_expr_call_enum(
        &mut self,
        e: &'ast ExprCallType,
        enum_id: EnumId,
        variant_id: u32,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let xenum = self.vm.enums[enum_id].read();
        let variant = &xenum.variants[variant_id as usize];

        if !self.check_expr_call_enum_args(variant, arg_types) {
            let enum_name = self.vm.interner.str(xenum.name).to_string();
            let variant_name = self.vm.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let msg =
                SemError::EnumArgsIncompatible(enum_name, variant_name, variant_types, arg_types);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        } else if variant.types.is_empty() {
            let enum_name = self.vm.interner.str(xenum.name).to_string();
            let variant_name = self.vm.interner.str(variant.name).to_string();
            let msg = SemError::EnumArgsNoParens(enum_name, variant_name);
            self.vm.diag.lock().report(self.file, e.pos, msg);
        }

        let list_id = self.vm.lists.lock().insert(TypeList::empty());
        let ty = BuiltinType::Enum(enum_id, list_id);
        self.src.set_ty(e.id, ty);
        return ty;
    }

    fn check_expr_call_enum_args(
        &mut self,
        variant: &vm::EnumVariant,
        arg_types: &[BuiltinType],
    ) -> bool {
        if variant.types.len() != arg_types.len() {
            return false;
        }

        for (def_ty, &arg_ty) in variant.types.iter().zip(arg_types) {
            if !def_ty.allows(self.vm, arg_ty) {
                return false;
            }
        }

        true
    }

    fn check_expr_call_generic_static_method(
        &mut self,
        e: &'ast ExprCallType,
        tp: BuiltinType,
        name: Name,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let mut fcts = Vec::new();

        let (type_param, tp_id) = self
            .fct
            .type_param_ty(self.vm, tp, |tp, tp_id| (tp.clone(), tp_id));

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
            return BuiltinType::Error;
        }

        if arg_types.contains(&BuiltinType::Error) {
            self.src.set_ty(e.id, BuiltinType::Error);
            return BuiltinType::Error;
        }

        let (trait_id, fct_id) = fcts[0];
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        if !args_compatible(
            self.vm,
            &*fct,
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

        let call_type = CallType::TraitStatic(tp_id, trait_id, fct_id);
        self.src.map_calls.insert(e.id, Arc::new(call_type));

        let return_type = replace_type_param(
            self.vm,
            fct.return_type,
            &TypeList::empty(),
            &TypeList::empty(),
            Some(tp),
        );

        self.src.set_ty(e.id, return_type);

        return_type
    }

    fn check_expr_call_expr(
        &mut self,
        e: &'ast ExprCallType,
        expr_type: BuiltinType,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let get = self.vm.interner.intern("get");

        if let Some((_, fct_id, return_type)) =
            self.find_method(e.pos, expr_type, false, get, arg_types, &TypeList::empty())
        {
            let call_type = CallType::Expr(expr_type, fct_id);
            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.src.set_ty(e.id, return_type);

            return_type
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);

            BuiltinType::Error
        }
    }

    fn check_expr_call_ident(
        &mut self,
        e: &'ast ExprCallType,
        fct_id: FctId,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let mut lookup = MethodLookup::new(self.vm, self.fct)
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

        ty
    }

    fn check_expr_call_static_method(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls_type_params = object_type.type_params(self.vm);
        assert_eq!(cls_type_params.len(), 0);

        let mut lookup = MethodLookup::new(self.vm, self.fct)
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

            return_type
        } else {
            self.src.set_ty(e.id, BuiltinType::Error);

            BuiltinType::Error
        }
    }

    fn check_expr_call_method(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        if object_type.is_type_param() {
            assert_eq!(type_params.len(), 0);
            return self.check_expr_call_generic(e, object_type, method_name, arg_types);
        }

        if object_type.is_error() {
            self.src.set_ty(e.id, BuiltinType::Error);

            return BuiltinType::Error;
        }

        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type)
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
                if method_type.is_module() {
                    CallType::ModuleMethod(method_type, fct_id, type_params.clone())
                } else {
                    CallType::Method(method_type, fct_id, type_params.clone())
                }
            };

            self.src
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
            self.src.set_ty(e.id, return_type);

            return_type
        } else if !object_type.is_nil() && lookup.found_fct_id().is_none() {
            // No method with this name found, so this might actually be a field
            self.check_expr_call_field(e, object_type, method_name, type_params, arg_types)
        } else {
            // Lookup the method again, but this time with error reporting
            let mut lookup = MethodLookup::new(self.vm, self.fct)
                .method(object_type)
                .name(method_name)
                .fct_type_params(&type_params)
                .pos(e.pos)
                .args(arg_types);

            assert!(!lookup.find());

            self.src.set_ty(e.id, BuiltinType::Error);

            BuiltinType::Error
        }
    }

    fn check_expr_call_field(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        if let Some((actual_type, field_id, field_type)) =
            find_field_in_class(self.vm, object_type, method_name)
        {
            self.src.set_ty(e.callee.id(), field_type);
            self.src
                .map_idents
                .insert_or_replace(e.callee.id(), IdentType::Field(actual_type, field_id));

            return self.check_expr_call_expr(e, field_type, arg_types);
        }

        // No field with that name as well, so report method
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .method(object_type)
            .name(method_name)
            .fct_type_params(&type_params)
            .pos(e.pos)
            .args(arg_types);
        assert!(!lookup.find());

        self.src.set_ty(e.id, BuiltinType::Error);

        BuiltinType::Error
    }

    fn check_expr_call_ctor(
        &mut self,
        e: &'ast ExprCallType,
        cls_id: ClassId,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .pos(e.pos)
            .ctor(cls_id)
            .args(arg_types)
            .cls_type_params(&type_params);

        let ty = if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            let cls_ty = self.vm.cls_with_type_list(cls_id, type_params.clone());
            let call_type = CallType::Ctor(cls_ty, fct_id);
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

        ty
    }

    fn check_expr_call_generic(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        name: Name,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
        self.fct.type_param_ty(self.vm, object_type, |tp, _| {
            self.check_expr_call_generic_type_param(e, object_type, tp, name, arg_types)
        })
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &'ast ExprCallType,
        object_type: BuiltinType,
        tp: &vm::TypeParam,
        name: Name,
        args: &[BuiltinType],
    ) -> BuiltinType {
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

            return_type
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

            BuiltinType::Error
        }
    }

    fn check_expr_call_path(
        &mut self,
        e: &'ast ExprCallType,
        type_params: TypeList,
        arg_types: &[BuiltinType],
    ) -> BuiltinType {
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
            return BuiltinType::Error;
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
            return BuiltinType::Error;
        }

        match self.vm.sym.lock().get_type(class) {
            Some(SymClass(cls_id)) => {
                let mut lookup = MethodLookup::new(self.vm, self.fct)
                    .pos(e.pos)
                    .static_method(cls_id)
                    .name(method_name)
                    .args(arg_types)
                    .fct_type_params(&type_params);

                let ty = if lookup.find() {
                    let fct_id = lookup.found_fct_id().unwrap();
                    let call_type = Arc::new(CallType::Fct(
                        fct_id,
                        TypeList::empty(),
                        type_params.clone(),
                    ));
                    self.src.map_calls.insert(e.id, call_type.clone());
                    let ty = lookup.found_ret().unwrap();
                    self.src.set_ty(e.id, ty);

                    ty
                } else {
                    self.src.set_ty(e.id, BuiltinType::Error);

                    BuiltinType::Error
                };

                return ty;
            }

            _ => {}
        }

        let msg = SemError::ClassExpected;
        self.vm.diag.lock().report(self.file, e.pos, msg);

        self.src.set_ty(e.id, BuiltinType::Error);

        BuiltinType::Error
    }

    fn check_expr_delegation(
        &mut self,
        e: &'ast ExprDelegationType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let arg_types: Vec<BuiltinType> = e
            .args
            .iter()
            .map(|arg| self.check_expr(arg, BuiltinType::Any))
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
                &*ctor,
                &arg_types,
                Some(cls_id),
                None,
                &parent_class_type_params,
                &TypeList::empty(),
                None,
            ) {
                self.src.map_tys.insert(e.id, parent_class);

                let cls_ty = self.vm.cls_with_type_list(cls_id, parent_class_type_params);
                let call_type = CallType::CtorParent(cls_ty, ctor.id);
                self.src.map_calls.insert(e.id, Arc::new(call_type));
                return BuiltinType::Error;
            }
        }

        let name = self.vm.interner.str(cls.name).to_string();
        let arg_types = arg_types.iter().map(|t| t.name(self.vm)).collect();
        let msg = SemError::UnknownCtor(name, arg_types);
        self.vm.diag.lock().report(self.file, e.pos, msg);

        BuiltinType::Error
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

    fn check_expr_path(&mut self, e: &'ast ExprPathType, _expected_ty: BuiltinType) -> BuiltinType {
        let ident_type = self.src.map_idents.get(e.lhs.id());

        let name = if let Some(ident) = e.rhs.to_ident() {
            ident.name
        } else {
            let msg = SemError::NameOfStaticMethodExpected;
            self.vm.diag.lock().report(self.file, e.rhs.pos(), msg);
            return BuiltinType::Error;
        };

        let ident_type = match ident_type {
            Some(&IdentType::Class(cls_id)) => {
                let list = self.vm.lists.lock().insert(TypeList::empty());
                let cls_ty = BuiltinType::Class(cls_id, list);

                IdentType::StaticMethod(cls_ty, name)
            }

            Some(&IdentType::Module(module_id))
            | Some(&IdentType::ClassAndModule(_, module_id)) => {
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
                    let variant = &xenum.variants[value as usize];

                    if !self.used_in_call.contains(&e.id) && !variant.types.is_empty() {
                        let enum_name = self.vm.interner.str(xenum.name).to_string();
                        let variant_name = self.vm.interner.str(variant.name).to_string();
                        let variant_types = variant
                            .types
                            .iter()
                            .map(|a| a.name(self.vm))
                            .collect::<Vec<_>>();
                        let arg_types = Vec::new();
                        let msg = SemError::EnumArgsIncompatible(
                            enum_name,
                            variant_name,
                            variant_types,
                            arg_types,
                        );
                        self.vm.diag.lock().report(self.file, e.pos, msg);
                    }

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

                let list_id = self.vm.lists.lock().insert(TypeList::empty());
                let ty = BuiltinType::Enum(id, list_id);
                self.src.set_ty(e.id, ty);
                return ty;
            }

            _ => {
                let msg = SemError::InvalidLeftSideOfSeparator;
                self.vm.diag.lock().report(self.file, e.lhs.pos(), msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                return BuiltinType::Error;
            }
        };

        if self.used_in_call.contains(&e.id) {
            self.src.map_idents.insert(e.id, ident_type);
            return BuiltinType::Error;
        }

        self.vm
            .diag
            .lock()
            .report(self.file, e.pos, SemError::FctUsedAsIdentifier);

        BuiltinType::Error
    }

    fn check_expr_type_param(
        &mut self,
        e: &'ast ExprTypeParamType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        if self.used_in_call.contains(&e.id) {
            self.used_in_call.insert(e.callee.id());
        }

        let expr_type = self.check_expr(&e.callee, BuiltinType::Any);
        let ident_type = self.src.map_idents.get(e.callee.id()).cloned();

        let type_params: Vec<BuiltinType> = e.args.iter().map(|p| self.src.ty(p.id())).collect();
        let type_params: TypeList = TypeList::with(type_params);

        match ident_type {
            Some(IdentType::Class(cls_id)) | Some(IdentType::ClassAndModule(cls_id, _)) => {
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
                return expr_type;
            }
        }

        expr_type
    }

    fn check_expr_dot(&mut self, e: &'ast ExprDotType, _expected_ty: BuiltinType) -> BuiltinType {
        let object_type = if e.lhs.is_super() {
            self.super_type(e.lhs.pos())
        } else {
            self.check_expr(&e.lhs, BuiltinType::Any)
        };

        if object_type.is_tuple() {
            return self.check_expr_dot_tuple(e, object_type);
        }

        let name = match e.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = SemError::NameExpected;
                self.vm.diag.lock().report(self.file, e.pos, msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                return BuiltinType::Error;
            }
        };

        if self.used_in_call.contains(&e.id) {
            self.src
                .map_idents
                .insert(e.id, IdentType::Method(object_type, name));
            return BuiltinType::Error;
        }

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id, _)) = find_field_in_class(self.vm, object_type, name) {
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
                return fty;
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

        BuiltinType::Error
    }

    fn check_expr_dot_tuple(
        &mut self,
        e: &'ast ExprDotType,
        object_type: BuiltinType,
    ) -> BuiltinType {
        let index = match e.rhs.to_lit_int() {
            Some(ident) => ident.value,

            None => {
                let msg = SemError::IndexExpected;
                self.vm.diag.lock().report(self.file, e.pos, msg);

                self.src.set_ty(e.id, BuiltinType::Error);
                return BuiltinType::Error;
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
            return BuiltinType::Error;
        }

        let ty = tuple[usize::try_from(index).unwrap()];
        self.src.set_ty(e.id, ty);

        ty
    }

    fn check_expr_this(&mut self, e: &'ast ExprSelfType, _expected_ty: BuiltinType) -> BuiltinType {
        match self.fct.parent {
            FctParent::Class(clsid) => {
                let cls = self.vm.classes.idx(clsid);
                let cls = cls.read();
                let ty = cls.ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
                let cls = cls.read();
                let ty = cls.ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            FctParent::Extension(extension_id) => {
                let extension = self.vm.extensions[extension_id].read();
                let ty = extension.class_ty;
                self.src.set_ty(e.id, ty);

                ty
            }

            _ => {
                let msg = SemError::ThisUnavailable;
                self.vm.diag.lock().report(self.file, e.pos, msg);
                self.src.set_ty(e.id, BuiltinType::Unit);

                BuiltinType::Unit
            }
        }
    }

    fn check_expr_super(
        &mut self,
        e: &'ast ExprSuperType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let msg = SemError::SuperNeedsMethodCall;
        self.vm.diag.lock().report(self.file, e.pos, msg);
        self.src.set_ty(e.id, BuiltinType::Unit);

        BuiltinType::Unit
    }

    fn check_expr_nil(&mut self, e: &'ast ExprNilType, _expected_ty: BuiltinType) -> BuiltinType {
        self.src.set_ty(e.id, BuiltinType::Nil);

        BuiltinType::Nil
    }

    fn check_expr_lambda(
        &mut self,
        e: &'ast ExprLambdaType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
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

        self.src.set_ty(e.id, ty);

        ty
    }

    fn check_expr_conv(&mut self, e: &'ast ExprConvType, _expected_ty: BuiltinType) -> BuiltinType {
        let object_type = self.check_expr(&e.object, BuiltinType::Any);
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
            return ty;
        }

        let error = ErrorReporting::Yes(self.file, e.data_type.pos());

        if !typeparamck::check_in_fct(self.vm, self.fct, error, check_type) {
            let ty = if e.is {
                BuiltinType::Bool
            } else {
                BuiltinType::Error
            };
            self.src.set_ty(e.id, ty);
            return ty;
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

        ty
    }

    fn check_expr_lit_int(
        &mut self,
        e: &'ast ExprLitIntType,
        negate: bool,
        expected_ty: BuiltinType,
    ) -> BuiltinType {
        let (ty, _) = check_lit_int(self.vm, self.file, e, negate, expected_ty);

        self.src.set_ty(e.id, ty);

        ty
    }

    fn check_expr_lit_float(
        &mut self,
        e: &'ast ExprLitFloatType,
        negate: bool,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let (ty, _) = check_lit_float(self.vm, self.file, e, negate);

        self.src.set_ty(e.id, ty);

        ty
    }

    fn check_expr_lit_str(
        &mut self,
        e: &'ast ExprLitStrType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let str_ty = self.vm.cls(self.vm.vips.string_class);
        self.src.set_ty(e.id, str_ty);

        str_ty
    }

    fn check_expr_lit_bool(
        &mut self,
        e: &'ast ExprLitBoolType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        self.src.set_ty(e.id, BuiltinType::Bool);

        BuiltinType::Bool
    }

    fn check_expr_lit_char(
        &mut self,
        e: &'ast ExprLitCharType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        self.src.set_ty(e.id, BuiltinType::Char);

        BuiltinType::Char
    }

    fn check_expr_template(
        &mut self,
        e: &'ast ExprTemplateType,
        _expected_ty: BuiltinType,
    ) -> BuiltinType {
        let stringable_trait = self.vm.vips.stringable_trait;

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = self.check_expr(part, BuiltinType::Any);

                let implements_stringable = if part_expr.is_type_param() {
                    self.fct.type_param_ty(self.vm, part_expr, |tp, _| {
                        tp.trait_bounds.contains(&stringable_trait)
                    })
                } else {
                    part_expr.implements_trait(self.vm, stringable_trait)
                };

                if implements_stringable || part_expr.is_error() {
                    continue;
                }

                let ty = part_expr.name(self.vm);
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

        str_ty
    }

    fn check_expr(&mut self, e: &'ast Expr, expected_ty: BuiltinType) -> BuiltinType {
        match *e {
            ExprLitChar(ref expr) => self.check_expr_lit_char(expr, expected_ty),
            ExprLitInt(ref expr) => self.check_expr_lit_int(expr, false, expected_ty),
            ExprLitFloat(ref expr) => self.check_expr_lit_float(expr, false, expected_ty),
            ExprLitStr(ref expr) => self.check_expr_lit_str(expr, expected_ty),
            ExprTemplate(ref expr) => self.check_expr_template(expr, expected_ty),
            ExprLitBool(ref expr) => self.check_expr_lit_bool(expr, expected_ty),
            ExprIdent(ref expr) => self.check_expr_ident(expr, expected_ty),
            ExprUn(ref expr) => self.check_expr_un(expr, expected_ty),
            ExprBin(ref expr) => self.check_expr_bin(expr, expected_ty),
            ExprCall(ref expr) => self.check_expr_call(expr, expected_ty),
            ExprTypeParam(ref expr) => self.check_expr_type_param(expr, expected_ty),
            ExprPath(ref expr) => self.check_expr_path(expr, expected_ty),
            ExprDelegation(ref expr) => self.check_expr_delegation(expr, expected_ty),
            ExprDot(ref expr) => self.check_expr_dot(expr, expected_ty),
            ExprSelf(ref expr) => self.check_expr_this(expr, expected_ty),
            ExprSuper(ref expr) => self.check_expr_super(expr, expected_ty),
            ExprNil(ref expr) => self.check_expr_nil(expr, expected_ty),
            ExprConv(ref expr) => self.check_expr_conv(expr, expected_ty),
            ExprLambda(ref expr) => self.check_expr_lambda(expr, expected_ty),
            ExprBlock(ref expr) => self.check_expr_block(expr, expected_ty),
            ExprIf(ref expr) => self.check_expr_if(expr, expected_ty),
            ExprTuple(ref expr) => self.check_expr_tuple(expr, expected_ty),
            ExprParen(ref expr) => self.check_expr_paren(expr, expected_ty),
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for TypeCheck<'a, 'ast> {
    fn visit_expr(&mut self, _e: &'ast Expr) {
        unreachable!();
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtLet(ref stmt) => self.check_stmt_let(stmt),
            StmtWhile(ref stmt) => self.check_stmt_while(stmt),
            StmtFor(ref stmt) => self.check_stmt_for(stmt),
            StmtReturn(ref stmt) => self.check_stmt_return(stmt),

            // for the rest of the statements, no special handling is necessary
            StmtBreak(_) => visit::walk_stmt(self, s),
            StmtContinue(_) => visit::walk_stmt(self, s),
            StmtExpr(ref stmt) => {
                self.check_expr(&stmt.expr, BuiltinType::Any);
            }
        }

        self.src.set_ty(s.id(), BuiltinType::Unit);
    }
}

pub fn args_compatible(
    vm: &VM,
    fct: &Fct,
    expr: &[BuiltinType],
    cls_id: Option<ClassId>,
    fct_id: Option<FctId>,
    cls_tps: &TypeList,
    fct_tps: &TypeList,
    self_ty: Option<BuiltinType>,
) -> bool {
    let def = fct.params_without_self();

    let right_number_of_arguments = if fct.variadic_arguments {
        def.len() - 1 <= expr.len()
    } else {
        def.len() == expr.len()
    };

    if !right_number_of_arguments {
        return false;
    }

    let (def, rest_ty): (&[BuiltinType], Option<BuiltinType>) = if fct.variadic_arguments {
        (&def[0..def.len() - 1], def.last().cloned())
    } else {
        (&def, None)
    };

    for (ind, &arg) in def.iter().enumerate() {
        if !arg_allows(
            vm, arg, expr[ind], cls_id, fct_id, cls_tps, fct_tps, self_ty,
        ) {
            return false;
        }
    }

    if let Some(rest_ty) = rest_ty {
        let ind = def.len();

        for &expr_ty in &expr[ind..] {
            if !arg_allows(
                vm, rest_ty, expr_ty, cls_id, fct_id, cls_tps, fct_tps, self_ty,
            ) {
                return false;
            }
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
        BuiltinType::Error | BuiltinType::Any => unreachable!(),
        BuiltinType::Unit
        | BuiltinType::Bool
        | BuiltinType::UInt8
        | BuiltinType::Char
        | BuiltinType::Struct(_, _)
        | BuiltinType::Int32
        | BuiltinType::Int64
        | BuiltinType::Float32
        | BuiltinType::Float64
        | BuiltinType::Enum(_, _) => def == arg,
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

            if global_cls_id != Some(cls_id) || tpid.to_usize() >= cls_tps.len() {
                return false;
            }

            arg_allows(
                vm,
                cls_tps[tpid.to_usize()],
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

            if global_fct_id != Some(fct_id) || tpid.to_usize() >= fct_tps.len() {
                return false;
            }

            arg_allows(
                vm,
                fct_tps[tpid.to_usize()],
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
    negate: bool,
    expected_type: BuiltinType,
) -> (BuiltinType, i64) {
    let ty = match e.suffix {
        IntSuffix::UInt8 => BuiltinType::UInt8,
        IntSuffix::Int32 => BuiltinType::Int32,
        IntSuffix::Int64 => BuiltinType::Int64,
        IntSuffix::None => match expected_type {
            BuiltinType::UInt8 => BuiltinType::UInt8,
            BuiltinType::Int32 => BuiltinType::Int32,
            BuiltinType::Int64 => BuiltinType::Int64,
            _ => BuiltinType::Int32,
        },
    };

    let ty_name = ty.name(vm);
    let value = e.value;

    if e.base == IntBase::Dec {
        let max = match ty {
            BuiltinType::UInt8 => 256,
            BuiltinType::Int32 => (1u64 << 31),
            BuiltinType::Int64 => (1u64 << 63),
            _ => unreachable!(),
        };

        if (negate && value > max) || (!negate && value >= max) {
            vm.diag
                .lock()
                .report(file, e.pos, SemError::NumberOverflow(ty_name.into()));
        }
    } else {
        assert!(!negate);

        let max = match ty {
            BuiltinType::UInt8 => 256 as u64,
            BuiltinType::Int32 => u32::max_value() as u64,
            BuiltinType::Int64 => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            vm.diag
                .lock()
                .report(file, e.pos, SemError::NumberOverflow(ty_name.into()));
        }
    }

    let value = if negate {
        (value as i64).wrapping_neg()
    } else {
        value as i64
    };

    (ty, value)
}

pub fn check_lit_float(
    vm: &VM,
    file: FileId,
    e: &ExprLitFloatType,
    negate: bool,
) -> (BuiltinType, f64) {
    let ty = match e.suffix {
        FloatSuffix::Float32 => BuiltinType::Float32,
        FloatSuffix::Float64 => BuiltinType::Float64,
    };

    let (min, max) = match e.suffix {
        FloatSuffix::Float32 => (f32::MIN as f64, f32::MAX as f64),
        FloatSuffix::Float64 => (f64::MIN, f64::MAX),
    };

    let value = if negate { -e.value } else { e.value };

    if value < min || value > max {
        let ty = match e.suffix {
            FloatSuffix::Float32 => "Float32",
            FloatSuffix::Float64 => "Float64",
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
                    ximpl.cls_id(vm)
                }

                _ => unreachable!(),
            };

            let cls_type_params = object_type.type_params(vm);

            if args_compatible(
                vm,
                &*method,
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
