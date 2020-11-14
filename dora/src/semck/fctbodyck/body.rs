use std::convert::TryFrom;
use std::sync::Arc;
use std::{f32, f64};

use crate::error::msg::SemError;
use crate::semck::fctbodyck::lookup::MethodLookup;
use crate::semck::report_term_shadow;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck::{self, ErrorReporting};
use crate::semck::{always_returns, expr_always_returns, read_type_table};
use crate::sym::{NestedSymTable, TermSym, TypeSym};
use crate::ty::{SourceType, TypeList, TypeListId};
use crate::vm::{
    self, const_accessible_from, ensure_tuple, find_field_in_class, find_methods_in_class,
    global_accessible_from, AnalysisData, CallType, ClassId, ConvInfo, EnumId, Fct, FctId,
    FctParent, FileId, ForTypeInfo, IdentType, Intrinsic, NamespaceId, Var, VarId, VM,
};

use dora_parser::ast::visit::Visitor;
use dora_parser::ast::*;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntBase, IntSuffix};

pub struct TypeCheck<'a> {
    pub vm: &'a VM,
    pub fct: &'a Fct,
    pub file_id: FileId,
    pub namespace_id: NamespaceId,
    pub analysis: &'a mut AnalysisData,
    pub ast: &'a Function,
    pub symtable: NestedSymTable<'a>,
    pub in_loop: bool,
}

impl<'a> TypeCheck<'a> {
    pub fn check(&mut self) {
        assert_eq!(self.symtable.levels(), 0);
        self.symtable.push_level();
        self.add_type_params();
        self.add_params();

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

            let return_type = self.fct.return_type.clone();
            self.check_expr(value, return_type)
        } else {
            SourceType::Unit
        };

        if !returns {
            self.check_fct_return_type(block.pos, return_type);
        }

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), 0);
    }

    fn add_type_params(&mut self) {
        let cls_type_params_count = match self.fct.parent {
            FctParent::Class(owner_class) => {
                let cls = self.vm.classes.idx(owner_class);
                let cls = cls.read();
                let mut type_param_id = 0;

                for param in &cls.type_params {
                    let sym = TypeSym::TypeParam(type_param_id.into());
                    self.symtable.insert_type(param.name, sym);
                    type_param_id += 1;
                }

                type_param_id
            }

            FctParent::Impl(_impl_id) => 0,

            FctParent::Extension(extension_id) => {
                let extension = self.vm.extensions[extension_id].read();
                let mut type_param_id = 0;

                for param in &extension.type_params {
                    let sym = TypeSym::TypeParam(type_param_id.into());
                    self.symtable.insert_type(param.name, sym);
                    type_param_id += 1;
                }

                type_param_id
            }

            FctParent::Module(_) => 0,
            FctParent::Trait(_) => 0,
            FctParent::None => 0,
        };

        if let Some(ref type_params) = self.fct.ast.type_params {
            for (tpid, tp) in type_params.iter().enumerate() {
                self.symtable.insert_type(
                    tp.name,
                    TypeSym::TypeParam((cls_type_params_count + tpid).into()),
                );
            }
        }
    }

    fn add_params(&mut self) {
        self.add_hidden_parameter_self();

        let self_count = if self.fct.has_self() { 1 } else { 0 };
        debug_assert_eq!(
            self.ast.params.len() + self_count,
            self.fct.param_types.len()
        );

        for (ind, (param, ty)) in self
            .ast
            .params
            .iter()
            .zip(self.fct.param_types.iter().skip(self_count))
            .enumerate()
        {
            // is this last argument of function with variadic arguments?
            let ty = if self.fct.variadic_arguments && ind == self.ast.params.len() - 1 {
                // type of variable is Array[T]
                self.vm.known.array_ty(self.vm, ty.clone())
            } else {
                ty.clone()
            };

            let var_ctxt = Var {
                id: VarId(0),
                name: param.name,
                reassignable: false,
                ty,
                node_id: param.id,
            };

            let var_id = self.add_var(var_ctxt);
            self.analysis.map_vars.insert(param.id, var_id);

            // params are only allowed to replace functions, vars cannot be replaced
            let term_sym = self.symtable.get_term(param.name);
            match term_sym {
                Some(TermSym::Fct(_)) | None => {
                    self.symtable.insert_term(param.name, TermSym::Var(var_id));
                }
                Some(conflict_sym) => report_term_shadow(
                    self.vm,
                    param.name,
                    self.fct.file_id,
                    param.pos,
                    conflict_sym,
                ),
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        if !self.fct.has_self() {
            return;
        }

        let ty = match self.fct.parent {
            FctParent::Class(cls_id) => {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();

                cls.ty.clone()
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
                let cls = cls.read();

                cls.ty.clone()
            }

            FctParent::Extension(extension_id) => {
                let extension = self.vm.extensions[extension_id].read();
                extension.ty.clone()
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

        assert!(self.analysis.vars.is_empty());
        self.analysis.vars.push(var);
    }

    fn add_local(&mut self, var: Var, pos: Position) -> VarId {
        let name = var.name;
        let var_id = self.add_var(var);
        match self.symtable.insert_term(name, TermSym::Var(var_id)) {
            Some(TermSym::Var(_)) | None => {}
            Some(sym) => report_term_shadow(self.vm, name, self.fct.file_id, pos, sym),
        }
        var_id
    }

    fn add_var(&mut self, mut var: Var) -> VarId {
        let var_id = VarId(self.analysis.vars.len());

        var.id = var_id;
        self.analysis.vars.push(var);

        var_id
    }

    fn check_stmt_let(&mut self, s: &StmtLetType) {
        let defined_type = if let Some(ref data_type) = s.data_type {
            self.read_type(data_type)
        } else {
            SourceType::Any
        };

        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| self.check_expr(&expr, defined_type.clone()))
            .unwrap_or(SourceType::Any);

        let defined_type = if s.data_type.is_some() {
            defined_type
        } else {
            expr_type.clone()
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
                .report(self.file_id, s.pos, SemError::VarNeedsTypeInfo(tyname));

            return;
        }

        // update type of variable, necessary when stmt has initializer expression but no type
        self.check_stmt_let_pattern(&s.pattern, defined_type.clone(), s.reassignable);

        if s.expr.is_some() {
            if !expr_type.is_error()
                && !defined_type.is_error()
                && !defined_type.allows(self.vm, expr_type.clone())
            {
                let name = self
                    .vm
                    .interner
                    .str(s.pattern.to_name().unwrap())
                    .to_string();
                let defined_type = defined_type.name_fct(self.vm, self.fct);
                let expr_type = expr_type.name_fct(self.vm, self.fct);
                let msg = SemError::AssignType(name, defined_type, expr_type);
                self.vm.diag.lock().report(self.file_id, s.pos, msg);
            }

        // let variable binding needs to be assigned
        } else {
            self.vm
                .diag
                .lock()
                .report(self.file_id, s.pos, SemError::LetMissingInitialization);
        }
    }

    fn read_type(&mut self, t: &Type) -> SourceType {
        read_type_table(self.vm, &self.symtable, self.fct.file_id, t).unwrap_or(SourceType::Error)
    }

    fn check_stmt_let_pattern(&mut self, pattern: &LetPattern, ty: SourceType, reassignable: bool) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var_ctxt = Var {
                    id: VarId(0),
                    name: ident.name,
                    reassignable: reassignable || ident.mutable,
                    ty,
                    node_id: ident.id,
                };

                let var_id = self.add_local(var_ctxt, ident.pos);
                self.analysis.map_vars.insert(ident.id, var_id);
            }

            LetPattern::Underscore(_) => {
                // nothing to do
            }

            LetPattern::Tuple(ref tuple) => {
                if !ty.is_tuple_or_unit() && !ty.is_error() {
                    let ty_name = ty.name_fct(self.vm, self.fct);
                    self.vm.diag.lock().report(
                        self.file_id,
                        tuple.pos,
                        SemError::LetPatternExpectedTuple(ty_name),
                    );
                    return;
                }

                if ty.is_unit() {
                    // () doesn't have any subparts
                    if tuple.parts.len() != 0 {
                        self.vm.diag.lock().report(
                            self.file_id,
                            tuple.pos,
                            SemError::LetPatternShouldBeUnit,
                        );
                    }
                    return;
                }

                if ty.is_error() {
                    for part in &tuple.parts {
                        self.check_stmt_let_pattern(part, SourceType::Error, reassignable);
                    }
                    return;
                }

                let tuple_id = ty.tuple_id().expect("type should be tuple");
                let parts = self.vm.tuples.lock().get(tuple_id).len();

                if parts != tuple.parts.len() {
                    let ty_name = ty.name_fct(self.vm, self.fct);
                    self.vm.diag.lock().report(
                        self.file_id,
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
                    let ty = self.vm.tuples.lock().get_ty(tuple_id, idx);
                    self.check_stmt_let_pattern(part, ty, reassignable);
                }
            }
        }
    }

    fn check_stmt_for(&mut self, stmt: &StmtForType) {
        let object_type = self.check_expr(&stmt.expr, SourceType::Any);

        if object_type.is_error() {
            self.symtable.push_level();
            self.check_stmt_let_pattern(&stmt.pattern, SourceType::Error, false);
            self.visit_stmt(&stmt.block);
            self.symtable.pop_level();
            return;
        }

        if let Some(cls_id) = object_type.cls_id(self.vm) {
            if cls_id == self.vm.known.classes.array {
                let type_list = object_type.type_params(self.vm);
                let var_ty = type_list[0].clone();

                self.symtable.push_level();
                self.check_stmt_let_pattern(&stmt.pattern, var_ty, false);
                self.check_loop_body(&stmt.block);
                self.symtable.pop_level();
                return;
            }
        }

        if let Some((for_type_info, ret_type)) =
            self.type_supports_iterator_protocol(object_type.clone())
        {
            self.symtable.push_level();
            // set variable type to return type of next
            self.check_stmt_let_pattern(&stmt.pattern, ret_type, false);
            // store fct ids for code generation
            self.analysis.map_fors.insert(stmt.id, for_type_info);
            self.check_loop_body(&stmt.block);
            self.symtable.pop_level();
            return;
        }

        if let Some((make_iterator, iterator_type)) =
            self.type_supports_make_iterator(object_type.clone())
        {
            if let Some((mut for_type_info, ret_type)) =
                self.type_supports_iterator_protocol(iterator_type)
            {
                self.symtable.push_level();
                // set variable type to return type of next
                self.check_stmt_let_pattern(&stmt.pattern, ret_type, false);

                // store fct ids for code generation
                for_type_info.make_iterator = Some(make_iterator);
                self.analysis.map_fors.insert(stmt.id, for_type_info);

                self.check_loop_body(&stmt.block);
                self.symtable.pop_level();
                return;
            }
        }

        let name = object_type.name_fct(self.vm, self.fct);
        let msg = SemError::TypeNotUsableInForIn(name);
        self.vm
            .diag
            .lock()
            .report(self.file_id, stmt.expr.pos(), msg);

        // set invalid error type
        self.symtable.push_level();
        self.check_stmt_let_pattern(&stmt.pattern, SourceType::Error, false);
        self.check_loop_body(&stmt.block);
        self.symtable.pop_level();
    }

    fn check_loop_body(&mut self, stmt: &Stmt) {
        let old_in_loop = self.in_loop;
        self.in_loop = true;
        self.visit_stmt(&stmt);
        self.in_loop = old_in_loop;
    }

    fn type_supports_make_iterator(
        &mut self,
        object_type: SourceType,
    ) -> Option<(FctId, SourceType)> {
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
        object_type: SourceType,
    ) -> Option<(ForTypeInfo, SourceType)> {
        let has_next_name = self.vm.interner.intern("hasNext");

        let mut has_next = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type.clone())
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
            .method(object_type.clone())
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
                next_type: next_type.clone(),
            },
            next_type,
        ))
    }

    fn check_stmt_while(&mut self, stmt: &StmtWhileType) {
        let expr_type = self.check_expr(&stmt.cond, SourceType::Any);

        if !expr_type.is_error() && !expr_type.is_bool() {
            let expr_type = expr_type.name_fct(self.vm, self.fct);
            let msg = SemError::WhileCondType(expr_type);
            self.vm.diag.lock().report(self.file_id, stmt.pos, msg);
        }

        self.check_loop_body(&stmt.block);
    }

    fn check_stmt_return(&mut self, s: &StmtReturnType) {
        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| self.check_expr(&expr, SourceType::Any))
            .unwrap_or(SourceType::Unit);

        self.check_fct_return_type(s.pos, expr_type);
    }

    fn check_fct_return_type(&mut self, pos: Position, expr_type: SourceType) {
        let fct_type = self.fct.return_type.clone();

        if !expr_type.is_error() && !fct_type.allows(self.vm, expr_type.clone()) {
            let fct_type = fct_type.name_fct(self.vm, self.fct);
            let expr_type = expr_type.name_fct(self.vm, self.fct);

            let msg = SemError::ReturnType(fct_type, expr_type);

            self.vm.diag.lock().report(self.file_id, pos, msg);
        }
    }

    fn check_expr_block(&mut self, block: &ExprBlockType, _expected_ty: SourceType) -> SourceType {
        self.symtable.push_level();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let ty = if let Some(ref expr) = block.expr {
            self.check_expr(expr, SourceType::Any)
        } else {
            SourceType::Unit
        };

        self.analysis.set_ty(block.id, ty.clone());
        self.symtable.pop_level();

        ty
    }

    fn check_expr_tuple(&mut self, tuple: &ExprTupleType, _expected_ty: SourceType) -> SourceType {
        let mut subtypes = Vec::new();

        if tuple.values.is_empty() {
            self.analysis.set_ty(tuple.id, SourceType::Unit);
            return SourceType::Unit;
        }

        for value in &tuple.values {
            let subtype = self.check_expr(value, SourceType::Any);
            subtypes.push(subtype);
        }

        let tuple_id = ensure_tuple(self.vm, subtypes);

        let ty = SourceType::Tuple(tuple_id);
        self.analysis.set_ty(tuple.id, ty.clone());

        ty
    }

    fn check_expr_paren(&mut self, paren: &ExprParenType, _expected_ty: SourceType) -> SourceType {
        let ty = self.check_expr(&paren.expr, SourceType::Any);
        self.analysis.set_ty(paren.id, ty.clone());

        ty
    }

    fn check_expr_if(&mut self, expr: &ExprIfType, _expected_ty: SourceType) -> SourceType {
        let expr_type = self.check_expr(&expr.cond, SourceType::Any);

        if !expr_type.is_bool() && !expr_type.is_error() {
            let expr_type = expr_type.name_fct(self.vm, self.fct);
            let msg = SemError::IfCondType(expr_type);
            self.vm.diag.lock().report(self.file_id, expr.pos, msg);
        }

        let then_type = self.check_expr(&expr.then_block, SourceType::Any);

        let merged_type = if let Some(ref else_block) = expr.else_block {
            let else_type = self.check_expr(else_block, SourceType::Any);

            if expr_always_returns(&expr.then_block) {
                else_type
            } else if expr_always_returns(else_block) {
                then_type
            } else if then_type.is_error() {
                else_type
            } else if else_type.is_error() {
                then_type
            } else if !then_type.allows(self.vm, else_type.clone()) {
                let then_type_name = then_type.name_fct(self.vm, self.fct);
                let else_type_name = else_type.name_fct(self.vm, self.fct);
                let msg = SemError::IfBranchTypesIncompatible(then_type_name, else_type_name);
                self.vm.diag.lock().report(self.file_id, expr.pos, msg);
                then_type
            } else {
                then_type
            }
        } else {
            SourceType::Unit
        };

        self.analysis.set_ty(expr.id, merged_type.clone());

        merged_type
    }

    fn check_expr_ident(&mut self, e: &ExprIdentType, expected_ty: SourceType) -> SourceType {
        let sym_term = self.symtable.get_term(e.name);
        let sym_type = self.symtable.get_type(e.name);

        match (sym_term, sym_type) {
            (Some(TermSym::Var(varid)), _) => {
                let ty = self.analysis.vars[varid].ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis.map_idents.insert(e.id, IdentType::Var(varid));

                ty
            }

            (Some(TermSym::Global(globalid)), _) => {
                let glob = self.vm.globals.idx(globalid);
                let ty = glob.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(globalid));

                ty
            }

            (Some(TermSym::Const(const_id)), _) => {
                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.read();

                self.analysis.set_ty(e.id, xconst.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                xconst.ty.clone()
            }

            (Some(TermSym::EnumValue(enum_id, variant_id)), _) => self
                .check_enum_value_without_args_id(
                    e.id,
                    e.pos,
                    expected_ty,
                    enum_id,
                    TypeList::empty(),
                    variant_id,
                ),

            (Some(TermSym::ClassConstructorAndModule(_, module_id)), _)
            | (Some(TermSym::Module(module_id)), _)
            | (Some(TermSym::StructConstructorAndModule(_, module_id)), _) => {
                let module = self.vm.modules.idx(module_id);
                let ty = module.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Module(module_id));

                ty
            }

            (None, None) => {
                let name = self.vm.interner.str(e.name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    SemError::UnknownIdentifier(name),
                );
                SourceType::Error
            }

            (_, _) => {
                self.vm
                    .diag
                    .lock()
                    .report(self.fct.file_id, e.pos, SemError::ValueExpected);
                SourceType::Error
            }
        }
    }

    fn check_expr_assign(&mut self, e: &ExprBinType) {
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
                .report(self.file_id, e.pos, SemError::LvalueExpected);
        }

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    fn check_expr_assign_ident(&mut self, e: &ExprBinType) {
        let rhs_type = self.check_expr(&e.rhs, SourceType::Any);

        self.analysis.set_ty(e.id, SourceType::Unit);

        let lhs_ident = e.lhs.to_ident().unwrap();
        let sym_term = self.symtable.get_term(lhs_ident.name);
        let sym_type = self.symtable.get_type(lhs_ident.name);

        let lhs_type = match (sym_term, sym_type) {
            (Some(TermSym::Var(varid)), _) => {
                if !self.analysis.vars[varid].reassignable {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, SemError::LetReassigned);
                }

                self.analysis
                    .map_idents
                    .insert(e.lhs.id(), IdentType::Var(varid));
                self.analysis.vars[varid].ty.clone()
            }

            (Some(TermSym::Global(global_id)), _) => {
                let glob = self.vm.globals.idx(global_id);
                let glob = glob.read();

                if !e.initializer && !glob.reassignable {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, SemError::LetReassigned);
                }

                self.analysis
                    .map_idents
                    .insert(e.lhs.id(), IdentType::Global(global_id));
                glob.ty.clone()
            }

            (None, None) => {
                let name = self.vm.interner.str(lhs_ident.name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    SemError::UnknownIdentifier(name),
                );

                return;
            }

            (_, _) => {
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    SemError::LvalueExpected,
                );

                return;
            }
        };

        if !lhs_type.is_error()
            && !rhs_type.is_error()
            && !lhs_type.allows(self.vm, rhs_type.clone())
        {
            let ident = e.lhs.to_ident().unwrap();
            let name = self.vm.interner.str(ident.name).to_string();
            let lhs_type = lhs_type.name_fct(self.vm, self.fct);
            let rhs_type = rhs_type.name_fct(self.vm, self.fct);

            self.analysis.set_ty(e.id, SourceType::Unit);

            let msg = SemError::AssignType(name, lhs_type, rhs_type);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }
    }

    fn check_expr_assign_call(&mut self, e: &ExprBinType) {
        let call = e.lhs.to_call().unwrap();
        let expr_type = self.check_expr(&call.callee, SourceType::Any);

        let mut arg_types: Vec<SourceType> = call
            .args
            .iter()
            .map(|arg| self.check_expr(arg, SourceType::Any))
            .collect();

        let value_type = self.check_expr(&e.rhs, SourceType::Any);

        let name = self.vm.interner.intern("set");
        arg_types.push(value_type);

        if let Some((_, fct_id, _)) = self.find_method(
            e.pos,
            expr_type.clone(),
            false,
            name,
            &arg_types,
            &TypeList::empty(),
        ) {
            let call_type = CallType::Expr(expr_type, fct_id);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
        }
    }

    fn check_expr_assign_field(&mut self, e: &ExprBinType) {
        let field_expr = e.lhs.to_dot().unwrap();

        let name = match field_expr.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = SemError::NameExpected;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return;
            }
        };

        let object_type = self.check_expr(&field_expr.lhs, SourceType::Any);
        let rhs_type = self.check_expr(&e.rhs, SourceType::Any);

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.vm, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let cls = self
                    .vm
                    .classes
                    .idx(cls_ty.cls_id(self.vm).expect("no class"));
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let class_type_params = cls_ty.type_params(self.vm);

                let fty = replace_type_param(self.vm, field.ty.clone(), &class_type_params, None);

                if !e.initializer && !field.reassignable {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, SemError::LetReassigned);
                }

                if !fty.allows(self.vm, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = self.vm.interner.str(name).to_string();

                    let object_type = object_type.name_fct(self.vm, self.fct);
                    let lhs_type = fty.name_fct(self.vm, self.fct);
                    let rhs_type = rhs_type.name_fct(self.vm, self.fct);

                    let msg = SemError::AssignField(name, object_type, lhs_type, rhs_type);
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, SourceType::Unit);
                return;
            }
        }

        // field not found, report error
        let field_name = self.vm.interner.str(name).to_string();
        let expr_name = object_type.name_fct(self.vm, self.fct);
        let msg = SemError::UnknownField(field_name, expr_name);
        self.vm
            .diag
            .lock()
            .report(self.file_id, field_expr.pos, msg);

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    fn find_method(
        &mut self,
        pos: Position,
        object_type: SourceType,
        is_static: bool,
        name: Name,
        args: &[SourceType],
        fct_type_params: &TypeList,
    ) -> Option<(ClassId, FctId, SourceType)> {
        let result = lookup_method(
            self.vm,
            object_type.clone(),
            is_static,
            name,
            args,
            fct_type_params,
            None,
        );

        if result.is_none() {
            let type_name = object_type.name_fct(self.vm, self.fct);
            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<String>>();
            let msg = if is_static {
                SemError::UnknownStaticMethod(type_name, name, param_names)
            } else {
                SemError::UnknownMethod(type_name, name, param_names)
            };

            self.vm.diag.lock().report(self.file_id, pos, msg);
        }

        result
    }

    fn check_expr_un(&mut self, e: &ExprUnType, _expected_ty: SourceType) -> SourceType {
        if e.op == UnOp::Neg && e.opnd.is_lit_int() {
            let expr_type =
                self.check_expr_lit_int(e.opnd.to_lit_int().unwrap(), true, SourceType::Any);
            self.analysis.set_ty(e.id, expr_type.clone());
            return expr_type;
        }

        let opnd = self.check_expr(&e.opnd, SourceType::Any);

        match e.op {
            UnOp::Plus => self.check_expr_un_method(e, e.op, "unaryPlus", opnd),
            UnOp::Neg => self.check_expr_un_method(e, e.op, "unaryMinus", opnd),
            UnOp::Not => self.check_expr_un_method(e, e.op, "not", opnd),
        }
    }

    fn check_expr_un_method(
        &mut self,
        e: &ExprUnType,
        op: UnOp,
        name: &str,
        ty: SourceType,
    ) -> SourceType {
        let name = self.vm.interner.intern(name);
        let call_types = [];

        if !ty.is_error() {
            if let Some((_, fct_id, return_type)) = lookup_method(
                self.vm,
                ty.clone(),
                false,
                name,
                &call_types,
                &TypeList::empty(),
                None,
            ) {
                let call_type = CallType::Method(ty.clone(), fct_id, TypeList::empty());
                self.analysis.map_calls.insert(e.id, Arc::new(call_type));

                self.analysis.set_ty(e.id, return_type.clone());
                return return_type;
            }

            let ty = ty.name_fct(self.vm, self.fct);
            let msg = SemError::UnOpType(op.as_str().into(), ty);

            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_bin(&mut self, e: &ExprBinType, _expected_ty: SourceType) -> SourceType {
        if e.op.is_any_assign() {
            self.check_expr_assign(e);
            return SourceType::Unit;
        }

        let lhs_type = self.check_expr(&e.lhs, SourceType::Any);
        let rhs_type = self.check_expr(&e.rhs, SourceType::Any);

        if lhs_type.is_error() || rhs_type.is_error() {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
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
        e: &ExprBinType,
        op: BinOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        self.check_type(e, op, lhs_type, rhs_type, SourceType::Bool);
        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    fn check_expr_bin_method(
        &mut self,
        e: &ExprBinType,
        op: BinOp,
        name: &str,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        let name = self.vm.interner.intern(name);
        let call_types = [rhs_type.clone()];

        if let Some((_, fct_id, return_type)) = lookup_method(
            self.vm,
            lhs_type.clone(),
            false,
            name,
            &call_types,
            &TypeList::empty(),
            None,
        ) {
            let call_type = CallType::Method(lhs_type, fct_id, TypeList::empty());
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.analysis.set_ty(e.id, return_type.clone());

            return_type
        } else {
            let lhs_type = lhs_type.name_fct(self.vm, self.fct);
            let rhs_type = rhs_type.name_fct(self.vm, self.fct);
            let msg = SemError::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_bin_cmp(
        &mut self,
        e: &ExprBinType,
        cmp: CmpOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        match cmp {
            CmpOp::Is | CmpOp::IsNot => {
                if !lhs_type.allows(self.vm, rhs_type.clone())
                    && !rhs_type.allows(self.vm, lhs_type.clone())
                {
                    let lhs_type = lhs_type.name_fct(self.vm, self.fct);
                    let rhs_type = rhs_type.name_fct(self.vm, self.fct);
                    self.vm.diag.lock().report(
                        self.file_id,
                        e.pos,
                        SemError::TypesIncompatible(lhs_type, rhs_type),
                    );
                }

                self.analysis.set_ty(e.id, SourceType::Bool);
                return SourceType::Bool;
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

        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    fn check_expr_cmp_enum(
        &mut self,
        e: &ExprBinType,
        op: CmpOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) {
        if lhs_type.allows(self.vm, rhs_type.clone()) {
            let intrinsic = match op {
                CmpOp::Eq => Intrinsic::EnumEq,
                CmpOp::Ne => Intrinsic::EnumNe,
                _ => unreachable!(),
            };
            let call_type = CallType::Intrinsic(intrinsic);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.analysis.set_ty(e.id, SourceType::Bool);
        } else {
            let lhs_type = lhs_type.name_fct(self.vm, self.fct);
            let rhs_type = rhs_type.name_fct(self.vm, self.fct);
            let msg = SemError::BinOpType("equals".into(), lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
        }
    }

    fn check_type(
        &mut self,
        e: &ExprBinType,
        op: BinOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
        expected_type: SourceType,
    ) {
        if !expected_type.allows(self.vm, lhs_type.clone())
            || !expected_type.allows(self.vm, rhs_type.clone())
        {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name_fct(self.vm, self.fct);
            let rhs_type = rhs_type.name_fct(self.vm, self.fct);
            let msg = SemError::BinOpType(op, lhs_type, rhs_type);

            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }
    }

    fn check_expr_call(&mut self, e: &ExprCallType, _expected_ty: SourceType) -> SourceType {
        let (callee, type_params) = if let Some(expr_type_params) = e.callee.to_type_param() {
            let type_params: Vec<SourceType> = expr_type_params
                .args
                .iter()
                .map(|p| self.read_type(p))
                .collect();
            let type_params: TypeList = TypeList::with(type_params);
            (&expr_type_params.callee, type_params)
        } else {
            (&e.callee, TypeList::empty())
        };

        let arg_types: Vec<SourceType> = e
            .args
            .iter()
            .map(|arg| self.check_expr(arg, SourceType::Any))
            .collect();

        if let Some(expr_ident) = callee.to_ident() {
            let sym_term = self.symtable.get_term(expr_ident.name);
            let sym_type = self.symtable.get_type(expr_ident.name);

            self.check_expr_call_sym(e, callee, sym_term, sym_type, type_params, &arg_types)
        } else if let Some(expr_dot) = callee.to_dot() {
            let object_type = if expr_dot.lhs.is_super() {
                self.super_type(expr_dot.lhs.pos())
            } else {
                self.check_expr(&expr_dot.lhs, SourceType::Any)
            };

            let method_name = match expr_dot.rhs.to_ident() {
                Some(ident) => ident.name,

                None => {
                    let msg = SemError::NameExpected;
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    return SourceType::Error;
                }
            };
            self.check_expr_call_method(e, object_type, method_name, type_params, &arg_types)
        } else if let Some(_expr_path) = callee.to_path() {
            self.check_expr_call_path(e, callee, type_params, &arg_types)
        } else {
            if !type_params.is_empty() {
                let msg = SemError::NoTypeParamsExpected;
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, e.callee.pos(), msg);
            }

            let expr_type = self.check_expr(callee, SourceType::Any);
            self.check_expr_call_expr(e, expr_type, &arg_types)
        }
    }

    fn check_expr_call_sym(
        &mut self,
        e: &ExprCallType,
        callee: &Expr,
        sym_term: Option<TermSym>,
        sym_type: Option<TypeSym>,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        match (sym_term, sym_type) {
            (Some(TermSym::Fct(fct_id)), _) => {
                self.check_expr_call_fct(e, fct_id, type_params, &arg_types)
            }

            (Some(TermSym::ClassConstructor(cls_id)), _)
            | (Some(TermSym::ClassConstructorAndModule(cls_id, _)), _) => {
                self.check_expr_call_ctor(e, cls_id, type_params, &arg_types)
            }

            (Some(TermSym::EnumValue(enum_id, variant_id)), _) => {
                self.check_enum_value_with_args(e, enum_id, type_params, variant_id, &arg_types)
            }

            (_, _) => {
                if !type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.callee.pos(), msg);
                }

                let expr_type = self.check_expr(callee, SourceType::Any);
                self.check_expr_call_expr(e, expr_type, &arg_types)
            }
        }
    }

    fn check_enum_value_with_args(
        &mut self,
        e: &ExprCallType,
        enum_id: EnumId,
        type_params: TypeList,
        variant_id: usize,
        arg_types: &[SourceType],
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();
        let variant = &xenum.variants[variant_id as usize];

        let list_id = self.vm.lists.lock().insert(type_params.clone());
        let ty = SourceType::Enum(enum_id, list_id);
        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            ty.clone(),
            ErrorReporting::Yes(self.file_id, e.pos),
        );

        if !self.check_expr_call_enum_args(enum_id, type_params.clone(), variant, arg_types) {
            let enum_name = self.vm.interner.str(xenum.name).to_string();
            let variant_name = self.vm.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name_enum(self.vm, &*xenum))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let msg =
                SemError::EnumArgsIncompatible(enum_name, variant_name, variant_types, arg_types);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        } else if variant.types.is_empty() {
            let enum_name = self.vm.interner.str(xenum.name).to_string();
            let variant_name = self.vm.interner.str(variant.name).to_string();
            let msg = SemError::EnumArgsNoParens(enum_name, variant_name);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        if type_params_ok {
            self.analysis
                .map_calls
                .insert(e.id, Arc::new(CallType::Enum(ty.clone(), variant_id)));

            self.analysis.set_ty(e.id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(e.id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_call_enum_args(
        &mut self,
        _enum_id: EnumId,
        type_params: TypeList,
        variant: &vm::EnumVariant,
        arg_types: &[SourceType],
    ) -> bool {
        if variant.types.len() != arg_types.len() {
            return false;
        }

        for (def_ty, arg_ty) in variant.types.iter().zip(arg_types) {
            let def_ty = replace_type_param(self.vm, def_ty.clone(), &type_params, None);

            if !def_ty.allows(self.vm, arg_ty.clone()) {
                return false;
            }
        }

        true
    }

    fn check_expr_call_generic_static_method(
        &mut self,
        e: &ExprCallType,
        tp: SourceType,
        name: Name,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut fcts = Vec::new();

        let (type_param, tp_id) = self
            .fct
            .type_param_ty(self.vm, tp.clone(), |tp, tp_id| (tp.clone(), tp_id));

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

            self.vm.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        if arg_types.contains(&SourceType::Error) {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let (trait_id, fct_id) = fcts[0];
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        if !args_compatible(
            self.vm,
            &*fct,
            arg_types,
            &TypeList::empty(),
            Some(tp.clone()),
        ) {
            let fct_name = self.vm.interner.str(name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, arg_types);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        let call_type = CallType::GenericStaticMethod(tp_id, trait_id, fct_id);
        self.analysis.map_calls.insert(e.id, Arc::new(call_type));

        let return_type = replace_type_param(
            self.vm,
            fct.return_type.clone(),
            &TypeList::empty(),
            Some(tp),
        );

        self.analysis.set_ty(e.id, return_type.clone());

        return_type
    }

    fn check_expr_call_expr(
        &mut self,
        e: &ExprCallType,
        expr_type: SourceType,
        arg_types: &[SourceType],
    ) -> SourceType {
        if expr_type.is_error() {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let get = self.vm.interner.intern("get");

        if let Some((_, fct_id, return_type)) = self.find_method(
            e.pos,
            expr_type.clone(),
            false,
            get,
            arg_types,
            &TypeList::empty(),
        ) {
            let call_type = CallType::Expr(expr_type.clone(), fct_id);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.analysis.set_ty(e.id, return_type.clone());

            return_type
        } else {
            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_fct(
        &mut self,
        e: &ExprCallType,
        fct_id: FctId,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .pos(e.pos)
            .callee(fct_id)
            .args(&arg_types)
            .fct_type_params(&type_params);

        let ty = if lookup.find() {
            let call_type = CallType::Fct(fct_id, TypeList::empty(), type_params.clone());
            self.analysis.map_calls.insert(e.id, Arc::new(call_type));

            lookup.found_ret().unwrap()
        } else {
            SourceType::Error
        };

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_call_static_method(
        &mut self,
        e: &ExprCallType,
        object_type: SourceType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        assert_eq!(object_type.type_params(self.vm).len(), 0);

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
            self.analysis.map_calls.insert(e.id, call_type.clone());

            self.analysis.set_ty(e.id, return_type.clone());

            return_type
        } else {
            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_method(
        &mut self,
        e: &ExprCallType,
        object_type: SourceType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        if object_type.is_type_param() {
            assert_eq!(type_params.len(), 0);
            return self.check_expr_call_generic(e, object_type, method_name, arg_types);
        }

        if object_type.is_error() {
            self.analysis.set_ty(e.id, SourceType::Error);

            return SourceType::Error;
        }

        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type.clone())
            .name(method_name)
            .fct_type_params(&type_params)
            .args(arg_types);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();

            let call_type = if let SourceType::TraitObject(trait_id) = object_type {
                CallType::TraitObjectMethod(trait_id, fct_id)
            } else {
                let method_type = lookup.found_class_type().unwrap();
                if method_type.is_module() {
                    CallType::ModuleMethod(method_type, fct_id, type_params.clone())
                } else {
                    CallType::Method(method_type, fct_id, type_params.clone())
                }
            };

            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
            self.analysis.set_ty(e.id, return_type.clone());

            return_type
        } else if lookup.found_fct_id().is_none() {
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

            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_field(
        &mut self,
        e: &ExprCallType,
        object_type: SourceType,
        method_name: Name,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        if let Some((actual_type, field_id, field_type)) =
            find_field_in_class(self.vm, object_type.clone(), method_name)
        {
            self.analysis.set_ty(e.callee.id(), field_type.clone());
            self.analysis
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

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_call_ctor(
        &mut self,
        e: &ExprCallType,
        cls_id: ClassId,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .pos(e.pos)
            .ctor(cls_id)
            .args(arg_types)
            .container_type_params(&type_params);

        let ty = if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();

            let cls_ty = self.vm.cls_with_type_list(cls_id, type_params.clone());
            let call_type = CallType::Ctor(cls_ty, fct_id);
            self.analysis.map_calls.insert(e.id, Arc::new(call_type));

            if cls.is_abstract {
                let msg = SemError::NewAbstractClass;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);
            }

            lookup.found_ret().unwrap()
        } else {
            SourceType::Error
        };

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_call_generic(
        &mut self,
        e: &ExprCallType,
        object_type: SourceType,
        name: Name,
        arg_types: &[SourceType],
    ) -> SourceType {
        self.fct
            .type_param_ty(self.vm, object_type.clone(), |tp, id| {
                self.check_expr_call_generic_type_param(e, object_type, id, tp, name, arg_types)
            })
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &ExprCallType,
        object_type: SourceType,
        _id: TypeListId,
        tp: &vm::TypeParam,
        name: Name,
        args: &[SourceType],
    ) -> SourceType {
        let mut found_fcts = Vec::new();

        for &trait_id in &tp.trait_bounds {
            let trai = self.vm.traits[trait_id].read();

            if let Some(fid) = trai.find_method_with_replace(self.vm, false, name, None, args) {
                found_fcts.push(fid);
            }
        }

        if found_fcts.len() == 1 {
            let fid = found_fcts[0];

            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();
            let return_type = fct.return_type.clone();

            self.analysis.set_ty(e.id, return_type.clone());

            let call_type = CallType::GenericMethod(_id, fct.trait_id(), fid);
            self.analysis.map_calls.insert(e.id, Arc::new(call_type));

            return_type
        } else {
            let type_name = object_type.name_fct(self.vm, self.fct);
            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<String>>();
            let msg = if found_fcts.len() == 0 {
                SemError::UnknownMethodForTypeParam(type_name, name, param_names)
            } else {
                SemError::MultipleCandidatesForTypeParam(type_name, name, param_names)
            };

            self.vm.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_path(
        &mut self,
        e: &ExprCallType,
        callee: &Expr,
        type_params: TypeList,
        arg_types: &[SourceType],
    ) -> SourceType {
        let callee_as_path = callee.to_path().unwrap();

        let (container_expr, container_type_params) =
            if let Some(expr_type_params) = callee_as_path.lhs.to_type_param() {
                let container_type_params: Vec<SourceType> = expr_type_params
                    .args
                    .iter()
                    .map(|p| self.read_type(p))
                    .collect();
                let container_type_params: TypeList = TypeList::with(container_type_params);

                (&expr_type_params.callee, container_type_params)
            } else {
                (&callee_as_path.lhs, TypeList::empty())
            };
        let method_expr = &callee_as_path.rhs;

        let (sym_term, sym_type) = match self.read_path(container_expr) {
            Ok((sym_term, sym_type)) => (sym_term, sym_type),
            Err(()) => {
                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let method_name = if let Some(method_name_expr) = method_expr.to_ident() {
            method_name_expr.name
        } else {
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm
                .diag
                .lock()
                .report(self.file_id, method_expr.pos(), msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        };

        match (sym_type, sym_term) {
            (_, Some(TermSym::ClassConstructorAndModule(_, module_id)))
            | (_, Some(TermSym::Module(module_id))) => {
                if !container_type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let module_ty = SourceType::Module(module_id);
                self.check_expr_call_method(e, module_ty, method_name, type_params, &arg_types)
            }

            (Some(TypeSym::Class(cls_id)), _) => {
                let list_id = self.vm.lists.lock().insert(container_type_params);
                self.check_expr_call_static_method(
                    e,
                    SourceType::Class(cls_id, list_id),
                    method_name,
                    type_params,
                    &arg_types,
                )
            }

            (Some(TypeSym::Enum(enum_id)), _) => {
                let xenum = self.vm.enums[enum_id].read();

                if !container_type_params.is_empty() && !type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let used_type_params = if type_params.is_empty() {
                    container_type_params
                } else {
                    type_params
                };

                if let Some(&variant_id) = xenum.name_to_value.get(&method_name) {
                    self.check_enum_value_with_args(
                        e,
                        enum_id,
                        used_type_params,
                        variant_id as usize,
                        &arg_types,
                    )
                } else {
                    let name = self.vm.interner.str(method_name).to_string();
                    self.vm.diag.lock().report(
                        self.file_id,
                        e.pos,
                        SemError::UnknownEnumValue(name),
                    );

                    SourceType::Error
                }
            }

            (Some(TypeSym::TypeParam(id)), _) => {
                if !container_type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let ty = SourceType::TypeParam(id);
                self.check_expr_call_generic_static_method(e, ty, method_name, &arg_types)
            }

            (_, Some(TermSym::Namespace(namespace_id))) => {
                if !container_type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let (sym_term, sym_type) = {
                    let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                    let table = namespace.table.read();

                    (table.get_term(method_name), table.get_type(method_name))
                };

                self.check_expr_call_sym(e, callee, sym_term, sym_type, type_params, arg_types)
            }

            (_, _) => {
                let msg = SemError::ClassExpected;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);

                SourceType::Error
            }
        }
    }

    fn check_expr_delegation(
        &mut self,
        e: &ExprDelegationType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let arg_types: Vec<SourceType> = e
            .args
            .iter()
            .map(|arg| self.check_expr(arg, SourceType::Any))
            .collect();

        let owner = self.vm.classes.idx(self.fct.cls_id());
        let owner = owner.read();

        let parent_class = owner.parent_class.clone().unwrap();
        let cls_id = parent_class.cls_id(self.vm).expect("no class");
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        if let Some(ctor_id) = cls.constructor {
            let ctor = self.vm.fcts.idx(ctor_id);
            let ctor = ctor.read();

            let parent_class_type_params = parent_class.type_params(self.vm);

            if args_compatible(self.vm, &*ctor, &arg_types, &parent_class_type_params, None) {
                self.analysis.map_tys.insert(e.id, parent_class);

                let cls_ty = self.vm.cls_with_type_list(cls_id, parent_class_type_params);
                let call_type = CallType::CtorParent(cls_ty, ctor.id);
                self.analysis.map_calls.insert(e.id, Arc::new(call_type));
                return SourceType::Error;
            }
        }

        let name = self.vm.interner.str(cls.name).to_string();
        let arg_types = arg_types
            .iter()
            .map(|t| t.name_fct(self.vm, self.fct))
            .collect();
        let msg = SemError::UnknownCtor(name, arg_types);
        self.vm.diag.lock().report(self.file_id, e.pos, msg);

        SourceType::Error
    }

    fn super_type(&self, pos: Position) -> SourceType {
        if let FctParent::Class(clsid) = self.fct.parent {
            let cls = self.vm.classes.idx(clsid);
            let cls = cls.read();

            if let Some(parent_class) = cls.parent_class.clone() {
                return parent_class;
            }
        }

        let msg = SemError::SuperUnavailable;
        self.vm.diag.lock().report(self.file_id, pos, msg);

        SourceType::Error
    }

    fn check_expr_path(&mut self, e: &ExprPathType, expected_ty: SourceType) -> SourceType {
        let (container_expr, type_params) = if let Some(expr_type_params) = e.lhs.to_type_param() {
            let type_params: Vec<SourceType> = expr_type_params
                .args
                .iter()
                .map(|p| self.read_type(p))
                .collect();
            let type_params: TypeList = TypeList::with(type_params);

            (&expr_type_params.callee, type_params)
        } else {
            (&e.lhs, TypeList::empty())
        };

        let (sym_term, sym_type) = match self.read_path(container_expr) {
            Ok((sym_term, sym_type)) => (sym_term, sym_type),
            Err(()) => {
                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let element_name = if let Some(ident) = e.rhs.to_ident() {
            ident.name
        } else {
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm.diag.lock().report(self.file_id, e.rhs.pos(), msg);
            return SourceType::Error;
        };

        match (sym_term, sym_type) {
            (_, Some(TypeSym::Enum(id))) => self.check_enum_value_without_args(
                e.id,
                e.pos,
                expected_ty,
                id,
                type_params,
                element_name,
            ),

            (Some(TermSym::Namespace(namespace_id)), _) => {
                self.check_expr_path_namespace(e, expected_ty, namespace_id, element_name)
            }

            _ => {
                let msg = SemError::InvalidLeftSideOfSeparator;
                self.vm.diag.lock().report(self.file_id, e.lhs.pos(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                SourceType::Error
            }
        }
    }

    fn read_path(&mut self, expr: &Expr) -> Result<(Option<TermSym>, Option<TypeSym>), ()> {
        if let Some(expr_path) = expr.to_path() {
            let (sym_term, sym_type) = self.read_path(&expr_path.lhs)?;

            let element_name = if let Some(ident) = expr_path.rhs.to_ident() {
                ident.name
            } else {
                let msg = SemError::ExpectedSomeIdentifier;
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, expr_path.rhs.pos(), msg);
                return Err(());
            };

            match (sym_term, sym_type) {
                (Some(TermSym::Namespace(namespace_id)), _) => {
                    let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();
                    let sym_term = symtable.get_term(element_name);
                    let sym_type = symtable.get_type(element_name);

                    Ok((sym_term, sym_type))
                }

                _ => {
                    let msg = SemError::ExpectedNamespace;
                    self.vm.diag.lock().report(self.file_id, expr.pos(), msg);
                    Err(())
                }
            }
        } else if let Some(expr_ident) = expr.to_ident() {
            let container_name = expr_ident.name;
            let sym_term = self.symtable.get_term(container_name);
            let sym_type = self.symtable.get_type(container_name);

            Ok((sym_term, sym_type))
        } else {
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm.diag.lock().report(self.file_id, expr.pos(), msg);
            Err(())
        }
    }

    fn check_expr_path_namespace(
        &mut self,
        e: &ExprPathType,
        expected_ty: SourceType,
        namespace_id: NamespaceId,
        element_name: Name,
    ) -> SourceType {
        let namespace = &self.vm.namespaces[namespace_id.to_usize()];
        let table = namespace.table.read();

        let sym_term = table.get_term(element_name);
        let sym_type = table.get_type(element_name);

        match (sym_term, sym_type) {
            (Some(TermSym::Global(global_id)), _) => {
                if !global_accessible_from(self.vm, global_id, self.namespace_id) {
                    let global = &self.vm.globals.idx(global_id);
                    let global = global.read();
                    let msg = SemError::NotAccessible(global.name(self.vm));
                    self.vm.diag.lock().report(self.file_id, self.ast.pos, msg);
                }

                let glob = self.vm.globals.idx(global_id);
                let ty = glob.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(global_id));

                ty
            }

            (Some(TermSym::Const(const_id)), _) => {
                if !const_accessible_from(self.vm, const_id, self.namespace_id) {
                    let xconst = self.vm.consts.idx(const_id);
                    let xconst = xconst.read();
                    let msg = SemError::NotAccessible(xconst.name(self.vm));
                    self.vm.diag.lock().report(self.file_id, self.ast.pos, msg);
                }

                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.read();

                self.analysis.set_ty(e.id, xconst.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                xconst.ty.clone()
            }

            (Some(TermSym::EnumValue(enum_id, variant_id)), _) => self
                .check_enum_value_without_args_id(
                    e.id,
                    e.pos,
                    expected_ty,
                    enum_id,
                    TypeList::empty(),
                    variant_id,
                ),

            (None, None) => {
                let namespace = namespace.name(self.vm);
                let name = self.vm.interner.str(element_name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    SemError::UnknownIdentifierInNamespace(namespace, name),
                );
                SourceType::Error
            }

            (_, _) => {
                self.vm
                    .diag
                    .lock()
                    .report(self.fct.file_id, e.pos, SemError::ValueExpected);
                SourceType::Error
            }
        }
    }

    fn check_enum_value_without_args(
        &mut self,
        expr_id: NodeId,
        expr_pos: Position,
        _expected_ty: SourceType,
        enum_id: EnumId,
        type_params: TypeList,
        name: Name,
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();

        let list_id = self.vm.lists.lock().insert(type_params.clone());
        let ty = SourceType::Enum(enum_id, list_id);
        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            ty.clone(),
            ErrorReporting::Yes(self.file_id, expr_pos),
        );

        if let Some(&value) = xenum.name_to_value.get(&name) {
            let variant = &xenum.variants[value as usize];

            if !variant.types.is_empty() {
                let enum_name = self.vm.interner.str(xenum.name).to_string();
                let variant_name = self.vm.interner.str(variant.name).to_string();
                let variant_types = variant
                    .types
                    .iter()
                    .map(|a| a.name_fct(self.vm, self.fct))
                    .collect::<Vec<_>>();
                let arg_types = Vec::new();
                let msg = SemError::EnumArgsIncompatible(
                    enum_name,
                    variant_name,
                    variant_types,
                    arg_types,
                );
                self.vm.diag.lock().report(self.file_id, expr_pos, msg);
            }

            self.analysis.map_idents.insert(
                expr_id,
                IdentType::EnumValue(enum_id, type_params, value as usize),
            );
        } else {
            let name = self.vm.interner.str(name).to_string();
            self.vm
                .diag
                .lock()
                .report(self.file_id, expr_pos, SemError::UnknownEnumValue(name));
        }

        if type_params_ok {
            self.analysis.set_ty(expr_id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(expr_id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_type_param(
        &mut self,
        e: &ExprTypeParamType,
        expected_ty: SourceType,
    ) -> SourceType {
        let type_params: Vec<SourceType> = e.args.iter().map(|p| self.read_type(p)).collect();
        let type_params: TypeList = TypeList::with(type_params);

        if let Some(ident) = e.callee.to_ident() {
            let method_name = ident.name;

            let sym_term = self.symtable.get_term(method_name);

            match sym_term {
                Some(TermSym::EnumValue(enum_id, variant_id)) => self
                    .check_enum_value_without_args_id(
                        e.id,
                        e.pos,
                        expected_ty,
                        enum_id,
                        type_params,
                        variant_id,
                    ),

                _ => {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, SemError::NoTypeParamsExpected);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else if let Some(path) = e.callee.to_path() {
            let container_name = if let Some(container_expr) = path.lhs.to_ident() {
                container_expr.name
            } else {
                let msg = SemError::ExpectedSomeIdentifier;
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, path.lhs.pos(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            };

            let method_name = if let Some(ident) = path.rhs.to_ident() {
                ident.name
            } else {
                let msg = SemError::ExpectedSomeIdentifier;
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, path.rhs.pos(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            };

            let sym_type = self.symtable.get_type(container_name);

            match sym_type {
                Some(TypeSym::Enum(enum_id)) => self.check_enum_value_without_args(
                    e.id,
                    e.pos,
                    expected_ty,
                    enum_id,
                    type_params,
                    method_name,
                ),

                _ => {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else {
            self.vm
                .diag
                .lock()
                .report(self.file_id, e.pos, SemError::NoTypeParamsExpected);
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    }

    fn check_enum_value_without_args_id(
        &mut self,
        expr_id: NodeId,
        expr_pos: Position,
        _expected_ty: SourceType,
        enum_id: EnumId,
        type_params: TypeList,
        variant_id: usize,
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();

        let list_id = self.vm.lists.lock().insert(type_params.clone());
        let ty = SourceType::Enum(enum_id, list_id);
        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            ty.clone(),
            ErrorReporting::Yes(self.file_id, expr_pos),
        );

        let variant = &xenum.variants[variant_id];

        if !variant.types.is_empty() {
            let enum_name = self.vm.interner.str(xenum.name).to_string();
            let variant_name = self.vm.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let arg_types = Vec::new();
            let msg =
                SemError::EnumArgsIncompatible(enum_name, variant_name, variant_types, arg_types);
            self.vm.diag.lock().report(self.file_id, expr_pos, msg);
        }

        self.analysis.map_idents.insert(
            expr_id,
            IdentType::EnumValue(enum_id, type_params, variant_id),
        );

        if type_params_ok {
            self.analysis.set_ty(expr_id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(expr_id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_dot(&mut self, e: &ExprDotType, _expected_ty: SourceType) -> SourceType {
        let object_type = if e.lhs.is_super() {
            self.super_type(e.lhs.pos())
        } else {
            self.check_expr(&e.lhs, SourceType::Any)
        };

        if object_type.is_tuple() {
            return self.check_expr_dot_tuple(e, object_type);
        }

        let name = match e.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = SemError::NameExpected;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        if object_type.cls_id(self.vm).is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.vm, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let cls = self
                    .vm
                    .classes
                    .idx(cls_ty.cls_id(self.vm).expect("no class"));
                let cls = cls.read();

                let field = &cls.fields[field_id];
                let class_type_params = cls_ty.type_params(self.vm);
                let fty = replace_type_param(self.vm, field.ty.clone(), &class_type_params, None);

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        // field not found, report error
        if !object_type.is_error() {
            let field_name = self.vm.interner.str(name).to_string();
            let expr_name = object_type.name_fct(self.vm, self.fct);
            let msg = SemError::UnknownField(field_name, expr_name);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_dot_tuple(&mut self, e: &ExprDotType, object_type: SourceType) -> SourceType {
        let index = match e.rhs.to_lit_int() {
            Some(ident) => ident.value,

            None => {
                let msg = SemError::IndexExpected;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let tuple_id = match object_type {
            SourceType::Tuple(tuple_id) => tuple_id,
            _ => unreachable!(),
        };

        let tuple = self.vm.tuples.lock().get(tuple_id);

        if index >= tuple.len() as u64 {
            let msg = SemError::IllegalTupleIndex(index, object_type.name_fct(self.vm, self.fct));
            self.vm.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let ty = tuple[usize::try_from(index).unwrap()].clone();
        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_this(&mut self, e: &ExprSelfType, _expected_ty: SourceType) -> SourceType {
        match self.fct.parent {
            FctParent::Class(clsid) => {
                let cls = self.vm.classes.idx(clsid);
                let cls = cls.read();
                let ty = cls.ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                ty
            }

            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
                let cls = cls.read();
                let ty = cls.ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                ty
            }

            FctParent::Extension(extension_id) => {
                let extension = self.vm.extensions[extension_id].read();
                let ty = extension.ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                ty
            }

            _ => {
                let msg = SemError::ThisUnavailable;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);
                self.analysis.set_ty(e.id, SourceType::Unit);

                SourceType::Unit
            }
        }
    }

    fn check_expr_super(&mut self, e: &ExprSuperType, _expected_ty: SourceType) -> SourceType {
        let msg = SemError::SuperNeedsMethodCall;
        self.vm.diag.lock().report(self.file_id, e.pos, msg);
        self.analysis.set_ty(e.id, SourceType::Unit);

        SourceType::Unit
    }

    fn check_expr_lambda(&mut self, e: &ExprLambdaType, _expected_ty: SourceType) -> SourceType {
        let ret = if let Some(ref ret_type) = e.ret {
            self.read_type(ret_type)
        } else {
            SourceType::Unit
        };

        let params = e
            .params
            .iter()
            .map(|p| self.read_type(&p.data_type))
            .collect::<Vec<_>>();

        let ty = self.vm.lambda_types.lock().insert(params, ret);
        let ty = SourceType::Lambda(ty);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_conv(&mut self, e: &ExprConvType, _expected_ty: SourceType) -> SourceType {
        let object_type = self.check_expr(&e.object, SourceType::Any);
        self.analysis.set_ty(e.object.id(), object_type.clone());

        let check_type = self.read_type(&e.data_type);

        if !check_type.is_error() && !check_type.is_cls() {
            let name = check_type.name_fct(self.vm, self.fct);
            self.vm
                .diag
                .lock()
                .report(self.file_id, e.pos, SemError::ReferenceTypeExpected(name));
            let ty = if e.is {
                SourceType::Bool
            } else {
                SourceType::Error
            };
            self.analysis.set_ty(e.id, ty.clone());
            return ty;
        }

        let mut valid = false;

        if object_type.subclass_from(self.vm, check_type.clone()) {
            // open class A { } class B extends A { }
            // (b is A) is valid

            valid = true;
        } else if check_type.subclass_from(self.vm, object_type.clone()) {
            // normal check
        } else {
            let object_type = object_type.name_fct(self.vm, self.fct);
            let check_type = check_type.name_fct(self.vm, self.fct);
            let msg = SemError::TypesIncompatible(object_type, check_type);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis.map_convs.insert(
            e.id,
            ConvInfo {
                check_type: check_type.clone(),
                valid,
            },
        );

        let ty = if e.is { SourceType::Bool } else { check_type };

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_int(
        &mut self,
        e: &ExprLitIntType,
        negate: bool,
        expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_int(self.vm, self.file_id, e, negate, expected_ty);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_float(
        &mut self,
        e: &ExprLitFloatType,
        negate: bool,
        _expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_float(self.vm, self.file_id, e, negate);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_str(&mut self, e: &ExprLitStrType, _expected_ty: SourceType) -> SourceType {
        let str_ty = self.vm.cls(self.vm.known.classes.string);
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    fn check_expr_lit_bool(&mut self, e: &ExprLitBoolType, _expected_ty: SourceType) -> SourceType {
        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    fn check_expr_lit_char(&mut self, e: &ExprLitCharType, _expected_ty: SourceType) -> SourceType {
        self.analysis.set_ty(e.id, SourceType::Char);

        SourceType::Char
    }

    fn check_expr_template(
        &mut self,
        e: &ExprTemplateType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let stringable_trait = self.vm.known.traits.stringable;

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = self.check_expr(part, SourceType::Any);

                let implements_stringable = if part_expr.is_type_param() {
                    self.fct.type_param_ty(self.vm, part_expr.clone(), |tp, _| {
                        tp.trait_bounds.contains(&stringable_trait)
                    })
                } else {
                    part_expr.implements_trait(self.vm, stringable_trait)
                };

                if implements_stringable || part_expr.is_error() {
                    continue;
                }

                let ty = part_expr.name_fct(self.vm, self.fct);
                self.vm.diag.lock().report(
                    self.file_id,
                    part.pos(),
                    SemError::ExpectedStringable(ty),
                );
            } else {
                assert!(part.is_lit_str());
            }
        }

        let str_ty = self.vm.cls(self.vm.known.classes.string);
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    fn check_expr(&mut self, e: &Expr, expected_ty: SourceType) -> SourceType {
        match *e {
            Expr::LitChar(ref expr) => self.check_expr_lit_char(expr, expected_ty),
            Expr::LitInt(ref expr) => self.check_expr_lit_int(expr, false, expected_ty),
            Expr::LitFloat(ref expr) => self.check_expr_lit_float(expr, false, expected_ty),
            Expr::LitStr(ref expr) => self.check_expr_lit_str(expr, expected_ty),
            Expr::Template(ref expr) => self.check_expr_template(expr, expected_ty),
            Expr::LitBool(ref expr) => self.check_expr_lit_bool(expr, expected_ty),
            Expr::Ident(ref expr) => self.check_expr_ident(expr, expected_ty),
            Expr::Un(ref expr) => self.check_expr_un(expr, expected_ty),
            Expr::Bin(ref expr) => self.check_expr_bin(expr, expected_ty),
            Expr::Call(ref expr) => self.check_expr_call(expr, expected_ty),
            Expr::TypeParam(ref expr) => self.check_expr_type_param(expr, expected_ty),
            Expr::Path(ref expr) => self.check_expr_path(expr, expected_ty),
            Expr::Delegation(ref expr) => self.check_expr_delegation(expr, expected_ty),
            Expr::Dot(ref expr) => self.check_expr_dot(expr, expected_ty),
            Expr::This(ref expr) => self.check_expr_this(expr, expected_ty),
            Expr::Super(ref expr) => self.check_expr_super(expr, expected_ty),
            Expr::Conv(ref expr) => self.check_expr_conv(expr, expected_ty),
            Expr::Lambda(ref expr) => self.check_expr_lambda(expr, expected_ty),
            Expr::Block(ref expr) => self.check_expr_block(expr, expected_ty),
            Expr::If(ref expr) => self.check_expr_if(expr, expected_ty),
            Expr::Tuple(ref expr) => self.check_expr_tuple(expr, expected_ty),
            Expr::Paren(ref expr) => self.check_expr_paren(expr, expected_ty),
            Expr::Match(_) => unimplemented!(),
        }
    }

    fn check_stmt_break_and_continue(&mut self, stmt: &Stmt) {
        if !self.in_loop {
            self.vm
                .diag
                .lock()
                .report(self.fct.file_id, stmt.pos(), SemError::OutsideLoop);
        }
    }
}

impl<'a> Visitor for TypeCheck<'a> {
    fn visit_expr(&mut self, _e: &Expr) {
        unreachable!();
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        match *s {
            Stmt::Let(ref stmt) => self.check_stmt_let(stmt),
            Stmt::While(ref stmt) => self.check_stmt_while(stmt),
            Stmt::For(ref stmt) => self.check_stmt_for(stmt),
            Stmt::Return(ref stmt) => self.check_stmt_return(stmt),

            // for the rest of the statements, no special handling is necessary
            Stmt::Break(_) | Stmt::Continue(_) => {
                self.check_stmt_break_and_continue(s);
            }
            Stmt::Expr(ref stmt) => {
                self.check_expr(&stmt.expr, SourceType::Any);
            }
        }

        self.analysis.set_ty(s.id(), SourceType::Unit);
    }
}

pub fn args_compatible(
    vm: &VM,
    callee: &Fct,
    args: &[SourceType],
    type_params: &TypeList,
    self_ty: Option<SourceType>,
) -> bool {
    let def_args = callee.params_without_self();

    let right_number_of_arguments = if callee.variadic_arguments {
        def_args.len() - 1 <= args.len()
    } else {
        def_args.len() == args.len()
    };

    if !right_number_of_arguments {
        return false;
    }

    let (def, rest_ty): (&[SourceType], Option<SourceType>) = if callee.variadic_arguments {
        (&def_args[0..def_args.len() - 1], def_args.last().cloned())
    } else {
        (&def_args, None)
    };

    for (ind, def_arg) in def.iter().enumerate() {
        let def_arg = replace_type_param(vm, def_arg.clone(), &type_params, self_ty.clone());

        if !arg_allows(vm, def_arg, args[ind].clone(), self_ty.clone()) {
            return false;
        }
    }

    if let Some(rest_ty) = rest_ty {
        let ind = def.len();
        let rest_ty = replace_type_param(vm, rest_ty, &type_params, self_ty.clone());

        for expr_ty in &args[ind..] {
            if !arg_allows(vm, rest_ty.clone(), expr_ty.clone(), self_ty.clone()) {
                return false;
            }
        }
    }

    true
}

fn arg_allows(vm: &VM, def: SourceType, arg: SourceType, self_ty: Option<SourceType>) -> bool {
    match def {
        SourceType::Error | SourceType::Any => unreachable!(),
        SourceType::Unit
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Struct(_, _)
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Enum(_, _) => def == arg,
        SourceType::Ptr => panic!("ptr should not occur in fct definition."),
        SourceType::This => {
            let real = self_ty.clone().expect("no Self type expected.");
            arg_allows(vm, real, arg, self_ty)
        }
        SourceType::TraitObject(_) => panic!("trait should not occur in fct definition."),

        SourceType::TypeParam(_) => def == arg,

        SourceType::Class(cls_id, list_id) => {
            if def == arg {
                return true;
            }

            let other_cls_id;
            let other_list_id;

            match arg {
                SourceType::Class(cls_id, list_id) => {
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
                if !arg_allows(vm, tp, op, self_ty.clone()) {
                    return false;
                }
            }

            true
        }

        SourceType::Tuple(tuple_id) => match arg {
            SourceType::Tuple(other_tuple_id) => {
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
                    let ty = subtypes[idx].clone();
                    let other_ty = other_subtypes[idx].clone();

                    if !arg_allows(vm, ty, other_ty, self_ty.clone()) {
                        return false;
                    }
                }

                true
            }

            _ => false,
        },

        SourceType::Module(_) => def == arg,

        SourceType::Lambda(_) => {
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
    expected_type: SourceType,
) -> (SourceType, i64) {
    let ty = match e.suffix {
        IntSuffix::UInt8 => SourceType::UInt8,
        IntSuffix::Int32 => SourceType::Int32,
        IntSuffix::Int64 => SourceType::Int64,
        IntSuffix::None => match expected_type {
            SourceType::UInt8 => SourceType::UInt8,
            SourceType::Int32 => SourceType::Int32,
            SourceType::Int64 => SourceType::Int64,
            _ => SourceType::Int32,
        },
    };

    let ty_name = ty.name(vm);
    let value = e.value;

    if e.base == IntBase::Dec {
        let max = match ty {
            SourceType::UInt8 => 256,
            SourceType::Int32 => (1u64 << 31),
            SourceType::Int64 => (1u64 << 63),
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
            SourceType::UInt8 => 256 as u64,
            SourceType::Int32 => u32::max_value() as u64,
            SourceType::Int64 => u64::max_value() as u64,
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
) -> (SourceType, f64) {
    let ty = match e.suffix {
        FloatSuffix::Float32 => SourceType::Float32,
        FloatSuffix::Float64 => SourceType::Float64,
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

pub fn lookup_method(
    vm: &VM,
    object_type: SourceType,
    is_static: bool,
    name: Name,
    args: &[SourceType],
    fct_tps: &TypeList,
    return_type: Option<SourceType>,
) -> Option<(ClassId, FctId, SourceType)> {
    let cls_id = object_type.cls_id(vm);

    if cls_id.is_some() {
        let candidates = find_methods_in_class(vm, object_type.clone(), name, is_static);

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
            let type_params = cls_type_params.append(fct_tps);

            if args_compatible(vm, &*method, args, &type_params, None) {
                let combined_type_params = cls_type_params.append(fct_tps);
                let cmp_type =
                    replace_type_param(vm, method.return_type.clone(), &combined_type_params, None);

                if return_type.is_none() || return_type.unwrap() == cmp_type {
                    return Some((cls_id, candidate, cmp_type));
                }
            }
        }
    }

    None
}
