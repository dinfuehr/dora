use std::collections::HashSet;
use std::convert::TryFrom;
use std::sync::Arc;
use std::{f32, f64};

use crate::error::msg::SemError;
use crate::semck::fctbodyck::lookup::MethodLookup;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck::{self, ErrorReporting};
use crate::semck::{always_returns, expr_always_returns, read_type, AllowSelf};
use crate::semck::{report_sym_shadow, TypeParamContext};
use crate::sym::{NestedSymTable, Sym};
use crate::ty::{implements_trait, SourceType, SourceTypeArray};
use crate::vm::{
    self, class_accessible_from, class_field_accessible_from, const_accessible_from, ensure_tuple,
    enum_accessible_from, fct_accessible_from, find_field_in_class, find_methods_in_class,
    find_methods_in_enum, find_methods_in_struct, global_accessible_from, method_accessible_from,
    namespace_accessible_from, struct_accessible_from, struct_field_accessible_from, AnalysisData,
    CallType, ClassId, ConvInfo, EnumId, Fct, FctId, FctParent, FileId, ForTypeInfo, IdentType,
    Intrinsic, NamespaceId, StructData, StructId, TypeParam, TypeParamDefinition, TypeParamId, Var,
    VarId, VM,
};

use dora_parser::ast;
use dora_parser::ast::visit::Visitor;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntBase, IntSuffix};
use fixedbitset::FixedBitSet;

pub struct TypeCheck<'a> {
    pub vm: &'a VM,
    pub fct: &'a Fct,
    pub file_id: FileId,
    pub namespace_id: NamespaceId,
    pub analysis: &'a mut AnalysisData,
    pub ast: &'a ast::Function,
    pub symtable: NestedSymTable<'a>,
    pub in_loop: bool,
    pub self_ty: Option<SourceType>,
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
        for (type_param_id, type_param) in self.fct.type_params.iter().enumerate() {
            self.symtable
                .insert(type_param.name, Sym::TypeParam(TypeParamId(type_param_id)));
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
                mutable: false,
                ty,
                node_id: param.id,
            };

            let var_id = self.add_var(var_ctxt);
            self.analysis.map_vars.insert(param.id, var_id);

            // params are only allowed to replace functions, vars cannot be replaced
            let sym = self.symtable.get(param.name);
            match sym {
                Some(Sym::Fct(_)) | None => {
                    self.symtable.insert(param.name, Sym::Var(var_id));
                }
                Some(conflict_sym) => report_sym_shadow(
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

        let self_ty = self.fct.param_types[0].clone();
        self.self_ty = Some(self_ty.clone());

        let ast_id = self.fct.ast.id;
        let name = self.vm.interner.intern("self");

        let var = Var {
            id: VarId(0),
            name,
            ty: self_ty,
            mutable: false,
            node_id: ast_id,
        };

        assert!(self.analysis.vars.is_empty());
        self.analysis.vars.push(var);
    }

    fn add_local(&mut self, var: Var, pos: Position) -> VarId {
        let name = var.name;
        let var_id = self.add_var(var);
        match self.symtable.insert(name, Sym::Var(var_id)) {
            Some(Sym::Var(_)) | None => {}
            Some(sym) => report_sym_shadow(self.vm, name, self.fct.file_id, pos, sym),
        }
        var_id
    }

    fn add_var(&mut self, mut var: Var) -> VarId {
        let var_id = VarId(self.analysis.vars.len());

        var.id = var_id;
        self.analysis.vars.push(var);

        var_id
    }

    fn check_stmt_let(&mut self, s: &ast::StmtLetType) {
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
        self.check_stmt_let_pattern(&s.pattern, defined_type.clone(), s.mutable);

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

    fn read_type(&mut self, t: &ast::Type) -> SourceType {
        read_type(
            self.vm,
            &self.symtable,
            self.fct.file_id,
            t,
            TypeParamContext::Fct(self.fct),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error)
    }

    fn check_stmt_let_pattern(&mut self, pattern: &ast::LetPattern, ty: SourceType, mutable: bool) {
        match pattern {
            ast::LetPattern::Ident(ref ident) => {
                let var_ctxt = Var {
                    id: VarId(0),
                    name: ident.name,
                    mutable: mutable || ident.mutable,
                    ty,
                    node_id: ident.id,
                };

                let var_id = self.add_local(var_ctxt, ident.pos);
                self.analysis.map_vars.insert(ident.id, var_id);
            }

            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }

            ast::LetPattern::Tuple(ref tuple) => {
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
                        self.check_stmt_let_pattern(part, SourceType::Error, mutable);
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
                    self.check_stmt_let_pattern(part, ty, mutable);
                }
            }
        }
    }

    fn check_stmt_for(&mut self, stmt: &ast::StmtForType) {
        let object_type = self.check_expr(&stmt.expr, SourceType::Any);

        if object_type.is_error() {
            self.symtable.push_level();
            self.check_stmt_let_pattern(&stmt.pattern, SourceType::Error, false);
            self.visit_stmt(&stmt.block);
            self.symtable.pop_level();
            return;
        }

        if let Some(cls_id) = object_type.cls_id() {
            if cls_id == self.vm.known.classes.array() {
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

    fn check_loop_body(&mut self, stmt: &ast::Stmt) {
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
            .type_param_defs(&self.fct.type_params)
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
            .type_param_defs(&self.fct.type_params)
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
            .type_param_defs(&self.fct.type_params)
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

    fn check_stmt_while(&mut self, stmt: &ast::StmtWhileType) {
        let expr_type = self.check_expr(&stmt.cond, SourceType::Any);

        if !expr_type.is_error() && !expr_type.is_bool() {
            let expr_type = expr_type.name_fct(self.vm, self.fct);
            let msg = SemError::WhileCondType(expr_type);
            self.vm.diag.lock().report(self.file_id, stmt.pos, msg);
        }

        self.check_loop_body(&stmt.block);
    }

    fn check_stmt_return(&mut self, s: &ast::StmtReturnType) {
        let expected_ty = self.fct.return_type.clone();

        let expr_type = s
            .expr
            .as_ref()
            .map(|expr| self.check_expr(&expr, expected_ty))
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

    fn check_expr_block(
        &mut self,
        block: &ast::ExprBlockType,
        _expected_ty: SourceType,
    ) -> SourceType {
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

    fn check_expr_tuple(
        &mut self,
        tuple: &ast::ExprTupleType,
        _expected_ty: SourceType,
    ) -> SourceType {
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

    fn check_expr_paren(
        &mut self,
        paren: &ast::ExprParenType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let ty = self.check_expr(&paren.expr, SourceType::Any);
        self.analysis.set_ty(paren.id, ty.clone());

        ty
    }

    fn check_expr_match(
        &mut self,
        node: &ast::ExprMatchType,
        expected_ty: SourceType,
    ) -> SourceType {
        let expr_type = self.check_expr(&node.expr, SourceType::Any);
        let mut result_type = SourceType::Error;

        if !expr_type.is_enum() {
            self.vm
                .diag
                .lock()
                .report(self.file_id, node.pos, SemError::EnumExpected);
        }

        let expr_enum_id = expr_type.enum_id();
        let expr_type_params = expr_type.type_params(self.vm);

        let enum_variants = if let Some(expr_enum_id) = expr_enum_id {
            let xenum = self.vm.enums[expr_enum_id].read();
            xenum.variants.len()
        } else {
            0
        };

        let mut used_variants = FixedBitSet::with_capacity(enum_variants);

        for case in &node.cases {
            self.symtable.push_level();

            match case.pattern.data {
                ast::MatchPatternData::Underscore => {
                    let mut negated_used_variants = used_variants.clone();
                    negated_used_variants.toggle_range(..);

                    if negated_used_variants.count_ones(..) == 0 {
                        let msg = SemError::MatchUnreachablePattern;
                        self.vm.diag.lock().report(self.file_id, case.pos, msg);
                    }

                    used_variants.insert_range(..);
                }

                ast::MatchPatternData::Ident(ref ident) => {
                    let sym = self.read_path(&ident.path);

                    let mut used_idents: HashSet<Name> = HashSet::new();

                    match sym {
                        Ok(Sym::EnumValue(enum_id, variant_id)) => {
                            if Some(enum_id) == expr_enum_id {
                                if used_variants.contains(variant_id) {
                                    let msg = SemError::MatchUnreachablePattern;
                                    self.vm.diag.lock().report(self.file_id, case.pos, msg);
                                }

                                used_variants.insert(variant_id);
                                self.analysis.map_idents.insert(
                                    case.pattern.id,
                                    IdentType::EnumValue(
                                        enum_id,
                                        expr_type_params.clone(),
                                        variant_id,
                                    ),
                                );

                                let xenum = self.vm.enums[enum_id].read();
                                let variant = &xenum.variants[variant_id];

                                let given_params = if let Some(ref params) = ident.params {
                                    params.len()
                                } else {
                                    0
                                };

                                if given_params == 0 && ident.params.is_some() {
                                    let msg = SemError::MatchPatternNoParens;
                                    self.vm.diag.lock().report(self.file_id, case.pos, msg);
                                }

                                let expected_params = variant.types.len();

                                if given_params != expected_params {
                                    let msg = SemError::MatchPatternWrongNumberOfParams(
                                        given_params,
                                        expected_params,
                                    );
                                    self.vm.diag.lock().report(self.file_id, case.pos, msg);
                                }

                                if let Some(ref params) = ident.params {
                                    for (idx, param) in params.iter().enumerate() {
                                        if let Some(name) = param.name {
                                            let ty = if idx < variant.types.len() {
                                                variant.types[idx].clone()
                                            } else {
                                                SourceType::Error
                                            };

                                            let ty = replace_type_param(
                                                self.vm,
                                                ty,
                                                &expr_type_params,
                                                None,
                                            );

                                            if used_idents.insert(name) == false {
                                                let msg = SemError::VarAlreadyInPattern;
                                                self.vm.diag.lock().report(
                                                    self.file_id,
                                                    param.pos,
                                                    msg,
                                                );
                                            }

                                            let var_ctxt = Var {
                                                id: VarId(0),
                                                name,
                                                mutable: param.mutable,
                                                ty,
                                                node_id: param.id,
                                            };

                                            let var_id = self.add_local(var_ctxt, param.pos);
                                            self.analysis.map_vars.insert(param.id, var_id);
                                        }
                                    }
                                }
                            } else {
                                let msg = SemError::EnumVariantExpected;
                                self.vm.diag.lock().report(self.file_id, node.pos, msg);
                            }
                        }

                        Ok(_) => {
                            let msg = SemError::EnumVariantExpected;
                            self.vm.diag.lock().report(self.file_id, node.pos, msg);
                        }

                        Err(()) => {}
                    }
                }
            }

            let case_ty = self.check_expr(&case.value, expected_ty.clone());

            if result_type.is_error() {
                result_type = case_ty;
            } else if case_ty.is_error() {
                // ignore this case
            } else if !result_type.allows(self.vm, case_ty.clone()) {
                let result_type_name = result_type.name_fct(self.vm, self.fct);
                let case_ty_name = case_ty.name_fct(self.vm, self.fct);
                let msg = SemError::MatchBranchTypesIncompatible(result_type_name, case_ty_name);
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, case.value.pos(), msg);
            }

            self.symtable.pop_level();
        }

        used_variants.toggle_range(..);

        if used_variants.count_ones(..) != 0 {
            let msg = SemError::MatchUncoveredVariant;
            self.vm.diag.lock().report(self.file_id, node.pos, msg);
        }

        self.analysis.set_ty(node.id, result_type.clone());

        result_type
    }

    fn check_expr_if(&mut self, expr: &ast::ExprIfType, _expected_ty: SourceType) -> SourceType {
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

    fn check_expr_ident(&mut self, e: &ast::ExprIdentType, expected_ty: SourceType) -> SourceType {
        let sym = self.symtable.get(e.name);

        match sym {
            Some(Sym::Var(varid)) => {
                let ty = self.analysis.vars[varid].ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis.map_idents.insert(e.id, IdentType::Var(varid));

                ty
            }

            Some(Sym::Global(globalid)) => {
                let glob = self.vm.globals.idx(globalid);
                let ty = glob.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(globalid));

                ty
            }

            Some(Sym::Const(const_id)) => {
                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.read();

                self.analysis.set_ty(e.id, xconst.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                xconst.ty.clone()
            }

            Some(Sym::EnumValue(enum_id, variant_id)) => self.check_enum_value_without_args_id(
                e.id,
                e.pos,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_id,
            ),

            Some(Sym::Module(module_id)) => {
                let module = self.vm.modules.idx(module_id);
                let ty = module.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Module(module_id));

                ty
            }

            None => {
                let name = self.vm.interner.str(e.name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    SemError::UnknownIdentifier(name),
                );
                SourceType::Error
            }

            _ => {
                self.vm
                    .diag
                    .lock()
                    .report(self.fct.file_id, e.pos, SemError::ValueExpected);
                SourceType::Error
            }
        }
    }

    fn check_expr_assign(&mut self, e: &ast::ExprBinType) {
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

    fn check_expr_assign_ident(&mut self, e: &ast::ExprBinType) {
        self.analysis.set_ty(e.id, SourceType::Unit);

        let lhs_ident = e.lhs.to_ident().unwrap();
        let sym = self.symtable.get(lhs_ident.name);

        let lhs_type = match sym {
            Some(Sym::Var(varid)) => {
                if !self.analysis.vars[varid].mutable {
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

            Some(Sym::Global(global_id)) => {
                let glob = self.vm.globals.idx(global_id);
                let glob = glob.read();

                if !e.initializer && !glob.mutable {
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

            None => {
                let name = self.vm.interner.str(lhs_ident.name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    SemError::UnknownIdentifier(name),
                );

                return;
            }

            _ => {
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    SemError::LvalueExpected,
                );

                return;
            }
        };

        let rhs_type = self.check_expr(&e.rhs, lhs_type.clone());

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

    fn check_expr_assign_call(&mut self, e: &ast::ExprBinType) {
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

        if let Some(descriptor) = self.find_method(
            e.pos,
            expr_type.clone(),
            false,
            name,
            &arg_types,
            &SourceTypeArray::empty(),
        ) {
            let call_type = CallType::Expr(expr_type, descriptor.fct_id, descriptor.type_params);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
        }
    }

    fn check_expr_assign_field(&mut self, e: &ast::ExprBinType) {
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

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.vm, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let cls = self.vm.classes.idx(cls_ty.cls_id().expect("no class"));
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let class_type_params = cls_ty.type_params(self.vm);

                let fty = replace_type_param(self.vm, field.ty.clone(), &class_type_params, None);

                if !e.initializer && !field.mutable {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, SemError::LetReassigned);
                }

                let rhs_type = self.check_expr(&e.rhs, fty.clone());

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

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        self.check_expr(&e.rhs, SourceType::Any);

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
        fct_type_params: &SourceTypeArray,
    ) -> Option<MethodDescriptor> {
        let descriptor = lookup_method(
            self.vm,
            object_type.clone(),
            &self.fct.type_params,
            None,
            is_static,
            name,
            args,
            fct_type_params,
        );

        if descriptor.is_none() {
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

        descriptor
    }

    fn check_expr_un(&mut self, e: &ast::ExprUnType, _expected_ty: SourceType) -> SourceType {
        if e.op == ast::UnOp::Neg && e.opnd.is_lit_int() {
            let expr_type =
                self.check_expr_lit_int(e.opnd.to_lit_int().unwrap(), true, SourceType::Any);
            self.analysis.set_ty(e.id, expr_type.clone());
            return expr_type;
        }

        let opnd = self.check_expr(&e.opnd, SourceType::Any);

        match e.op {
            ast::UnOp::Plus => self.check_expr_un_method(e, e.op, "unaryPlus", opnd),
            ast::UnOp::Neg => self.check_expr_un_method(e, e.op, "unaryMinus", opnd),
            ast::UnOp::Not => self.check_expr_un_method(e, e.op, "not", opnd),
        }
    }

    fn check_expr_un_method(
        &mut self,
        e: &ast::ExprUnType,
        op: ast::UnOp,
        name: &str,
        ty: SourceType,
    ) -> SourceType {
        let name = self.vm.interner.intern(name);
        let call_types = [];

        if !ty.is_error() {
            if let Some(descriptor) = lookup_method(
                self.vm,
                ty.clone(),
                &self.fct.type_params,
                None,
                false,
                name,
                &call_types,
                &SourceTypeArray::empty(),
            ) {
                let call_type =
                    CallType::Method(ty.clone(), descriptor.fct_id, descriptor.type_params);
                self.analysis.map_calls.insert(e.id, Arc::new(call_type));

                self.analysis.set_ty(e.id, descriptor.return_type.clone());
                return descriptor.return_type;
            }

            let ty = ty.name_fct(self.vm, self.fct);
            let msg = SemError::UnOpType(op.as_str().into(), ty);

            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_bin(&mut self, e: &ast::ExprBinType, _expected_ty: SourceType) -> SourceType {
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
            ast::BinOp::Or | ast::BinOp::And => {
                self.check_expr_bin_bool(e, e.op, lhs_type, rhs_type)
            }
            ast::BinOp::Cmp(cmp) => self.check_expr_bin_cmp(e, cmp, lhs_type, rhs_type),
            ast::BinOp::Add => self.check_expr_bin_method(e, e.op, "plus", lhs_type, rhs_type),
            ast::BinOp::Sub => self.check_expr_bin_method(e, e.op, "minus", lhs_type, rhs_type),
            ast::BinOp::Mul => self.check_expr_bin_method(e, e.op, "times", lhs_type, rhs_type),
            ast::BinOp::Div => self.check_expr_bin_method(e, e.op, "div", lhs_type, rhs_type),
            ast::BinOp::Mod => self.check_expr_bin_method(e, e.op, "mod", lhs_type, rhs_type),
            ast::BinOp::BitOr => {
                self.check_expr_bin_method(e, e.op, "bitwiseOr", lhs_type, rhs_type)
            }
            ast::BinOp::BitAnd => {
                self.check_expr_bin_method(e, e.op, "bitwiseAnd", lhs_type, rhs_type)
            }
            ast::BinOp::BitXor => {
                self.check_expr_bin_method(e, e.op, "bitwiseXor", lhs_type, rhs_type)
            }
            ast::BinOp::ShiftL => {
                self.check_expr_bin_method(e, e.op, "shiftLeft", lhs_type, rhs_type)
            }
            ast::BinOp::ArithShiftR => {
                self.check_expr_bin_method(e, e.op, "shiftRightSigned", lhs_type, rhs_type)
            }
            ast::BinOp::LogicalShiftR => {
                self.check_expr_bin_method(e, e.op, "shiftRight", lhs_type, rhs_type)
            }
            ast::BinOp::Assign => unreachable!(),
        }
    }

    fn check_expr_bin_bool(
        &mut self,
        e: &ast::ExprBinType,
        op: ast::BinOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        self.check_type(e, op, lhs_type, rhs_type, SourceType::Bool);
        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    fn check_expr_bin_method(
        &mut self,
        e: &ast::ExprBinType,
        op: ast::BinOp,
        name: &str,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        let name = self.vm.interner.intern(name);
        let call_types = [rhs_type.clone()];

        if let Some(descriptor) = lookup_method(
            self.vm,
            lhs_type.clone(),
            &self.fct.type_params,
            None,
            false,
            name,
            &call_types,
            &SourceTypeArray::empty(),
        ) {
            let call_type =
                CallType::Method(lhs_type.clone(), descriptor.fct_id, descriptor.type_params);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.analysis.set_ty(e.id, descriptor.return_type.clone());

            descriptor.return_type
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
        e: &ast::ExprBinType,
        cmp: ast::CmpOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) -> SourceType {
        match cmp {
            ast::CmpOp::Is | ast::CmpOp::IsNot => {
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

            ast::CmpOp::Eq | ast::CmpOp::Ne => {
                if is_simple_enum(self.vm, lhs_type.clone()) {
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
        e: &ast::ExprBinType,
        op: ast::CmpOp,
        lhs_type: SourceType,
        rhs_type: SourceType,
    ) {
        if lhs_type.allows(self.vm, rhs_type.clone()) {
            let intrinsic = match op {
                ast::CmpOp::Eq => Intrinsic::EnumEq,
                ast::CmpOp::Ne => Intrinsic::EnumNe,
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
        e: &ast::ExprBinType,
        op: ast::BinOp,
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

    fn check_expr_call(&mut self, e: &ast::ExprCallType, expected_ty: SourceType) -> SourceType {
        let (callee, type_params) = if let Some(expr_type_params) = e.callee.to_type_param() {
            let type_params: Vec<SourceType> = expr_type_params
                .args
                .iter()
                .map(|p| self.read_type(p))
                .collect();
            let type_params: SourceTypeArray = SourceTypeArray::with(type_params);
            (&expr_type_params.callee, type_params)
        } else {
            (&e.callee, SourceTypeArray::empty())
        };

        let arg_types: Vec<SourceType> = e
            .args
            .iter()
            .map(|arg| self.check_expr(arg, SourceType::Any))
            .collect();

        if let Some(expr_ident) = callee.to_ident() {
            let sym = self.symtable.get(expr_ident.name);

            self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, &arg_types)
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
            self.check_expr_call_path(e, expected_ty, callee, type_params, &arg_types)
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
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        callee: &ast::Expr,
        sym: Option<Sym>,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        match sym {
            Some(Sym::Fct(fct_id)) => self.check_expr_call_fct(e, fct_id, type_params, &arg_types),

            Some(Sym::Class(cls_id)) => {
                self.check_expr_call_ctor(e, cls_id, type_params, &arg_types)
            }

            Some(Sym::Struct(struct_id)) => {
                self.check_expr_call_struct(e, struct_id, type_params, &arg_types)
            }

            Some(Sym::EnumValue(enum_id, variant_id)) => self.check_enum_value_with_args(
                e,
                expected_ty,
                enum_id,
                type_params,
                variant_id,
                &arg_types,
            ),

            _ => {
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
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        enum_id: EnumId,
        type_params: SourceTypeArray,
        variant_id: usize,
        arg_types: &[SourceType],
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();
        let variant = &xenum.variants[variant_id as usize];

        if !enum_accessible_from(self.vm, enum_id, self.namespace_id) {
            let msg = SemError::NotAccessible(xenum.name(self.vm));
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params(self.vm)
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.pos),
        );

        if !type_params_ok {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

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

        let list_id = self
            .vm
            .source_type_arrays
            .lock()
            .insert(type_params.clone());
        let ty = SourceType::Enum(enum_id, list_id);

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Enum(ty.clone(), variant_id)));

        self.analysis.set_ty(e.id, ty.clone());
        ty
    }

    fn check_expr_call_enum_args(
        &mut self,
        _enum_id: EnumId,
        type_params: SourceTypeArray,
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
        e: &ast::ExprCallType,
        tp_id: TypeParamId,
        name: Name,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut fcts = Vec::new();

        let type_param = self.fct.type_param(tp_id);

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

        let tp = SourceType::TypeParam(tp_id);

        if !args_compatible(
            self.vm,
            &*fct,
            arg_types,
            &SourceTypeArray::empty(),
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
            &SourceTypeArray::empty(),
            Some(tp),
        );

        self.analysis.set_ty(e.id, return_type.clone());

        return_type
    }

    fn check_expr_call_expr(
        &mut self,
        e: &ast::ExprCallType,
        expr_type: SourceType,
        arg_types: &[SourceType],
    ) -> SourceType {
        if expr_type.is_error() {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let get = self.vm.interner.intern("get");

        if let Some(descriptor) = self.find_method(
            e.pos,
            expr_type.clone(),
            false,
            get,
            arg_types,
            &SourceTypeArray::empty(),
        ) {
            let call_type =
                CallType::Expr(expr_type.clone(), descriptor.fct_id, descriptor.type_params);
            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));

            self.analysis.set_ty(e.id, descriptor.return_type.clone());

            descriptor.return_type
        } else {
            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_fct(
        &mut self,
        e: &ast::ExprCallType,
        fct_id: FctId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if !fct_accessible_from(self.vm, fct_id, self.namespace_id) {
            let fct = self.vm.fcts.idx(fct_id);
            let fct = fct.read();
            let msg = SemError::NotAccessible(fct.name(self.vm));
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .pos(e.pos)
            .callee(fct_id)
            .args(&arg_types)
            .fct_type_params(&type_params);

        let ty = if lookup.find() {
            let call_type = CallType::Fct(fct_id, type_params.clone());
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
        e: &ast::ExprCallType,
        object_type: SourceType,
        method_name: Name,
        fct_type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .pos(e.pos)
            .static_method(object_type)
            .name(method_name)
            .args(arg_types)
            .fct_type_params(&fct_type_params)
            .type_param_defs(&self.fct.type_params);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();
            let container_type_params = lookup.found_container_type_params().unwrap();
            let type_params = container_type_params.connect(&fct_type_params);
            let call_type = Arc::new(CallType::Fct(fct_id, type_params));
            self.analysis.map_calls.insert(e.id, call_type.clone());

            if !method_accessible_from(self.vm, fct_id, self.namespace_id) {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();

                let name = fct.name(self.vm);
                let msg = SemError::NotAccessible(name);
                self.vm.diag.lock().report(self.file_id, e.pos, msg);
            }

            self.analysis.set_ty(e.id, return_type.clone());

            return_type
        } else {
            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_method(
        &mut self,
        e: &ast::ExprCallType,
        object_type: SourceType,
        method_name: Name,
        fct_type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if let SourceType::TypeParam(id) = object_type {
            assert_eq!(fct_type_params.len(), 0);
            return self.check_expr_call_generic(e, id, method_name, arg_types);
        }

        if object_type.is_error() {
            self.analysis.set_ty(e.id, SourceType::Error);

            return SourceType::Error;
        }

        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .no_error_reporting()
            .method(object_type.clone())
            .name(method_name)
            .fct_type_params(&fct_type_params)
            .type_param_defs(&self.fct.type_params)
            .args(arg_types);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();

            let call_type = if object_type.is_trait() {
                CallType::TraitObjectMethod(object_type, fct_id)
            } else {
                let method_type = lookup.found_class_type().unwrap();
                if method_type.is_module() {
                    CallType::ModuleMethod(method_type, fct_id, fct_type_params.clone())
                } else {
                    let container_type_params =
                        lookup.found_container_type_params().clone().unwrap();
                    let type_params = container_type_params.connect(&fct_type_params);
                    CallType::Method(method_type, fct_id, type_params)
                }
            };

            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
            self.analysis.set_ty(e.id, return_type.clone());

            if !method_accessible_from(self.vm, fct_id, self.namespace_id) {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();

                let name = fct.name(self.vm);
                let msg = SemError::NotAccessible(name);
                self.vm.diag.lock().report(self.file_id, e.pos, msg);
            }

            return_type
        } else if lookup.found_fct_id().is_none() {
            // No method with this name found, so this might actually be a field
            self.check_expr_call_field(e, object_type, method_name, fct_type_params, arg_types)
        } else {
            // Lookup the method again, but this time with error reporting
            let mut lookup = MethodLookup::new(self.vm, self.fct)
                .method(object_type)
                .name(method_name)
                .fct_type_params(&fct_type_params)
                .type_param_defs(&self.fct.type_params)
                .pos(e.pos)
                .args(arg_types);

            assert!(!lookup.find());

            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_field(
        &mut self,
        e: &ast::ExprCallType,
        object_type: SourceType,
        method_name: Name,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if let Some((actual_type, field_id, field_type)) =
            find_field_in_class(self.vm, object_type.clone(), method_name)
        {
            self.analysis.set_ty(e.callee.id(), field_type.clone());
            self.analysis
                .map_idents
                .insert_or_replace(e.callee.id(), IdentType::Field(actual_type, field_id));

            let cls_id = object_type.cls_id().expect("class expected");

            if !class_field_accessible_from(self.vm, cls_id, field_id, self.namespace_id) {
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let name = self.vm.interner.str(field.name).to_string();
                let msg = SemError::NotAccessible(name);
                self.vm.diag.lock().report(self.file_id, e.pos, msg);
            }

            return self.check_expr_call_expr(e, field_type, arg_types);
        }

        if let Some(struct_id) = object_type.struct_id() {
            let xstruct = self.vm.structs.idx(struct_id);
            let xstruct = xstruct.read();
            if let Some(&field_id) = xstruct.field_names.get(&method_name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &xstruct.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params(self.vm);
                let field_type =
                    replace_type_param(self.vm, field.ty.clone(), &struct_type_params, None);

                if !struct_field_accessible_from(self.vm, struct_id, field_id, self.namespace_id) {
                    let name = self.vm.interner.str(field.name).to_string();
                    let msg = SemError::NotAccessible(name);
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, field_type.clone());
                return self.check_expr_call_expr(e, field_type, arg_types);
            }
        }

        // No field with that name as well, so report method
        let mut lookup = MethodLookup::new(self.vm, self.fct)
            .method(object_type)
            .name(method_name)
            .fct_type_params(&type_params)
            .type_param_defs(&self.fct.type_params)
            .pos(e.pos)
            .args(arg_types);
        assert!(!lookup.find());

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_call_struct(
        &mut self,
        e: &ast::ExprCallType,
        struct_id: StructId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if !struct_accessible_from(self.vm, struct_id, self.namespace_id) {
            let xstruct = self.vm.structs.idx(struct_id);
            let xstruct = xstruct.read();
            let msg = SemError::NotAccessible(xstruct.name(self.vm));
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        let xstruct = self.vm.structs.idx(struct_id);
        let xstruct = xstruct.read();

        let list_id = self
            .vm
            .source_type_arrays
            .lock()
            .insert(type_params.clone());
        let ty = SourceType::Struct(struct_id, list_id);
        let type_params_ok = typeparamck::check_struct(
            self.vm,
            self.fct,
            struct_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.pos),
        );

        if !type_params_ok {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        if !self.check_expr_call_struct_args(&*xstruct, type_params.clone(), arg_types) {
            let struct_name = self.vm.interner.str(xstruct.name).to_string();
            let field_types = xstruct
                .fields
                .iter()
                .map(|field| field.ty.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.vm, self.fct))
                .collect::<Vec<_>>();
            let msg = SemError::StructArgsIncompatible(struct_name, field_types, arg_types);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Struct(struct_id, type_params)));

        self.analysis.set_ty(e.id, ty.clone());
        ty
    }

    fn check_expr_call_struct_args(
        &mut self,
        xstruct: &StructData,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> bool {
        if xstruct.fields.len() != arg_types.len() {
            return false;
        }

        for (def_ty, arg_ty) in xstruct.fields.iter().zip(arg_types) {
            let def_ty = replace_type_param(self.vm, def_ty.ty.clone(), &type_params, None);

            if !def_ty.allows(self.vm, arg_ty.clone()) {
                return false;
            }
        }

        true
    }

    fn check_expr_call_ctor(
        &mut self,
        e: &ast::ExprCallType,
        cls_id: ClassId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if !class_accessible_from(self.vm, cls_id, self.namespace_id) {
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let msg = SemError::NotAccessible(cls.name(self.vm));
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        if !typeparamck::check_class(
            self.vm,
            self.fct,
            cls_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.pos),
        ) {
            return SourceType::Error;
        };

        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        if cls.constructor.is_none() {
            self.vm
                .diag
                .lock()
                .report(self.file_id, e.pos, SemError::UnknownCtor);
        }

        let ctor_id = cls.constructor.expect("missing constructor");
        let ctor = self.vm.fcts.idx(ctor_id);
        let ctor = ctor.read();

        let cls_ty = self.vm.cls_with_type_list(cls_id, type_params.clone());

        if !args_compatible(self.vm, &*ctor, arg_types, &type_params, None) {
            let fct_name = self.vm.interner.str(ctor.name).to_string();
            let fct_params = ctor
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(self.vm, &*ctor))
                .collect::<Vec<_>>();
            let call_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.vm, &*ctor))
                .collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        let call_type = CallType::Ctor(cls_ty.clone(), ctor_id);
        self.analysis.map_calls.insert(e.id, Arc::new(call_type));

        if cls.is_abstract {
            let msg = SemError::NewAbstractClass;
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis.set_ty(e.id, cls_ty.clone());

        cls_ty
    }

    fn check_expr_call_generic(
        &mut self,
        e: &ast::ExprCallType,
        tp_id: TypeParamId,
        name: Name,
        arg_types: &[SourceType],
    ) -> SourceType {
        let tp = self.fct.type_param(tp_id);
        self.check_expr_call_generic_type_param(
            e,
            SourceType::TypeParam(tp_id),
            tp_id,
            tp,
            name,
            arg_types,
        )
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &ast::ExprCallType,
        object_type: SourceType,
        id: TypeParamId,
        tp: &vm::TypeParam,
        name: Name,
        args: &[SourceType],
    ) -> SourceType {
        let mut found_fcts = Vec::new();

        for &trait_id in &tp.trait_bounds {
            let xtrait = self.vm.traits[trait_id].read();

            if let Some(fid) = xtrait.find_method_with_replace(self.vm, false, name, None, args) {
                found_fcts.push(fid);
            }
        }

        if found_fcts.len() == 1 {
            let fid = found_fcts[0];

            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();
            let return_type = fct.return_type.clone();

            self.analysis.set_ty(e.id, return_type.clone());

            let call_type = CallType::GenericMethod(id, fct.trait_id(), fid);
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
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        callee: &ast::Expr,
        type_params: SourceTypeArray,
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
                let container_type_params: SourceTypeArray =
                    SourceTypeArray::with(container_type_params);

                (&expr_type_params.callee, container_type_params)
            } else {
                (&callee_as_path.lhs, SourceTypeArray::empty())
            };
        let method_expr = &callee_as_path.rhs;

        let sym = match self.read_path_expr(container_expr) {
            Ok(sym) => sym,
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

        match sym {
            Some(Sym::Module(module_id)) => {
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

            Some(Sym::Class(cls_id)) => {
                if typeparamck::check_class(
                    self.vm,
                    self.fct,
                    cls_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.pos),
                ) {
                    let list_id = self
                        .vm
                        .source_type_arrays
                        .lock()
                        .insert(container_type_params);
                    self.check_expr_call_static_method(
                        e,
                        SourceType::Class(cls_id, list_id),
                        method_name,
                        type_params,
                        &arg_types,
                    )
                } else {
                    SourceType::Error
                }
            }

            Some(Sym::Struct(struct_id)) => {
                let xstruct = self.vm.structs.idx(struct_id);
                let xstruct = xstruct.read();

                if typeparamck::check_struct(
                    self.vm,
                    self.fct,
                    struct_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.pos),
                ) {
                    let object_ty = if let Some(ref primitive_ty) = xstruct.primitive_ty {
                        assert!(container_type_params.is_empty());
                        primitive_ty.clone()
                    } else {
                        let list_id = self
                            .vm
                            .source_type_arrays
                            .lock()
                            .insert(container_type_params);
                        SourceType::Struct(struct_id, list_id)
                    };

                    self.check_expr_call_static_method(
                        e,
                        object_ty,
                        method_name,
                        type_params,
                        &arg_types,
                    )
                } else {
                    SourceType::Error
                }
            }

            Some(Sym::Enum(enum_id)) => {
                let xenum = self.vm.enums[enum_id].read();

                if let Some(&variant_id) = xenum.name_to_value.get(&method_name) {
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

                    self.check_enum_value_with_args(
                        e,
                        expected_ty,
                        enum_id,
                        used_type_params,
                        variant_id as usize,
                        &arg_types,
                    )
                } else {
                    if typeparamck::check_enum(
                        self.vm,
                        self.fct,
                        enum_id,
                        &container_type_params,
                        ErrorReporting::Yes(self.file_id, e.pos),
                    ) {
                        let list_id = self
                            .vm
                            .source_type_arrays
                            .lock()
                            .insert(container_type_params);
                        let object_ty = SourceType::Enum(enum_id, list_id);

                        self.check_expr_call_static_method(
                            e,
                            object_ty,
                            method_name,
                            type_params,
                            &arg_types,
                        )
                    } else {
                        SourceType::Error
                    }
                }
            }

            Some(Sym::TypeParam(id)) => {
                if !container_type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                self.check_expr_call_generic_static_method(e, id, method_name, &arg_types)
            }

            Some(Sym::Namespace(namespace_id)) => {
                if !container_type_params.is_empty() {
                    let msg = SemError::NoTypeParamsExpected;
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let sym = {
                    let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                    let table = namespace.table.read();

                    table.get(method_name)
                };

                self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, arg_types)
            }

            _ => {
                let msg = SemError::ClassExpected;
                self.vm.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);

                SourceType::Error
            }
        }
    }

    fn check_expr_delegation(
        &mut self,
        e: &ast::ExprDelegationType,
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
        let cls_id = parent_class.cls_id().expect("no class");
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

        self.vm
            .diag
            .lock()
            .report(self.file_id, e.pos, SemError::UnknownCtor);

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

    fn check_expr_path(&mut self, e: &ast::ExprPathType, expected_ty: SourceType) -> SourceType {
        let (container_expr, type_params) = if let Some(expr_type_params) = e.lhs.to_type_param() {
            let type_params: Vec<SourceType> = expr_type_params
                .args
                .iter()
                .map(|p| self.read_type(p))
                .collect();
            let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

            (&expr_type_params.callee, type_params)
        } else {
            (&e.lhs, SourceTypeArray::empty())
        };

        let sym = match self.read_path_expr(container_expr) {
            Ok(sym) => sym,
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

        match sym {
            Some(Sym::Enum(id)) => self.check_enum_value_without_args(
                e.id,
                e.pos,
                expected_ty,
                id,
                type_params,
                element_name,
            ),

            Some(Sym::Namespace(namespace_id)) => {
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

    fn read_path_expr(&mut self, expr: &ast::Expr) -> Result<Option<Sym>, ()> {
        if let Some(expr_path) = expr.to_path() {
            let sym = self.read_path_expr(&expr_path.lhs)?;

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

            match sym {
                Some(Sym::Namespace(namespace_id)) => {
                    let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();
                    let sym = symtable.get(element_name);

                    Ok(sym)
                }

                _ => {
                    let msg = SemError::ExpectedNamespace;
                    self.vm.diag.lock().report(self.file_id, expr.pos(), msg);
                    Err(())
                }
            }
        } else if let Some(expr_ident) = expr.to_ident() {
            let container_name = expr_ident.name;
            let sym = self.symtable.get(container_name);

            Ok(sym)
        } else {
            let msg = SemError::ExpectedSomeIdentifier;
            self.vm.diag.lock().report(self.file_id, expr.pos(), msg);
            Err(())
        }
    }

    fn read_path(&mut self, path: &ast::Path) -> Result<Sym, ()> {
        let names = &path.names;
        let mut sym = self.symtable.get(names[0]);

        for &name in &names[1..] {
            match sym {
                Some(Sym::Namespace(namespace_id)) => {
                    if !namespace_accessible_from(self.vm, namespace_id, self.namespace_id) {
                        let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                        let msg = SemError::NotAccessible(namespace.name(self.vm));
                        self.vm.diag.lock().report(self.file_id, path.pos, msg);
                    }

                    let namespace = &self.vm.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();
                    sym = symtable.get(name);
                }

                Some(Sym::Enum(enum_id)) => {
                    let xenum = self.vm.enums[enum_id].read();

                    if !enum_accessible_from(self.vm, enum_id, self.namespace_id) {
                        let msg = SemError::NotAccessible(xenum.name(self.vm));
                        self.vm.diag.lock().report(self.file_id, path.pos, msg);
                    }

                    if let Some(&variant_id) = xenum.name_to_value.get(&name) {
                        sym = Some(Sym::EnumValue(enum_id, variant_id as usize));
                    } else {
                        let name = self.vm.interner.str(name).to_string();
                        self.vm.diag.lock().report(
                            self.file_id.into(),
                            path.pos,
                            SemError::UnknownEnumValue(name),
                        );
                        return Err(());
                    }
                }

                Some(_) => {
                    let msg = SemError::ExpectedNamespace;
                    self.vm.diag.lock().report(self.file_id, path.pos, msg);
                    return Err(());
                }

                None => {
                    let name = self.vm.interner.str(names[0]).to_string();
                    let msg = SemError::UnknownIdentifier(name);
                    self.vm.diag.lock().report(self.file_id, path.pos, msg);
                    return Err(());
                }
            }
        }

        if let Some(sym) = sym {
            Ok(sym)
        } else {
            let name = self.vm.interner.str(names[0]).to_string();
            let msg = SemError::UnknownIdentifier(name);
            self.vm.diag.lock().report(self.file_id, path.pos, msg);

            Err(())
        }
    }

    fn check_expr_path_namespace(
        &mut self,
        e: &ast::ExprPathType,
        expected_ty: SourceType,
        namespace_id: NamespaceId,
        element_name: Name,
    ) -> SourceType {
        let namespace = &self.vm.namespaces[namespace_id.to_usize()];
        let table = namespace.table.read();

        let sym = table.get(element_name);

        match sym {
            Some(Sym::Global(global_id)) => {
                if !global_accessible_from(self.vm, global_id, self.namespace_id) {
                    let global = &self.vm.globals.idx(global_id);
                    let global = global.read();
                    let msg = SemError::NotAccessible(global.name(self.vm));
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

                let glob = self.vm.globals.idx(global_id);
                let ty = glob.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(global_id));

                ty
            }

            Some(Sym::Const(const_id)) => {
                if !const_accessible_from(self.vm, const_id, self.namespace_id) {
                    let xconst = self.vm.consts.idx(const_id);
                    let xconst = xconst.read();
                    let msg = SemError::NotAccessible(xconst.name(self.vm));
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

                let xconst = self.vm.consts.idx(const_id);
                let xconst = xconst.read();

                self.analysis.set_ty(e.id, xconst.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                xconst.ty.clone()
            }

            Some(Sym::EnumValue(enum_id, variant_id)) => self.check_enum_value_without_args_id(
                e.id,
                e.pos,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_id,
            ),

            None => {
                let namespace = namespace.name(self.vm);
                let name = self.vm.interner.str(element_name).to_string();
                self.vm.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    SemError::UnknownIdentifierInNamespace(namespace, name),
                );
                SourceType::Error
            }

            _ => {
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
        expr_id: ast::NodeId,
        expr_pos: Position,
        _expected_ty: SourceType,
        enum_id: EnumId,
        type_params: SourceTypeArray,
        name: Name,
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();

        if !enum_accessible_from(self.vm, enum_id, self.namespace_id) {
            let msg = SemError::NotAccessible(xenum.name(self.vm));
            self.vm.diag.lock().report(self.file_id, expr_pos, msg);
        }

        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            enum_id,
            &type_params,
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
                    .map(|a| a.name_enum(self.vm, &*xenum))
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
                IdentType::EnumValue(enum_id, type_params.clone(), value as usize),
            );
        } else {
            let name = self.vm.interner.str(name).to_string();
            self.vm
                .diag
                .lock()
                .report(self.file_id, expr_pos, SemError::UnknownEnumValue(name));
        }

        if type_params_ok {
            let list_id = self
                .vm
                .source_type_arrays
                .lock()
                .insert(type_params.clone());
            let ty = SourceType::Enum(enum_id, list_id);

            self.analysis.set_ty(expr_id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(expr_id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_type_param(
        &mut self,
        e: &ast::ExprTypeParamType,
        expected_ty: SourceType,
    ) -> SourceType {
        let type_params: Vec<SourceType> = e.args.iter().map(|p| self.read_type(p)).collect();
        let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

        if let Some(ident) = e.callee.to_ident() {
            let method_name = ident.name;

            let sym = self.symtable.get(method_name);

            match sym {
                Some(Sym::EnumValue(enum_id, variant_id)) => self.check_enum_value_without_args_id(
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

            let sym = self.symtable.get(container_name);

            match sym {
                Some(Sym::Enum(enum_id)) => self.check_enum_value_without_args(
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
        expr_id: ast::NodeId,
        expr_pos: Position,
        expected_ty: SourceType,
        enum_id: EnumId,
        type_params: SourceTypeArray,
        variant_id: usize,
    ) -> SourceType {
        let xenum = self.vm.enums[enum_id].read();

        if !enum_accessible_from(self.vm, enum_id, self.namespace_id) {
            let msg = SemError::NotAccessible(xenum.name(self.vm));
            self.vm.diag.lock().report(self.file_id, expr_pos, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params(self.vm)
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.vm,
            self.fct,
            enum_id,
            &type_params,
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
            IdentType::EnumValue(enum_id, type_params.clone(), variant_id),
        );

        if type_params_ok {
            let list_id = self
                .vm
                .source_type_arrays
                .lock()
                .insert(type_params.clone());
            let ty = SourceType::Enum(enum_id, list_id);

            self.analysis.set_ty(expr_id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(expr_id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_dot(&mut self, e: &ast::ExprDotType, _expected_ty: SourceType) -> SourceType {
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

        if let Some(struct_id) = object_type.struct_id() {
            let xstruct = self.vm.structs.idx(struct_id);
            let xstruct = xstruct.read();
            if let Some(&field_id) = xstruct.field_names.get(&name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &xstruct.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params(self.vm);
                let fty = replace_type_param(self.vm, field.ty.clone(), &struct_type_params, None);

                if !struct_field_accessible_from(self.vm, struct_id, field_id, self.namespace_id) {
                    let name = self.vm.interner.str(field.name).to_string();
                    let msg = SemError::NotAccessible(name);
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.vm, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let cls_id = cls_ty.cls_id().expect("no class");
                let cls = self.vm.classes.idx(cls_id);
                let cls = cls.read();

                let field = &cls.fields[field_id];
                let class_type_params = cls_ty.type_params(self.vm);
                let fty = replace_type_param(self.vm, field.ty.clone(), &class_type_params, None);

                if !class_field_accessible_from(self.vm, cls_id, field_id, self.namespace_id) {
                    let name = self.vm.interner.str(field.name).to_string();
                    let msg = SemError::NotAccessible(name);
                    self.vm.diag.lock().report(self.file_id, e.pos, msg);
                }

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

    fn check_expr_dot_tuple(
        &mut self,
        e: &ast::ExprDotType,
        object_type: SourceType,
    ) -> SourceType {
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

    fn check_expr_this(&mut self, e: &ast::ExprSelfType, _expected_ty: SourceType) -> SourceType {
        let self_ty = if let Some(ref self_ty) = self.self_ty {
            self_ty.clone()
        } else {
            let msg = SemError::ThisUnavailable;
            self.vm.diag.lock().report(self.file_id, e.pos, msg);
            SourceType::Error
        };

        self.analysis.set_ty(e.id, self_ty.clone());
        self_ty
    }

    fn check_expr_super(&mut self, e: &ast::ExprSuperType, _expected_ty: SourceType) -> SourceType {
        let msg = SemError::SuperNeedsMethodCall;
        self.vm.diag.lock().report(self.file_id, e.pos, msg);
        self.analysis.set_ty(e.id, SourceType::Unit);

        SourceType::Unit
    }

    fn check_expr_lambda(
        &mut self,
        e: &Arc<ast::Function>,
        _expected_ty: SourceType,
    ) -> SourceType {
        let ret = if let Some(ref ret_type) = e.return_type {
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

    fn check_expr_conv(&mut self, e: &ast::ExprConvType, _expected_ty: SourceType) -> SourceType {
        let object_type = self.check_expr(&e.object, SourceType::Any);
        self.analysis.set_ty(e.object.id(), object_type.clone());

        let check_type = self.read_type(&e.data_type);
        self.analysis.set_ty(e.data_type.id(), check_type.clone());

        if let SourceType::Trait(trait_id, _) = check_type.clone() {
            if !e.is {
                let implements = implements_trait(
                    self.vm,
                    object_type.clone(),
                    &self.fct.type_params,
                    trait_id,
                );

                if !implements {
                    let object_type = object_type.name_fct(self.vm, self.fct);
                    let check_type = check_type.name_fct(self.vm, self.fct);

                    self.vm.diag.lock().report(
                        self.file_id,
                        e.pos,
                        SemError::TypeNotImplementingTrait(object_type, check_type),
                    );
                }

                self.analysis.set_ty(e.id, check_type.clone());
                return check_type;
            }
        }

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
        } else if !object_type.is_error() && !check_type.is_error() {
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
        e: &ast::ExprLitIntType,
        negate: bool,
        expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_int(self.vm, self.file_id, e, negate, expected_ty);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_float(
        &mut self,
        e: &ast::ExprLitFloatType,
        negate: bool,
        _expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_float(self.vm, self.file_id, e, negate);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_str(
        &mut self,
        e: &ast::ExprLitStrType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let str_ty = self.vm.cls(self.vm.known.classes.string());
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    fn check_expr_lit_bool(
        &mut self,
        e: &ast::ExprLitBoolType,
        _expected_ty: SourceType,
    ) -> SourceType {
        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    fn check_expr_lit_char(
        &mut self,
        e: &ast::ExprLitCharType,
        _expected_ty: SourceType,
    ) -> SourceType {
        self.analysis.set_ty(e.id, SourceType::Char);

        SourceType::Char
    }

    fn check_expr_template(
        &mut self,
        e: &ast::ExprTemplateType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let stringable_trait = self.vm.known.traits.stringable;

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = self.check_expr(part, SourceType::Any);

                if part_expr.is_error() {
                    continue;
                }

                let implements_stringable = if let SourceType::TypeParam(id) = part_expr {
                    let tp = self.fct.type_param(id);
                    tp.trait_bounds.contains(&stringable_trait)
                } else {
                    implements_trait(
                        self.vm,
                        part_expr.clone(),
                        &self.fct.type_params,
                        stringable_trait,
                    )
                };

                if implements_stringable {
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

        let str_ty = self.vm.cls(self.vm.known.classes.string());
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    fn check_expr(&mut self, e: &ast::Expr, expected_ty: SourceType) -> SourceType {
        match *e {
            ast::Expr::LitChar(ref expr) => self.check_expr_lit_char(expr, expected_ty),
            ast::Expr::LitInt(ref expr) => self.check_expr_lit_int(expr, false, expected_ty),
            ast::Expr::LitFloat(ref expr) => self.check_expr_lit_float(expr, false, expected_ty),
            ast::Expr::LitStr(ref expr) => self.check_expr_lit_str(expr, expected_ty),
            ast::Expr::Template(ref expr) => self.check_expr_template(expr, expected_ty),
            ast::Expr::LitBool(ref expr) => self.check_expr_lit_bool(expr, expected_ty),
            ast::Expr::Ident(ref expr) => self.check_expr_ident(expr, expected_ty),
            ast::Expr::Un(ref expr) => self.check_expr_un(expr, expected_ty),
            ast::Expr::Bin(ref expr) => self.check_expr_bin(expr, expected_ty),
            ast::Expr::Call(ref expr) => self.check_expr_call(expr, expected_ty),
            ast::Expr::TypeParam(ref expr) => self.check_expr_type_param(expr, expected_ty),
            ast::Expr::Path(ref expr) => self.check_expr_path(expr, expected_ty),
            ast::Expr::Delegation(ref expr) => self.check_expr_delegation(expr, expected_ty),
            ast::Expr::Dot(ref expr) => self.check_expr_dot(expr, expected_ty),
            ast::Expr::This(ref expr) => self.check_expr_this(expr, expected_ty),
            ast::Expr::Super(ref expr) => self.check_expr_super(expr, expected_ty),
            ast::Expr::Conv(ref expr) => self.check_expr_conv(expr, expected_ty),
            ast::Expr::Lambda(ref expr) => self.check_expr_lambda(expr, expected_ty),
            ast::Expr::Block(ref expr) => self.check_expr_block(expr, expected_ty),
            ast::Expr::If(ref expr) => self.check_expr_if(expr, expected_ty),
            ast::Expr::Tuple(ref expr) => self.check_expr_tuple(expr, expected_ty),
            ast::Expr::Paren(ref expr) => self.check_expr_paren(expr, expected_ty),
            ast::Expr::Match(ref expr) => self.check_expr_match(expr, expected_ty),
        }
    }

    fn check_stmt_break_and_continue(&mut self, stmt: &ast::Stmt) {
        if !self.in_loop {
            self.vm
                .diag
                .lock()
                .report(self.fct.file_id, stmt.pos(), SemError::OutsideLoop);
        }
    }
}

impl<'a> Visitor for TypeCheck<'a> {
    fn visit_expr(&mut self, _e: &ast::Expr) {
        unreachable!();
    }

    fn visit_stmt(&mut self, s: &ast::Stmt) {
        match *s {
            ast::Stmt::Let(ref stmt) => self.check_stmt_let(stmt),
            ast::Stmt::While(ref stmt) => self.check_stmt_while(stmt),
            ast::Stmt::For(ref stmt) => self.check_stmt_for(stmt),
            ast::Stmt::Return(ref stmt) => self.check_stmt_return(stmt),

            // for the rest of the statements, no special handling is necessary
            ast::Stmt::Break(_) | ast::Stmt::Continue(_) => {
                self.check_stmt_break_and_continue(s);
            }
            ast::Stmt::Expr(ref stmt) => {
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
    type_params: &SourceTypeArray,
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
        | SourceType::Enum(_, _)
        | SourceType::Trait(_, _) => def == arg,
        SourceType::Ptr => panic!("ptr should not occur in fct definition."),
        SourceType::This => {
            let real = self_ty.clone().expect("no Self type expected.");
            arg_allows(vm, real, arg, self_ty)
        }

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

            let params = vm.source_type_arrays.lock().get(list_id);
            let other_params = vm.source_type_arrays.lock().get(other_list_id);

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
    e: &ast::ExprLitIntType,
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
    e: &ast::ExprLitFloatType,
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

struct MethodDescriptor {
    fct_id: FctId,
    type_params: SourceTypeArray,
    return_type: SourceType,
}

fn lookup_method(
    vm: &VM,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    type_param_defs2: Option<&TypeParamDefinition>,
    is_static: bool,
    name: Name,
    args: &[SourceType],
    fct_type_params: &SourceTypeArray,
) -> Option<MethodDescriptor> {
    let candidates = if object_type.is_enum() {
        find_methods_in_enum(
            vm,
            object_type,
            type_param_defs,
            type_param_defs2,
            name,
            is_static,
        )
    } else if object_type.is_struct() || object_type.is_primitive() {
        find_methods_in_struct(
            vm,
            object_type,
            type_param_defs,
            type_param_defs2,
            name,
            is_static,
        )
    } else if object_type.cls_id().is_some() {
        find_methods_in_class(
            vm,
            object_type,
            type_param_defs,
            type_param_defs2,
            name,
            is_static,
        )
    } else {
        Vec::new()
    };

    if candidates.len() == 1 {
        let method_id = candidates[0].fct_id;
        let method = vm.fcts.idx(method_id);
        let method = method.read();

        let container_type_params = &candidates[0].container_type_params;
        let type_params = container_type_params.connect(fct_type_params);

        if args_compatible(vm, &*method, args, &type_params, None) {
            let cmp_type = replace_type_param(vm, method.return_type.clone(), &type_params, None);

            return Some(MethodDescriptor {
                fct_id: method_id,
                type_params: type_params,
                return_type: cmp_type,
            });
        }
    }

    None
}

fn is_simple_enum(vm: &VM, ty: SourceType) -> bool {
    match ty {
        SourceType::Enum(enum_id, _) => {
            let xenum = vm.enums[enum_id].read();
            xenum.simple_enumeration
        }

        _ => false,
    }
}
