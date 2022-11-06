use std::collections::HashSet;
use std::convert::TryFrom;
use std::sync::Arc;
use std::{f32, f64};

use fixedbitset::FixedBitSet;
use option_ext::OptionExt;

use dora_parser::ast;
use dora_parser::ast::visit::Visitor;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntBase, IntSuffix};

use crate::language::access::{
    class_accessible_from, class_field_accessible_from, const_accessible_from,
    enum_accessible_from, fct_accessible_from, global_accessible_from, is_default_accessible,
    method_accessible_from, module_accessible_from, struct_accessible_from,
    struct_field_accessible_from,
};
use crate::language::error::msg::ErrorMessage;
use crate::language::fctbodyck::lookup::MethodLookup;
use crate::language::sem_analysis::{
    create_tuple, find_field_in_class, find_methods_in_class, find_methods_in_enum,
    find_methods_in_struct, implements_trait, AnalysisData, CallType, ClassDefinition,
    ClassDefinitionId, ContextIdx, EnumDefinitionId, EnumVariant, FctDefinition, FctDefinitionId,
    FctParent, Field, FieldId, ForTypeInfo, IdentType, Intrinsic, ModuleDefinitionId, NestedVarId,
    PackageDefinitionId, SemAnalysis, SourceFileId, StructDefinition, StructDefinitionId,
    TypeParamDefinition, TypeParamId, Var, VarAccess, VarId, VarLocation, Visibility,
};
use crate::language::specialize::replace_type_param;
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::language::typeparamck::{self, ErrorReporting};
use crate::language::{always_returns, expr_always_returns, read_type, AllowSelf};
use crate::language::{report_sym_shadow, TypeParamContext};

pub struct TypeCheck<'a> {
    pub sa: &'a mut SemAnalysis,
    pub fct: &'a FctDefinition,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub analysis: &'a mut AnalysisData,
    pub ast: &'a ast::Function,
    pub symtable: &'a mut ModuleSymTable,
    pub in_loop: bool,
    pub self_available: bool,
    pub vars: &'a mut VarManager,
    pub contains_lambda: bool,
    pub outer_context_access_in_function: bool,
    pub outer_context_access_from_lambda: bool,
}

impl<'a> TypeCheck<'a> {
    pub fn check(&mut self) {
        let start_level = self.symtable.levels();
        self.symtable.push_level();
        self.vars.enter_function();

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
        assert_eq!(self.symtable.levels(), start_level);

        self.prepare_local_and_context_vars();
    }

    fn prepare_local_and_context_vars(&mut self) {
        if self.needs_context() {
            self.setup_context_class();
        }

        // Store var definitions for all local and context vars defined in this function.
        self.analysis.vars = self.vars.leave_function();

        self.analysis.outer_context_access =
            Some(self.outer_context_access_in_function || self.outer_context_access_from_lambda);
    }

    fn needs_context(&self) -> bool {
        // As soon as this function has context variables,
        // it definitely needs a Context object.
        if self.vars.has_context_vars() {
            return true;
        }

        // We also need a Context object, when any lambda
        // defined in this function, accesses some outside
        // context variables.
        self.outer_context_access_from_lambda
    }

    fn setup_context_class(&mut self) {
        let function = self.vars.current_function();
        let start_index = function.start_idx;
        let number_fields = function.next_context_id;
        let mut fields = Vec::with_capacity(number_fields);
        let mut map: Vec<Option<NestedVarId>> = vec![None; number_fields];

        let needs_outer_context_slot = self.fct.is_lambda()
            && (self.outer_context_access_in_function || self.outer_context_access_from_lambda);

        if needs_outer_context_slot {
            let name = self.sa.interner.intern("outer_context");

            fields.push(Field {
                id: FieldId(0),
                name,
                ty: SourceType::Ptr,
                mutable: true,
                visibility: Visibility::Module,
            });
        }

        for var in self.vars.vars.iter().skip(start_index) {
            if !var.location.is_context() {
                continue;
            }

            match var.location {
                VarLocation::Context(field_id) => {
                    let ContextIdx(field_id) = field_id;
                    map[field_id] = Some(var.id);
                }
                VarLocation::Stack => {}
            }
        }

        for var_id in map {
            let var_id = var_id.expect("missing field");
            let var = self.vars.get_var(var_id);

            let id = FieldId(fields.len());

            fields.push(Field {
                id,
                name: var.name,
                ty: var.ty.clone(),
                mutable: true,
                visibility: Visibility::Module,
            });
        }

        let mut name = self.fct.display_name(self.sa);
        name.push_str("$Context");

        let name = self.sa.interner.intern(&name);

        let mut class = ClassDefinition::new_without_source(
            self.package_id,
            self.module_id,
            Some(self.file_id),
            Some(self.ast.pos),
            name,
            Visibility::from_ast(self.ast.visibility),
            fields,
        );
        class.type_params = Some(self.fct.type_params.clone());
        let class_id = self.sa.classes.push(class);
        self.analysis.context_cls_id = Some(class_id);

        self.analysis.context_has_outer_context_slot = Some(needs_outer_context_slot);
    }

    fn add_type_params(&mut self) {
        for (id, name) in self.fct.type_params.names() {
            self.symtable.insert(name, Sym::TypeParam(id));
        }
    }

    fn add_params(&mut self) {
        self.add_hidden_parameter_self();

        let self_count = if self.fct.has_self() { 1 } else { 0 };
        assert_eq!(
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
            let ty = if self.fct.is_variadic && ind == self.ast.params.len() - 1 {
                // type of variable is Array[T]
                self.sa.known.array_ty(ty.clone())
            } else {
                ty.clone()
            };

            let var_id = self.vars.add_var(param.name, ty, false);
            self.analysis
                .map_vars
                .insert(param.id, self.vars.local_var_id(var_id));

            // params are only allowed to replace functions, vars cannot be replaced
            let replaced_sym = self.symtable.insert(param.name, Sym::Var(var_id));
            if let Some(replaced_sym) = replaced_sym {
                report_sym_shadow(
                    self.sa,
                    param.name,
                    self.fct.file_id,
                    param.pos,
                    replaced_sym,
                )
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        if !self.fct.has_self() {
            return;
        }

        let self_ty = self.fct.param_types[0].clone();

        // The lambda-object isn't available through `self` in lambdas.
        if !self.fct.is_lambda() {
            assert!(!self.self_available);
            self.self_available = true;
        }

        let name = self.sa.interner.intern("self");

        assert!(!self.vars.has_local_vars());
        let var_id = self.vars.add_var(name, self_ty, false);
        if !self.fct.is_lambda() {
            assert_eq!(NestedVarId(0), var_id);
        }
    }

    fn add_local(&mut self, id: NestedVarId, pos: Position) {
        let name = self.vars.get_var(id).name;
        match self.symtable.insert(name, Sym::Var(id)) {
            Some(Sym::Var(_)) | None => {}
            Some(sym) => report_sym_shadow(self.sa, name, self.fct.file_id, pos, sym),
        }
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

        if !defined_type.is_error() && !defined_type.is_defined_type(self.sa) {
            let tyname = self
                .sa
                .interner
                .str(s.pattern.to_name().unwrap())
                .to_string();
            self.sa
                .diag
                .lock()
                .report(self.file_id, s.pos, ErrorMessage::VarNeedsTypeInfo(tyname));

            return;
        }

        // update type of variable, necessary when stmt has initializer expression but no type
        self.check_stmt_let_pattern(&s.pattern, defined_type.clone(), s.mutable);

        if s.expr.is_some() {
            if !expr_type.is_error()
                && !defined_type.is_error()
                && !defined_type.allows(self.sa, expr_type.clone())
            {
                let name = self
                    .sa
                    .interner
                    .str(s.pattern.to_name().unwrap())
                    .to_string();
                let defined_type = defined_type.name_fct(self.sa, self.fct);
                let expr_type = expr_type.name_fct(self.sa, self.fct);
                let msg = ErrorMessage::AssignType(name, defined_type, expr_type);
                self.sa.diag.lock().report(self.file_id, s.pos, msg);
            }

        // let variable binding needs to be assigned
        } else {
            self.sa
                .diag
                .lock()
                .report(self.file_id, s.pos, ErrorMessage::LetMissingInitialization);
        }
    }

    fn read_type(&mut self, t: &ast::Type) -> SourceType {
        read_type(
            self.sa,
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
                let var_id = self.vars.add_var(ident.name, ty, mutable);

                self.add_local(var_id, ident.pos);
                self.analysis
                    .map_vars
                    .insert(ident.id, self.vars.local_var_id(var_id));
            }

            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }

            ast::LetPattern::Tuple(ref tuple) => {
                if !ty.is_tuple_or_unit() && !ty.is_error() {
                    let ty_name = ty.name_fct(self.sa, self.fct);
                    self.sa.diag.lock().report(
                        self.file_id,
                        tuple.pos,
                        ErrorMessage::LetPatternExpectedTuple(ty_name),
                    );
                    return;
                }

                if ty.is_unit() {
                    // () doesn't have any subparts
                    if tuple.parts.len() != 0 {
                        self.sa.diag.lock().report(
                            self.file_id,
                            tuple.pos,
                            ErrorMessage::LetPatternShouldBeUnit,
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

                let subtypes = ty.tuple_subtypes();

                if subtypes.len() != tuple.parts.len() {
                    let ty_name = ty.name_fct(self.sa, self.fct);
                    self.sa.diag.lock().report(
                        self.file_id,
                        tuple.pos,
                        ErrorMessage::LetPatternExpectedTupleWithLength(
                            ty_name,
                            subtypes.len(),
                            tuple.parts.len(),
                        ),
                    );
                    return;
                }

                for (part, subtype) in tuple.parts.iter().zip(subtypes.iter()) {
                    self.check_stmt_let_pattern(&*part, subtype.clone(), mutable);
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
                self.type_supports_iterator_protocol(iterator_type.clone())
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

        let name = object_type.name_fct(self.sa, self.fct);
        let msg = ErrorMessage::TypeNotUsableInForIn(name);
        self.sa
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
    ) -> Option<(FctDefinitionId, SourceType)> {
        let make_iterator_name = self.sa.interner.intern("iterator");

        let mut lookup = MethodLookup::new(self.sa, self.fct)
            .no_error_reporting()
            .method(object_type)
            .name(make_iterator_name)
            .type_param_defs(&self.fct.type_params)
            .arg_types(&[]);

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
        let next_name = self.sa.interner.intern("next");

        let mut next = MethodLookup::new(self.sa, self.fct)
            .no_error_reporting()
            .method(object_type.clone())
            .name(next_name)
            .type_param_defs(&self.fct.type_params)
            .arg_types(&[]);

        if !next.find() {
            return None;
        }

        let next_result_type = next.found_ret().unwrap();

        let value_type = if let SourceType::Enum(enum_id, type_params) = next_result_type.clone() {
            if enum_id == self.sa.known.enums.option() {
                assert_eq!(type_params.len(), 1);
                type_params[0].clone()
            } else {
                return None;
            }
        } else {
            return None;
        };

        Some((
            ForTypeInfo {
                make_iterator: None,
                next: next.found_fct_id().expect("fct_id missing"),
                iterator_type: object_type,
                next_type: next_result_type,
                value_type: value_type.clone(),
            },
            value_type,
        ))
    }

    fn check_stmt_while(&mut self, stmt: &ast::StmtWhileType) {
        let expr_type = self.check_expr(&stmt.cond, SourceType::Any);

        if !expr_type.is_error() && !expr_type.is_bool() {
            let expr_type = expr_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::WhileCondType(expr_type);
            self.sa.diag.lock().report(self.file_id, stmt.pos, msg);
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

        if !expr_type.is_error() && !fct_type.allows(self.sa, expr_type.clone()) {
            let fct_type = fct_type.name_fct(self.sa, self.fct);
            let expr_type = expr_type.name_fct(self.sa, self.fct);

            let msg = ErrorMessage::ReturnType(fct_type, expr_type);

            self.sa.diag.lock().report(self.file_id, pos, msg);
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

        let ty = create_tuple(self.sa, subtypes);
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
            self.sa
                .diag
                .lock()
                .report(self.file_id, node.pos, ErrorMessage::EnumExpected);
        }

        let expr_enum_id = expr_type.enum_id();
        let expr_type_params = expr_type.type_params();

        let enum_variants = if let Some(expr_enum_id) = expr_enum_id {
            let enum_ = self.sa.enums[expr_enum_id].read();
            enum_.variants.len()
        } else {
            0
        };

        let mut used_variants = FixedBitSet::with_capacity(enum_variants);

        for case in &node.cases {
            self.symtable.push_level();

            debug_assert_eq!(case.patterns.len(), 1);
            let pattern = case.patterns.first().expect("no pattern");

            match pattern.data {
                ast::MatchPatternData::Underscore => {
                    let mut negated_used_variants = used_variants.clone();
                    negated_used_variants.toggle_range(..);

                    if negated_used_variants.count_ones(..) == 0 {
                        let msg = ErrorMessage::MatchUnreachablePattern;
                        self.sa.diag.lock().report(self.file_id, case.pos, msg);
                    }

                    used_variants.insert_range(..);
                }

                ast::MatchPatternData::Ident(ref ident) => {
                    let sym = self.read_path(&ident.path);

                    let mut used_idents: HashSet<Name> = HashSet::new();

                    match sym {
                        Ok(Sym::EnumVariant(enum_id, variant_idx)) => {
                            if Some(enum_id) == expr_enum_id {
                                if used_variants.contains(variant_idx) {
                                    let msg = ErrorMessage::MatchUnreachablePattern;
                                    self.sa.diag.lock().report(self.file_id, case.pos, msg);
                                }

                                used_variants.insert(variant_idx);
                                self.analysis.map_idents.insert(
                                    pattern.id,
                                    IdentType::EnumValue(
                                        enum_id,
                                        expr_type_params.clone(),
                                        variant_idx,
                                    ),
                                );

                                let enum_ = self.sa.enums.idx(enum_id);
                                let enum_ = enum_.read();
                                let variant = &enum_.variants[variant_idx];

                                let given_params = if let Some(ref params) = ident.params {
                                    params.len()
                                } else {
                                    0
                                };

                                if given_params == 0 && ident.params.is_some() {
                                    let msg = ErrorMessage::MatchPatternNoParens;
                                    self.sa.diag.lock().report(self.file_id, case.pos, msg);
                                }

                                let expected_params = variant.types.len();

                                if given_params != expected_params {
                                    let msg = ErrorMessage::MatchPatternWrongNumberOfParams(
                                        given_params,
                                        expected_params,
                                    );
                                    self.sa.diag.lock().report(self.file_id, case.pos, msg);
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
                                                self.sa,
                                                ty,
                                                &expr_type_params,
                                                None,
                                            );

                                            if used_idents.insert(name) == false {
                                                let msg = ErrorMessage::VarAlreadyInPattern;
                                                self.sa.diag.lock().report(
                                                    self.file_id,
                                                    param.pos,
                                                    msg,
                                                );
                                            }

                                            let var_id = self.vars.add_var(name, ty, false);
                                            self.add_local(var_id, param.pos);
                                            self.analysis
                                                .map_vars
                                                .insert(param.id, self.vars.local_var_id(var_id));
                                        }
                                    }
                                }
                            } else {
                                let msg = ErrorMessage::EnumVariantExpected;
                                self.sa.diag.lock().report(self.file_id, node.pos, msg);
                            }
                        }

                        Ok(_) => {
                            let msg = ErrorMessage::EnumVariantExpected;
                            self.sa.diag.lock().report(self.file_id, node.pos, msg);
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
            } else if !result_type.allows(self.sa, case_ty.clone()) {
                let result_type_name = result_type.name_fct(self.sa, self.fct);
                let case_ty_name = case_ty.name_fct(self.sa, self.fct);
                let msg =
                    ErrorMessage::MatchBranchTypesIncompatible(result_type_name, case_ty_name);
                self.sa
                    .diag
                    .lock()
                    .report(self.file_id, case.value.pos(), msg);
            }

            self.symtable.pop_level();
        }

        used_variants.toggle_range(..);

        if used_variants.count_ones(..) != 0 {
            let msg = ErrorMessage::MatchUncoveredVariant;
            self.sa.diag.lock().report(self.file_id, node.pos, msg);
        }

        self.analysis.set_ty(node.id, result_type.clone());

        result_type
    }

    fn check_expr_if(&mut self, expr: &ast::ExprIfType, expected_ty: SourceType) -> SourceType {
        let expr_type = self.check_expr(&expr.cond, SourceType::Any);

        if !expr_type.is_bool() && !expr_type.is_error() {
            let expr_type = expr_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::IfCondType(expr_type);
            self.sa.diag.lock().report(self.file_id, expr.pos, msg);
        }

        let then_type = self.check_expr(&expr.then_block, expected_ty.clone());

        let merged_type = if let Some(ref else_block) = expr.else_block {
            let else_type = self.check_expr(else_block, expected_ty);

            if expr_always_returns(&expr.then_block) {
                else_type
            } else if expr_always_returns(else_block) {
                then_type
            } else if then_type.is_error() {
                else_type
            } else if else_type.is_error() {
                then_type
            } else if !then_type.allows(self.sa, else_type.clone()) {
                let then_type_name = then_type.name_fct(self.sa, self.fct);
                let else_type_name = else_type.name_fct(self.sa, self.fct);
                let msg = ErrorMessage::IfBranchTypesIncompatible(then_type_name, else_type_name);
                self.sa.diag.lock().report(self.file_id, expr.pos, msg);
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
            Some(Sym::Var(var_id)) => {
                let ty = self.vars.get_var(var_id).ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                // Variable may have to be context-allocated.
                let ident = self
                    .vars
                    .check_context_allocated(var_id, &mut self.outer_context_access_in_function);
                self.analysis.map_idents.insert(e.id, ident);

                ty
            }

            Some(Sym::Global(globalid)) => {
                let global_var = self.sa.globals.idx(globalid);
                let ty = global_var.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(globalid));

                ty
            }

            Some(Sym::Const(const_id)) => {
                let const_ = self.sa.consts.idx(const_id);
                let const_ = const_.read();

                self.analysis.set_ty(e.id, const_.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                const_.ty.clone()
            }

            Some(Sym::EnumVariant(enum_id, variant_idx)) => self.check_enum_value_without_args_id(
                e.id,
                e.pos,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_idx,
            ),

            None => {
                let name = self.sa.interner.str(e.name).to_string();
                self.sa.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    ErrorMessage::UnknownIdentifier(name),
                );
                SourceType::Error
            }

            _ => {
                self.sa
                    .diag
                    .lock()
                    .report(self.fct.file_id, e.pos, ErrorMessage::ValueExpected);
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
            self.sa
                .diag
                .lock()
                .report(self.file_id, e.pos, ErrorMessage::LvalueExpected);
        }

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    fn check_expr_assign_ident(&mut self, e: &ast::ExprBinType) {
        self.analysis.set_ty(e.id, SourceType::Unit);

        let lhs_ident = e.lhs.to_ident().unwrap();
        let sym = self.symtable.get(lhs_ident.name);

        let lhs_type = match sym {
            Some(Sym::Var(var_id)) => {
                if !self.vars.get_var(var_id).mutable {
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, ErrorMessage::LetReassigned);
                }

                // Variable may have to be context-allocated.
                let ident = self
                    .vars
                    .check_context_allocated(var_id, &mut self.outer_context_access_in_function);
                self.analysis.map_idents.insert(e.lhs.id(), ident);

                self.vars.get_var(var_id).ty.clone()
            }

            Some(Sym::Global(global_id)) => {
                let global_var = self.sa.globals.idx(global_id);
                let global_var = global_var.read();

                if !e.initializer && !global_var.mutable {
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, ErrorMessage::LetReassigned);
                }

                self.analysis
                    .map_idents
                    .insert(e.lhs.id(), IdentType::Global(global_id));
                global_var.ty.clone()
            }

            None => {
                let name = self.sa.interner.str(lhs_ident.name).to_string();
                self.sa.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    ErrorMessage::UnknownIdentifier(name),
                );

                return;
            }

            _ => {
                self.sa.diag.lock().report(
                    self.fct.file_id,
                    lhs_ident.pos,
                    ErrorMessage::LvalueExpected,
                );

                return;
            }
        };

        let rhs_type = self.check_expr(&e.rhs, lhs_type.clone());

        if !lhs_type.is_error()
            && !rhs_type.is_error()
            && !lhs_type.allows(self.sa, rhs_type.clone())
        {
            let ident = e.lhs.to_ident().unwrap();
            let name = self.sa.interner.str(ident.name).to_string();
            let lhs_type = lhs_type.name_fct(self.sa, self.fct);
            let rhs_type = rhs_type.name_fct(self.sa, self.fct);

            self.analysis.set_ty(e.id, SourceType::Unit);

            let msg = ErrorMessage::AssignType(name, lhs_type, rhs_type);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }
    }

    fn check_expr_assign_call(&mut self, e: &ast::ExprBinType) {
        let call = e.lhs.to_call().unwrap();
        let expr_type = self.check_expr(&call.callee, SourceType::Any);

        let mut arg_types: Vec<SourceType> = call
            .args
            .iter()
            .map(|arg| self.check_expr(&arg.expr, SourceType::Any))
            .collect();

        let value_type = self.check_expr(&e.rhs, SourceType::Any);

        let name = self.sa.interner.intern("set");
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
                let msg = ErrorMessage::NameExpected;
                self.sa.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return;
            }
        };

        let object_type = self.check_expr(&field_expr.lhs, SourceType::Any);

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.sa, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let cls = self.sa.classes.idx(cls_ty.cls_id().expect("no class"));
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let class_type_params = cls_ty.type_params();

                let fty = replace_type_param(self.sa, field.ty.clone(), &class_type_params, None);

                if !e.initializer && !field.mutable {
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, e.pos, ErrorMessage::LetReassigned);
                }

                let rhs_type = self.check_expr(&e.rhs, fty.clone());

                if !fty.allows(self.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = self.sa.interner.str(name).to_string();

                    let object_type = object_type.name_fct(self.sa, self.fct);
                    let lhs_type = fty.name_fct(self.sa, self.fct);
                    let rhs_type = rhs_type.name_fct(self.sa, self.fct);

                    let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, SourceType::Unit);
                return;
            }
        }

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        self.check_expr(&e.rhs, SourceType::Any);

        // field not found, report error
        let field_name = self.sa.interner.str(name).to_string();
        let expr_name = object_type.name_fct(self.sa, self.fct);
        let msg = ErrorMessage::UnknownField(field_name, expr_name);
        self.sa
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
            self.sa,
            object_type.clone(),
            &self.fct.type_params,
            is_static,
            name,
            args,
            fct_type_params,
        );

        if descriptor.is_none() {
            let type_name = object_type.name_fct(self.sa, self.fct);
            let name = self.sa.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<String>>();
            let msg = if is_static {
                ErrorMessage::UnknownStaticMethod(type_name, name, param_names)
            } else {
                ErrorMessage::UnknownMethod(type_name, name, param_names)
            };

            self.sa.diag.lock().report(self.file_id, pos, msg);
        }

        descriptor
    }

    fn check_expr_un(&mut self, e: &ast::ExprUnType, expected_ty: SourceType) -> SourceType {
        if e.op == ast::UnOp::Neg && e.opnd.is_lit_int() {
            let expr_type =
                self.check_expr_lit_int(e.opnd.to_lit_int().unwrap(), true, expected_ty);
            self.analysis.set_ty(e.id, expr_type.clone());
            return expr_type;
        }

        let opnd = self.check_expr(&e.opnd, SourceType::Any);

        match e.op {
            ast::UnOp::Plus => self.check_expr_un_method(e, e.op, "unaryPlus", opnd),
            ast::UnOp::Neg => self.check_expr_un_method(e, e.op, "unaryMinus", opnd),
        }
    }

    fn check_expr_un_method(
        &mut self,
        e: &ast::ExprUnType,
        op: ast::UnOp,
        name: &str,
        ty: SourceType,
    ) -> SourceType {
        let name = self.sa.interner.intern(name);
        let call_types = [];

        if !ty.is_error() {
            if let Some(descriptor) = lookup_method(
                self.sa,
                ty.clone(),
                &self.fct.type_params,
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

            let ty = ty.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::UnOpType(op.as_str().into(), ty);

            self.sa.diag.lock().report(self.file_id, e.pos, msg);
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
            ast::BinOp::BitOr => {
                self.check_expr_bin_method(e, e.op, "bitwiseOr", lhs_type, rhs_type)
            }
            ast::BinOp::BitAnd => {
                self.check_expr_bin_method(e, e.op, "bitwiseAnd", lhs_type, rhs_type)
            }
            ast::BinOp::BitXor => {
                self.check_expr_bin_method(e, e.op, "bitwiseXor", lhs_type, rhs_type)
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
        let name = self.sa.interner.intern(name);
        let call_types = [rhs_type.clone()];

        if let Some(descriptor) = lookup_method(
            self.sa,
            lhs_type.clone(),
            &self.fct.type_params,
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
            let lhs_type = lhs_type.name_fct(self.sa, self.fct);
            let rhs_type = rhs_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.sa.diag.lock().report(self.file_id, e.pos, msg);

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
                if !lhs_type.allows(self.sa, rhs_type.clone())
                    && !rhs_type.allows(self.sa, lhs_type.clone())
                {
                    let lhs_type = lhs_type.name_fct(self.sa, self.fct);
                    let rhs_type = rhs_type.name_fct(self.sa, self.fct);
                    self.sa.diag.lock().report(
                        self.file_id,
                        e.pos,
                        ErrorMessage::TypesIncompatible(lhs_type, rhs_type),
                    );
                }

                self.analysis.set_ty(e.id, SourceType::Bool);
                return SourceType::Bool;
            }

            ast::CmpOp::Eq | ast::CmpOp::Ne => {
                if is_simple_enum(self.sa, lhs_type.clone()) {
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
        if lhs_type.allows(self.sa, rhs_type.clone()) {
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
            let lhs_type = lhs_type.name_fct(self.sa, self.fct);
            let rhs_type = rhs_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::BinOpType("equals".into(), lhs_type, rhs_type);

            self.sa.diag.lock().report(self.file_id, e.pos, msg);

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
        if !expected_type.allows(self.sa, lhs_type.clone())
            || !expected_type.allows(self.sa, rhs_type.clone())
        {
            let op = op.as_str().into();
            let lhs_type = lhs_type.name_fct(self.sa, self.fct);
            let rhs_type = rhs_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

            self.sa.diag.lock().report(self.file_id, e.pos, msg);
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
            .map(|arg| self.check_expr(&arg.expr, SourceType::Any))
            .collect();

        if let Some(expr_ident) = callee.to_ident() {
            let sym = self.symtable.get(expr_ident.name);

            self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, &arg_types)
        } else if let Some(expr_dot) = callee.to_dot() {
            let object_type = self.check_expr(&expr_dot.lhs, SourceType::Any);

            let method_name = match expr_dot.rhs.to_ident() {
                Some(ident) => ident.name,

                None => {
                    let msg = ErrorMessage::NameExpected;
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    return SourceType::Error;
                }
            };
            self.check_expr_call_method(e, object_type, method_name, type_params, &arg_types)
        } else if let Some(_expr_path) = callee.to_path() {
            self.check_expr_call_path(e, expected_ty, callee, type_params, &arg_types)
        } else {
            if !type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                self.sa
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
                self.check_expr_call_class(e, expected_ty, cls_id, type_params, &arg_types)
            }

            Some(Sym::Struct(struct_id)) => {
                self.check_expr_call_struct(e, struct_id, type_params, &arg_types)
            }

            Some(Sym::EnumVariant(enum_id, variant_idx)) => self.check_enum_value_with_args(
                e,
                expected_ty,
                enum_id,
                type_params,
                variant_idx,
                &arg_types,
            ),

            _ => {
                if !type_params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa
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
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant_idx: usize,
        arg_types: &[SourceType],
    ) -> SourceType {
        let enum_ = self.sa.enums.idx(enum_id);
        let enum_ = enum_.read();
        let variant = &enum_.variants[variant_idx as usize];

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.sa,
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
            let enum_name = self.sa.interner.str(enum_.name).to_string();
            let variant_name = self.sa.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name_enum(self.sa, &*enum_))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::EnumArgsIncompatible(
                enum_name,
                variant_name,
                variant_types,
                arg_types,
            );
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        } else if variant.types.is_empty() {
            let enum_name = self.sa.interner.str(enum_.name).to_string();
            let variant_name = self.sa.interner.str(variant.name).to_string();
            let msg = ErrorMessage::EnumArgsNoParens(enum_name, variant_name);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let ty = SourceType::Enum(enum_id, type_params);

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Enum(ty.clone(), variant_idx)));

        self.analysis.set_ty(e.id, ty.clone());
        ty
    }

    fn check_expr_call_enum_args(
        &mut self,
        _enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant: &EnumVariant,
        arg_types: &[SourceType],
    ) -> bool {
        if variant.types.len() != arg_types.len() {
            return false;
        }

        for (def_ty, arg_ty) in variant.types.iter().zip(arg_types) {
            let def_ty = replace_type_param(self.sa, def_ty.clone(), &type_params, None);

            if !def_ty.allows(self.sa, arg_ty.clone()) {
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

        for trait_ty in self.fct.type_params.bounds_for_type_param(tp_id) {
            let trait_id = trait_ty.trait_id().expect("trait expected");
            let trait_ = self.sa.traits[trait_id].read();

            if let Some(fct_id) = trait_.find_method(self.sa, name, true) {
                fcts.push((trait_id, fct_id));
            }
        }

        if fcts.len() != 1 {
            let msg = if fcts.len() > 1 {
                ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam
            } else {
                ErrorMessage::UnknownStaticMethodWithTypeParam
            };

            self.sa.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        if arg_types.contains(&SourceType::Error) {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let (trait_id, fct_id) = fcts[0];
        let fct = self.sa.fcts.idx(fct_id);
        let fct = fct.read();

        let tp = SourceType::TypeParam(tp_id);

        if !args_compatible_fct(
            self.sa,
            &*fct,
            arg_types,
            &SourceTypeArray::empty(),
            Some(tp.clone()),
        ) {
            let fct_name = self.sa.interner.str(name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::ParamTypesIncompatible(fct_name, fct_params, arg_types);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let call_type = CallType::GenericStaticMethod(tp_id, trait_id, fct_id);
        self.analysis.map_calls.insert(e.id, Arc::new(call_type));

        let return_type = replace_type_param(
            self.sa,
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

        if expr_type.is_lambda() {
            return self.check_expr_call_expr_lambda(e, expr_type, arg_types);
        }

        let get = self.sa.interner.intern("get");

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

    fn check_expr_call_expr_lambda(
        &mut self,
        e: &ast::ExprCallType,
        expr_type: SourceType,
        arg_types: &[SourceType],
    ) -> SourceType {
        let (params, return_type) = expr_type.to_lambda().expect("lambda expected");

        // Type params are mapped to themselves.
        let type_params_count = self.fct.type_params.len();
        let type_params = (0..type_params_count)
            .into_iter()
            .map(|idx| SourceType::TypeParam(TypeParamId(idx)))
            .collect::<Vec<SourceType>>();
        let type_params = SourceTypeArray::with(type_params);

        if !args_compatible(
            self.sa,
            params.types(),
            false,
            arg_types,
            &type_params,
            None,
        ) {
            let fct_params = params
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::LambdaParamTypesIncompatible(fct_params, arg_types);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let call_type = CallType::Lambda(params, return_type.clone());

        self.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));

        self.analysis.set_ty(e.id, return_type.clone());
        return_type
    }

    fn check_expr_call_fct(
        &mut self,
        e: &ast::ExprCallType,
        fct_id: FctDefinitionId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        if !fct_accessible_from(self.sa, fct_id, self.module_id) {
            let fct = self.sa.fcts.idx(fct_id);
            let fct = fct.read();
            let msg = ErrorMessage::NotAccessible(fct.display_name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let mut lookup = MethodLookup::new(self.sa, self.fct)
            .pos(e.pos)
            .callee(fct_id)
            .arg_types(&arg_types)
            .arg_names(e.arg_names())
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
        let mut lookup = MethodLookup::new(self.sa, self.fct)
            .pos(e.pos)
            .static_method(object_type)
            .name(method_name)
            .arg_types(arg_types)
            .arg_names(e.arg_names())
            .fct_type_params(&fct_type_params)
            .type_param_defs(&self.fct.type_params);

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();
            let container_type_params = lookup.found_container_type_params().unwrap();
            let type_params = container_type_params.connect(&fct_type_params);
            let call_type = Arc::new(CallType::Fct(fct_id, type_params));
            self.analysis.map_calls.insert(e.id, call_type.clone());

            if !method_accessible_from(self.sa, fct_id, self.module_id) {
                let fct = self.sa.fcts.idx(fct_id);
                let fct = fct.read();

                let name = fct.display_name(self.sa);
                let msg = ErrorMessage::NotAccessible(name);
                self.sa.diag.lock().report(self.file_id, e.pos, msg);
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

        let mut lookup = MethodLookup::new(self.sa, self.fct)
            .no_error_reporting()
            .method(object_type.clone())
            .name(method_name)
            .fct_type_params(&fct_type_params)
            .type_param_defs(&self.fct.type_params)
            .arg_types(arg_types)
            .arg_names(e.arg_names());

        if lookup.find() {
            let fct_id = lookup.found_fct_id().unwrap();
            let return_type = lookup.found_ret().unwrap();

            let call_type = if object_type.is_trait() {
                CallType::TraitObjectMethod(object_type, fct_id)
            } else {
                let method_type = lookup.found_class_type().unwrap();
                let container_type_params = lookup.found_container_type_params().clone().unwrap();
                let type_params = container_type_params.connect(&fct_type_params);
                CallType::Method(method_type, fct_id, type_params)
            };

            self.analysis
                .map_calls
                .insert_or_replace(e.id, Arc::new(call_type));
            self.analysis.set_ty(e.id, return_type.clone());

            if !method_accessible_from(self.sa, fct_id, self.module_id) {
                let fct = self.sa.fcts.idx(fct_id);
                let fct = fct.read();

                let name = fct.display_name(self.sa);
                let msg = ErrorMessage::NotAccessible(name);
                self.sa.diag.lock().report(self.file_id, e.pos, msg);
            }

            return_type
        } else if lookup.found_fct_id().is_none() {
            // No method with this name found, so this might actually be a field
            self.check_expr_call_field(e, object_type, method_name, fct_type_params, arg_types)
        } else {
            // Lookup the method again, but this time with error reporting
            let mut lookup = MethodLookup::new(self.sa, self.fct)
                .method(object_type)
                .name(method_name)
                .fct_type_params(&fct_type_params)
                .type_param_defs(&self.fct.type_params)
                .pos(e.pos)
                .arg_types(arg_types)
                .arg_names(e.arg_names());

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
            find_field_in_class(self.sa, object_type.clone(), method_name)
        {
            self.analysis.set_ty(e.callee.id(), field_type.clone());
            self.analysis
                .map_idents
                .insert_or_replace(e.callee.id(), IdentType::Field(actual_type, field_id));

            let cls_id = object_type.cls_id().expect("class expected");

            if !class_field_accessible_from(self.sa, cls_id, field_id, self.module_id) {
                let cls = self.sa.classes.idx(cls_id);
                let cls = cls.read();
                let field = &cls.fields[field_id];

                let name = self.sa.interner.str(field.name).to_string();
                let msg = ErrorMessage::NotAccessible(name);
                self.sa.diag.lock().report(self.file_id, e.pos, msg);
            }

            return self.check_expr_call_expr(e, field_type, arg_types);
        }

        if let Some(struct_id) = object_type.struct_id() {
            let struct_ = self.sa.structs.idx(struct_id);
            let struct_ = struct_.read();
            if let Some(&field_id) = struct_.field_names.get(&method_name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &struct_.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params();
                let field_type =
                    replace_type_param(self.sa, field.ty.clone(), &struct_type_params, None);

                if !struct_field_accessible_from(self.sa, struct_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, field_type.clone());
                return self.check_expr_call_expr(e, field_type, arg_types);
            }
        }

        // No field with that name as well, so report method
        let mut lookup = MethodLookup::new(self.sa, self.fct)
            .method(object_type)
            .name(method_name)
            .fct_type_params(&type_params)
            .type_param_defs(&self.fct.type_params)
            .pos(e.pos)
            .arg_types(arg_types)
            .arg_names(e.arg_names());
        assert!(!lookup.find());

        self.analysis.set_ty(e.id, SourceType::Error);

        SourceType::Error
    }

    fn check_expr_call_struct(
        &mut self,
        e: &ast::ExprCallType,
        struct_id: StructDefinitionId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        let is_struct_accessible = struct_accessible_from(self.sa, struct_id, self.module_id);

        if !is_struct_accessible {
            let struct_ = self.sa.structs.idx(struct_id);
            let struct_ = struct_.read();
            let msg = ErrorMessage::NotAccessible(struct_.name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let struct_ = self.sa.structs.idx(struct_id);
        let struct_ = struct_.read();

        if !is_default_accessible(self.sa, struct_.module_id, self.module_id)
            && !struct_.all_fields_are_public()
            && is_struct_accessible
        {
            let msg = ErrorMessage::StructConstructorNotAccessible(struct_.name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let ty = SourceType::Struct(struct_id, type_params.clone());
        let type_params_ok = typeparamck::check_struct(
            self.sa,
            self.fct,
            struct_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.pos),
        );

        if !type_params_ok {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let field_names: Vec<Name> = struct_.fields.iter().map(|f| f.name).collect();
        let arg_names = &e.arg_names();
        if !arg_names_valid(&field_names, arg_names) {
            let struct_1 = &*struct_;
            let struct_name = struct_1.name(self.sa);
            let param_names = struct_1
                .fields
                .iter()
                .map(|f| self.sa.interner.str(f.name).to_string());
            let param_type_names = struct_1
                .fields
                .iter()
                .map(|field| field.ty.name_struct(self.sa, struct_1));
            let msg = self.argument_name_mismatch_message(
                struct_name,
                param_names,
                param_type_names,
                arg_names,
                arg_types,
            );
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }
        if !self.check_expr_call_struct_args(&*struct_, type_params.clone(), arg_types) {
            let struct_name = self.sa.interner.str(struct_.name).to_string();
            let field_types = struct_
                .fields
                .iter()
                .map(|field| field.ty.name_struct(self.sa, &*struct_))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::StructArgsIncompatible(struct_name, field_types, arg_types);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Struct(struct_id, type_params)));

        self.analysis.set_ty(e.id, ty.clone());
        ty
    }

    fn check_expr_call_struct_args(
        &mut self,
        struct_: &StructDefinition,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> bool {
        if struct_.fields.len() != arg_types.len() {
            return false;
        }

        for (def_ty, arg_ty) in struct_.fields.iter().zip(arg_types) {
            let def_ty = replace_type_param(self.sa, def_ty.ty.clone(), &type_params, None);

            if !def_ty.allows(self.sa, arg_ty.clone()) {
                return false;
            }
        }

        true
    }

    fn check_expr_call_class(
        &mut self,
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        cls_id: ClassDefinitionId,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        let is_class_accessible = class_accessible_from(self.sa, cls_id, self.module_id);

        if !is_class_accessible {
            let cls = self.sa.classes.idx(cls_id);
            let cls = cls.read();
            let msg = ErrorMessage::NotAccessible(cls.name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let type_params = if expected_ty.is_cls_id(cls_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        if !typeparamck::check_class(
            self.sa,
            self.fct,
            cls_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.pos),
        ) {
            return SourceType::Error;
        };

        let cls = self.sa.classes.idx(cls_id);
        let cls = cls.read();

        let cls_ty = self.sa.cls_with_type_list(cls_id, type_params.clone());

        if !is_default_accessible(self.sa, cls.module_id, self.module_id)
            && !cls.all_fields_are_public()
            && is_class_accessible
        {
            let msg = ErrorMessage::ClassConstructorNotAccessible(cls.name(self.sa));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        let field_names: Vec<Name> = cls.fields.iter().map(|f| f.name).collect();
        let arg_names = &e.arg_names();
        if !arg_names_valid(&field_names, arg_names) {
            let cls = &*cls;
            let class_name = cls.name(self.sa);
            let param_names = cls
                .fields
                .iter()
                .map(|f| self.sa.interner.str(f.name).to_string());
            let param_type_names = cls
                .fields
                .iter()
                .map(|field| field.ty.name_cls(self.sa, cls));
            let msg = self.argument_name_mismatch_message(
                class_name,
                param_names,
                param_type_names,
                arg_names,
                arg_types,
            );
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }
        if !self.check_expr_call_class_args(&*cls, type_params.clone(), arg_types) {
            let class_name = cls.name(self.sa);
            let param_type_names = cls
                .fields
                .iter()
                .map(|field| field.ty.name_cls(self.sa, &*cls))
                .collect::<Vec<_>>();
            let arg_type_names = arg_types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let msg =
                ErrorMessage::ParamTypesIncompatible(class_name, param_type_names, arg_type_names);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
        }

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Class2Ctor(cls.id(), type_params)));

        self.analysis.set_ty(e.id, cls_ty.clone());
        cls_ty
    }

    fn argument_name_mismatch_message(
        &self,
        name: String,
        param_names: impl Iterator<Item = String>,
        param_type_names: impl Iterator<Item = String>,
        arg_names: &Vec<&Option<Name>>,
        arg_types: &[SourceType],
    ) -> ErrorMessage {
        let params = param_names
            .zip(param_type_names)
            .map(|(n, t)| n + ": " + &*t)
            .collect();
        let arg_names = arg_names
            .iter()
            .map(|on| on.map(|n| self.sa.interner.str(n).to_string()));
        let arg_type_names = arg_types.iter().map(|a| a.name_fct(self.sa, self.fct));
        let args = arg_names
            .zip(arg_type_names)
            .map(|(arg_name, type_name)| {
                if arg_name.is_some() {
                    arg_name.unwrap() + ": " + type_name.as_str()
                } else {
                    type_name
                }
            })
            .collect();
        ErrorMessage::ArgumentNameMismatch(name, params, args)
    }

    fn check_expr_call_class_args(
        &mut self,
        cls: &ClassDefinition,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> bool {
        if cls.fields.len() != arg_types.len() {
            return false;
        }

        for (def_ty, arg_ty) in cls.fields.iter().zip(arg_types) {
            let def_ty = replace_type_param(self.sa, def_ty.ty.clone(), &type_params, None);

            if !def_ty.allows(self.sa, arg_ty.clone()) {
                return false;
            }
        }

        true
    }

    fn check_expr_call_generic(
        &mut self,
        e: &ast::ExprCallType,
        tp_id: TypeParamId,
        name: Name,
        arg_types: &[SourceType],
    ) -> SourceType {
        self.check_expr_call_generic_type_param(
            e,
            SourceType::TypeParam(tp_id),
            tp_id,
            name,
            arg_types,
        )
    }

    fn check_expr_call_generic_type_param(
        &mut self,
        e: &ast::ExprCallType,
        object_type: SourceType,
        id: TypeParamId,
        name: Name,
        args: &[SourceType],
    ) -> SourceType {
        let mut found_fcts = Vec::new();

        for trait_ty in self.fct.type_params.bounds_for_type_param(id) {
            let trait_id = trait_ty.trait_id().expect("trait expected");
            let trait_ = self.sa.traits[trait_id].read();

            if let Some(fid) = trait_.find_method_with_replace(self.sa, false, name, None, args) {
                found_fcts.push(fid);
            }
        }

        if found_fcts.len() == 1 {
            let fid = found_fcts[0];

            let fct = self.sa.fcts.idx(fid);
            let fct = fct.read();
            let return_type = fct.return_type.clone();

            self.analysis.set_ty(e.id, return_type.clone());

            let call_type = CallType::GenericMethod(id, fct.trait_id(), fid);
            self.analysis.map_calls.insert(e.id, Arc::new(call_type));

            return_type
        } else {
            let type_name = object_type.name_fct(self.sa, self.fct);
            let name = self.sa.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<String>>();
            let msg = if found_fcts.len() == 0 {
                ErrorMessage::UnknownMethodForTypeParam(type_name, name, param_names)
            } else {
                ErrorMessage::MultipleCandidatesForTypeParam(type_name, name, param_names)
            };

            self.sa.diag.lock().report(self.file_id, e.pos, msg);

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
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa
                .diag
                .lock()
                .report(self.file_id, method_expr.pos(), msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        };

        match sym {
            Some(Sym::Class(cls_id)) => {
                if typeparamck::check_class(
                    self.sa,
                    self.fct,
                    cls_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.pos),
                ) {
                    self.check_expr_call_static_method(
                        e,
                        SourceType::Class(cls_id, container_type_params),
                        method_name,
                        type_params,
                        &arg_types,
                    )
                } else {
                    SourceType::Error
                }
            }

            Some(Sym::Struct(struct_id)) => {
                let struct_ = self.sa.structs.idx(struct_id);
                let struct_ = struct_.read();

                if typeparamck::check_struct(
                    self.sa,
                    self.fct,
                    struct_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.pos),
                ) {
                    let object_ty = if let Some(ref primitive_ty) = struct_.primitive_ty {
                        assert!(container_type_params.is_empty());
                        primitive_ty.clone()
                    } else {
                        SourceType::Struct(struct_id, container_type_params)
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
                let enum_ = self.sa.enums.idx(enum_id);
                let enum_ = enum_.read();

                if let Some(&variant_idx) = enum_.name_to_value.get(&method_name) {
                    if !container_type_params.is_empty() && !type_params.is_empty() {
                        let msg = ErrorMessage::NoTypeParamsExpected;
                        self.sa
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
                        variant_idx as usize,
                        &arg_types,
                    )
                } else {
                    if typeparamck::check_enum(
                        self.sa,
                        self.fct,
                        enum_id,
                        &container_type_params,
                        ErrorReporting::Yes(self.file_id, e.pos),
                    ) {
                        let object_ty = SourceType::Enum(enum_id, container_type_params);

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
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                self.check_expr_call_generic_static_method(e, id, method_name, &arg_types)
            }

            Some(Sym::Module(module_id)) => {
                if !container_type_params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, callee_as_path.lhs.pos(), msg);
                }

                let sym = {
                    let module = &self.sa.modules[module_id].read();
                    let table = module.table.read();

                    table.get(method_name)
                };

                self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, arg_types)
            }

            _ => {
                let msg = ErrorMessage::ClassExpected;
                self.sa.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);

                SourceType::Error
            }
        }
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
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa.diag.lock().report(self.file_id, e.rhs.pos(), msg);
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

            Some(Sym::Module(module_id)) => {
                self.check_expr_path_module(e, expected_ty, module_id, element_name)
            }

            _ => {
                let msg = ErrorMessage::InvalidLeftSideOfSeparator;
                self.sa.diag.lock().report(self.file_id, e.lhs.pos(), msg);

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
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa
                    .diag
                    .lock()
                    .report(self.file_id, expr_path.rhs.pos(), msg);
                return Err(());
            };

            match sym {
                Some(Sym::Module(module_id)) => {
                    let module = &self.sa.modules[module_id].read();
                    let symtable = module.table.read();
                    let sym = symtable.get(element_name);

                    Ok(sym)
                }

                _ => {
                    let msg = ErrorMessage::ExpectedModule;
                    self.sa.diag.lock().report(self.file_id, expr.pos(), msg);
                    Err(())
                }
            }
        } else if let Some(expr_ident) = expr.to_ident() {
            let container_name = expr_ident.name;
            let sym = self.symtable.get(container_name);

            Ok(sym)
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa.diag.lock().report(self.file_id, expr.pos(), msg);
            Err(())
        }
    }

    fn read_path(&mut self, path: &ast::Path) -> Result<Sym, ()> {
        let names = &path.names;
        let mut sym = self.symtable.get(names[0]);

        for &name in &names[1..] {
            match sym {
                Some(Sym::Module(module_id)) => {
                    if !module_accessible_from(self.sa, module_id, self.module_id) {
                        let module = &self.sa.modules[module_id].read();
                        let msg = ErrorMessage::NotAccessible(module.name(self.sa));
                        self.sa.diag.lock().report(self.file_id, path.pos, msg);
                    }

                    let module = &self.sa.modules[module_id].read();
                    let symtable = module.table.read();
                    sym = symtable.get(name);
                }

                Some(Sym::Enum(enum_id)) => {
                    let enum_ = self.sa.enums[enum_id].read();

                    if !enum_accessible_from(self.sa, enum_id, self.module_id) {
                        let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
                        self.sa.diag.lock().report(self.file_id, path.pos, msg);
                    }

                    if let Some(&variant_idx) = enum_.name_to_value.get(&name) {
                        sym = Some(Sym::EnumVariant(enum_id, variant_idx as usize));
                    } else {
                        let name = self.sa.interner.str(name).to_string();
                        self.sa.diag.lock().report(
                            self.file_id.into(),
                            path.pos,
                            ErrorMessage::UnknownEnumVariant(name),
                        );
                        return Err(());
                    }
                }

                Some(_) => {
                    let msg = ErrorMessage::ExpectedModule;
                    self.sa.diag.lock().report(self.file_id, path.pos, msg);
                    return Err(());
                }

                None => {
                    let name = self.sa.interner.str(names[0]).to_string();
                    let msg = ErrorMessage::UnknownIdentifier(name);
                    self.sa.diag.lock().report(self.file_id, path.pos, msg);
                    return Err(());
                }
            }
        }

        if let Some(sym) = sym {
            Ok(sym)
        } else {
            let name = self.sa.interner.str(names[0]).to_string();
            let msg = ErrorMessage::UnknownIdentifier(name);
            self.sa.diag.lock().report(self.file_id, path.pos, msg);

            Err(())
        }
    }

    fn check_expr_path_module(
        &mut self,
        e: &ast::ExprPathType,
        expected_ty: SourceType,
        module_id: ModuleDefinitionId,
        element_name: Name,
    ) -> SourceType {
        let module = &self.sa.modules.idx(module_id);
        let module = module.read();
        let table = module.table.read();

        let sym = table.get(element_name);

        match sym {
            Some(Sym::Global(global_id)) => {
                if !global_accessible_from(self.sa, global_id, self.module_id) {
                    let global = &self.sa.globals.idx(global_id);
                    let global = global.read();
                    let msg = ErrorMessage::NotAccessible(global.name(self.sa));
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                let global_var = self.sa.globals.idx(global_id);
                let ty = global_var.read().ty.clone();
                self.analysis.set_ty(e.id, ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Global(global_id));

                ty
            }

            Some(Sym::Const(const_id)) => {
                if !const_accessible_from(self.sa, const_id, self.module_id) {
                    let const_ = self.sa.consts.idx(const_id);
                    let const_ = const_.read();
                    let msg = ErrorMessage::NotAccessible(const_.name(self.sa));
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                let const_ = self.sa.consts.idx(const_id);
                let const_ = const_.read();

                self.analysis.set_ty(e.id, const_.ty.clone());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                const_.ty.clone()
            }

            Some(Sym::EnumVariant(enum_id, variant_idx)) => self.check_enum_value_without_args_id(
                e.id,
                e.pos,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_idx,
            ),

            None => {
                let module = module.name(self.sa);
                let name = self.sa.interner.str(element_name).to_string();
                self.sa.diag.lock().report(
                    self.fct.file_id,
                    e.pos,
                    ErrorMessage::UnknownIdentifierInModule(module, name),
                );
                SourceType::Error
            }

            _ => {
                self.sa
                    .diag
                    .lock()
                    .report(self.fct.file_id, e.pos, ErrorMessage::ValueExpected);
                SourceType::Error
            }
        }
    }

    fn check_enum_value_without_args(
        &mut self,
        expr_id: ast::NodeId,
        expr_pos: Position,
        _expected_ty: SourceType,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        name: Name,
    ) -> SourceType {
        let enum_ = self.sa.enums[enum_id].read();

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.diag.lock().report(self.file_id, expr_pos, msg);
        }

        let type_params_ok = typeparamck::check_enum(
            self.sa,
            self.fct,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, expr_pos),
        );

        if let Some(&value) = enum_.name_to_value.get(&name) {
            let variant = &enum_.variants[value as usize];

            if !variant.types.is_empty() {
                let enum_name = self.sa.interner.str(enum_.name).to_string();
                let variant_name = self.sa.interner.str(variant.name).to_string();
                let variant_types = variant
                    .types
                    .iter()
                    .map(|a| a.name_enum(self.sa, &*enum_))
                    .collect::<Vec<_>>();
                let arg_types = Vec::new();
                let msg = ErrorMessage::EnumArgsIncompatible(
                    enum_name,
                    variant_name,
                    variant_types,
                    arg_types,
                );
                self.sa.diag.lock().report(self.file_id, expr_pos, msg);
            }

            self.analysis.map_idents.insert(
                expr_id,
                IdentType::EnumValue(enum_id, type_params.clone(), value as usize),
            );
        } else {
            let name = self.sa.interner.str(name).to_string();
            self.sa.diag.lock().report(
                self.file_id,
                expr_pos,
                ErrorMessage::UnknownEnumVariant(name),
            );
        }

        if type_params_ok {
            let ty = SourceType::Enum(enum_id, type_params);

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
                Some(Sym::EnumVariant(enum_id, variant_idx)) => self
                    .check_enum_value_without_args_id(
                        e.id,
                        e.pos,
                        expected_ty,
                        enum_id,
                        type_params,
                        variant_idx,
                    ),

                _ => {
                    self.sa.diag.lock().report(
                        self.file_id,
                        e.pos,
                        ErrorMessage::NoTypeParamsExpected,
                    );

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else if let Some(path) = e.callee.to_path() {
            let container_name = if let Some(container_expr) = path.lhs.to_ident() {
                container_expr.name
            } else {
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa
                    .diag
                    .lock()
                    .report(self.file_id, path.lhs.pos(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            };

            let method_name = if let Some(ident) = path.rhs.to_ident() {
                ident.name
            } else {
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa
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
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else {
            self.sa
                .diag
                .lock()
                .report(self.file_id, e.pos, ErrorMessage::NoTypeParamsExpected);
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    }

    fn check_enum_value_without_args_id(
        &mut self,
        expr_id: ast::NodeId,
        expr_pos: Position,
        expected_ty: SourceType,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant_idx: usize,
    ) -> SourceType {
        let enum_ = self.sa.enums[enum_id].read();

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.diag.lock().report(self.file_id, expr_pos, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.sa,
            self.fct,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, expr_pos),
        );

        let variant = &enum_.variants[variant_idx];

        if !variant.types.is_empty() {
            let enum_name = self.sa.interner.str(enum_.name).to_string();
            let variant_name = self.sa.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| a.name_fct(self.sa, self.fct))
                .collect::<Vec<_>>();
            let arg_types = Vec::new();
            let msg = ErrorMessage::EnumArgsIncompatible(
                enum_name,
                variant_name,
                variant_types,
                arg_types,
            );
            self.sa.diag.lock().report(self.file_id, expr_pos, msg);
        }

        self.analysis.map_idents.insert(
            expr_id,
            IdentType::EnumValue(enum_id, type_params.clone(), variant_idx),
        );

        if type_params_ok {
            let ty = SourceType::Enum(enum_id, type_params);

            self.analysis.set_ty(expr_id, ty.clone());
            ty
        } else {
            self.analysis.set_ty(expr_id, SourceType::Error);
            SourceType::Error
        }
    }

    fn check_expr_dot(&mut self, e: &ast::ExprDotType, _expected_ty: SourceType) -> SourceType {
        let object_type = self.check_expr(&e.lhs, SourceType::Any);

        if object_type.is_tuple() {
            return self.check_expr_dot_tuple(e, object_type);
        }

        let name = match e.rhs.to_ident() {
            Some(ident) => ident.name,

            None => {
                let msg = ErrorMessage::NameExpected;
                self.sa.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        if let Some(struct_id) = object_type.struct_id() {
            let struct_ = self.sa.structs.idx(struct_id);
            let struct_ = struct_.read();
            if let Some(&field_id) = struct_.field_names.get(&name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &struct_.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params();
                let fty = replace_type_param(self.sa, field.ty.clone(), &struct_type_params, None);

                if !struct_field_accessible_from(self.sa, struct_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.sa, object_type.clone(), name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let cls_id = cls_ty.cls_id().expect("no class");
                let cls = self.sa.classes.idx(cls_id);
                let cls = cls.read();

                let field = &cls.fields[field_id];
                let class_type_params = cls_ty.type_params();
                let fty = replace_type_param(self.sa, field.ty.clone(), &class_type_params, None);

                if !class_field_accessible_from(self.sa, cls_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.diag.lock().report(self.file_id, e.pos, msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        // field not found, report error
        if !object_type.is_error() {
            let field_name = self.sa.interner.str(name).to_string();
            let expr_name = object_type.name_fct(self.sa, self.fct);
            let msg = ErrorMessage::UnknownField(field_name, expr_name);
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
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
                let msg = ErrorMessage::IndexExpected;
                self.sa.diag.lock().report(self.file_id, e.pos, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let subtypes = object_type.tuple_subtypes();

        if index >= subtypes.len() as u64 {
            let msg =
                ErrorMessage::IllegalTupleIndex(index, object_type.name_fct(self.sa, self.fct));
            self.sa.diag.lock().report(self.file_id, e.pos, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let ty = subtypes[usize::try_from(index).unwrap()].clone();
        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_this(&mut self, e: &ast::ExprSelfType, _expected_ty: SourceType) -> SourceType {
        if !self.self_available {
            let msg = ErrorMessage::ThisUnavailable;
            self.sa.diag.lock().report(self.file_id, e.pos, msg);
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        assert!(self.self_available);
        let var_id = NestedVarId(0);
        let ident = self
            .vars
            .check_context_allocated(var_id, &mut self.outer_context_access_in_function);
        self.analysis.map_idents.insert(e.id, ident);

        let var = self.vars.get_var(var_id);
        self.analysis.set_ty(e.id, var.ty.clone());
        var.ty.clone()
    }

    fn check_expr_lambda(
        &mut self,
        node: &Arc<ast::Function>,
        _expected_ty: SourceType,
    ) -> SourceType {
        let ret = if let Some(ref ret_type) = node.return_type {
            self.read_type(ret_type)
        } else {
            SourceType::Unit
        };

        self.contains_lambda = true;

        let mut params = Vec::new();

        for param in &node.params {
            params.push(self.read_type(&param.data_type));
        }

        let ty = SourceType::Lambda(SourceTypeArray::with(params.clone()), Box::new(ret.clone()));
        let parent_fct_id = self.fct.id();

        let mut params_with_ctxt = vec![SourceType::Ptr];
        params_with_ctxt.append(&mut params);

        let mut lambda = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            FctParent::Function(parent_fct_id),
        );
        lambda.param_types = params_with_ctxt;
        lambda.return_type = ret;
        lambda.type_params = self.fct.type_params.clone();
        let lambda_fct_id = self.sa.add_fct(lambda);
        self.analysis.map_lambdas.insert(node.id, lambda_fct_id);

        {
            let lambda = self.sa.fcts.idx(lambda_fct_id);

            let mut analysis = AnalysisData::new();

            {
                let lambda = lambda.read();

                let mut typeck = TypeCheck {
                    sa: self.sa,
                    fct: &*lambda,
                    package_id: self.fct.package_id,
                    module_id: self.fct.module_id,
                    file_id: self.fct.file_id,
                    analysis: &mut analysis,
                    ast: &node,
                    symtable: &mut self.symtable,
                    in_loop: false,
                    self_available: self.self_available.clone(),
                    vars: self.vars,
                    contains_lambda: false,
                    outer_context_access_in_function: false,
                    outer_context_access_from_lambda: false,
                };

                typeck.check();
            }

            if analysis.outer_context_access() {
                self.outer_context_access_from_lambda = true
            }

            lambda.write().analysis = Some(analysis);
        }

        self.analysis.set_ty(node.id, ty.clone());

        ty
    }

    fn check_expr_conv(&mut self, e: &ast::ExprConvType, _expected_ty: SourceType) -> SourceType {
        let object_type = self.check_expr(&e.object, SourceType::Any);
        self.analysis.set_ty(e.object.id(), object_type.clone());

        let check_type = self.read_type(&e.data_type);
        self.analysis.set_ty(e.data_type.id(), check_type.clone());

        if check_type.is_trait() {
            let implements = implements_trait(
                self.sa,
                object_type.clone(),
                &self.fct.type_params,
                check_type.clone(),
            );

            if !implements {
                let object_type = object_type.name_fct(self.sa, self.fct);
                let check_type = check_type.name_fct(self.sa, self.fct);

                self.sa.diag.lock().report(
                    self.file_id,
                    e.pos,
                    ErrorMessage::TypeNotImplementingTrait(object_type, check_type),
                );
            }

            self.analysis.set_ty(e.id, check_type.clone());
            check_type
        } else if !check_type.is_error() {
            let name = check_type.name_fct(self.sa, self.fct);
            self.sa
                .diag
                .lock()
                .report(self.file_id, e.pos, ErrorMessage::TraitExpected(name));
            let ty = SourceType::Error;
            self.analysis.set_ty(e.id, ty.clone());
            ty
        } else {
            SourceType::Error
        }
    }

    fn check_expr_lit_int(
        &mut self,
        e: &ast::ExprLitIntType,
        negate: bool,
        expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_int(self.sa, self.file_id, e, negate, expected_ty);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_float(
        &mut self,
        e: &ast::ExprLitFloatType,
        negate: bool,
        _expected_ty: SourceType,
    ) -> SourceType {
        let (ty, _) = check_lit_float(self.sa, self.file_id, e, negate);

        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_lit_str(
        &mut self,
        e: &ast::ExprLitStrType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let str_ty = self.sa.cls(self.sa.known.classes.string());
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
        let stringable_trait = self.sa.known.traits.stringable();
        let stringable_trait_ty = SourceType::new_trait(stringable_trait);

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = self.check_expr(part, SourceType::Any);

                if part_expr.is_error() {
                    continue;
                }

                let implements_stringable = if let SourceType::TypeParam(id) = part_expr {
                    self.fct
                        .type_params
                        .implements_trait(id, stringable_trait_ty.clone())
                } else {
                    implements_trait(
                        self.sa,
                        part_expr.clone(),
                        &self.fct.type_params,
                        stringable_trait_ty.clone(),
                    )
                };

                if implements_stringable {
                    continue;
                }

                let ty = part_expr.name_fct(self.sa, self.fct);
                self.sa.diag.lock().report(
                    self.file_id,
                    part.pos(),
                    ErrorMessage::ExpectedStringable(ty),
                );
            } else {
                assert!(part.is_lit_str());
            }
        }

        let str_ty = self.sa.cls(self.sa.known.classes.string());
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
            ast::Expr::Dot(ref expr) => self.check_expr_dot(expr, expected_ty),
            ast::Expr::This(ref expr) => self.check_expr_this(expr, expected_ty),
            ast::Expr::Conv(ref expr) => self.check_expr_conv(expr, expected_ty),
            ast::Expr::Lambda(ref expr) => self.check_expr_lambda(expr, expected_ty),
            ast::Expr::Block(ref expr) => self.check_expr_block(expr, expected_ty),
            ast::Expr::If(ref expr) => self.check_expr_if(expr, expected_ty),
            ast::Expr::Tuple(ref expr) => self.check_expr_tuple(expr, expected_ty),
            ast::Expr::Paren(ref expr) => self.check_expr_paren(expr, expected_ty),
            ast::Expr::Match(ref expr) => self.check_expr_match(expr, expected_ty),
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
            ast::Stmt::Expr(ref stmt) => {
                self.check_expr(&stmt.expr, SourceType::Any);
            }
        }

        self.analysis.set_ty(s.id(), SourceType::Unit);
    }
}

pub fn args_compatible_fct(
    sa: &SemAnalysis,
    callee: &FctDefinition,
    args: &[SourceType],
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> bool {
    let arg_types = callee.params_without_self();
    let variadic_arguments = callee.is_variadic;
    args_compatible(
        sa,
        arg_types,
        variadic_arguments,
        args,
        type_params,
        self_ty,
    )
}

pub fn arg_names_valid(param_names: &Vec<Name>, arg_names: &Vec<&Option<Name>>) -> bool {
    for (idx, arg_name) in arg_names.iter().enumerate() {
        if arg_name.is_some() && !arg_name.contains(&param_names[idx]) {
            return false;
        }
    }

    true
}

fn args_compatible(
    sa: &SemAnalysis,
    fct_arg_types: &[SourceType],
    variadic_arguments: bool,
    args: &[SourceType],
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> bool {
    let right_number_of_arguments = if variadic_arguments {
        fct_arg_types.len() - 1 <= args.len()
    } else {
        fct_arg_types.len() == args.len()
    };

    if !right_number_of_arguments {
        return false;
    }

    let (def, rest_ty): (&[SourceType], Option<SourceType>) = if variadic_arguments {
        (
            &fct_arg_types[0..fct_arg_types.len() - 1],
            fct_arg_types.last().cloned(),
        )
    } else {
        (&fct_arg_types, None)
    };

    for (ind, def_arg) in def.iter().enumerate() {
        let def_arg = replace_type_param(sa, def_arg.clone(), &type_params, self_ty.clone());

        if !arg_allows(sa, def_arg, args[ind].clone(), self_ty.clone()) {
            return false;
        }
    }

    if let Some(rest_ty) = rest_ty {
        let ind = def.len();
        let rest_ty = replace_type_param(sa, rest_ty, &type_params, self_ty.clone());

        for expr_ty in &args[ind..] {
            if !arg_allows(sa, rest_ty.clone(), expr_ty.clone(), self_ty.clone()) {
                return false;
            }
        }
    }

    true
}

fn arg_allows(
    sa: &SemAnalysis,
    def: SourceType,
    arg: SourceType,
    self_ty: Option<SourceType>,
) -> bool {
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
            arg_allows(sa, real, arg, self_ty)
        }

        SourceType::TypeParam(_) => def == arg,

        SourceType::Class(cls_id, ref params) => {
            if def == arg {
                return true;
            }

            let other_cls_id;
            let other_params;

            match arg {
                SourceType::Class(cls_id, ref params) => {
                    other_cls_id = cls_id;
                    other_params = params.clone();
                }

                _ => {
                    return false;
                }
            };

            if cls_id != other_cls_id || params.len() != other_params.len() {
                return false;
            }

            for (tp, op) in params.iter().zip(other_params.iter()) {
                if !arg_allows(sa, tp, op, self_ty.clone()) {
                    return false;
                }
            }

            true
        }

        SourceType::Tuple(subtypes) => match arg {
            SourceType::Tuple(other_subtypes) => {
                if subtypes.len() != other_subtypes.len() {
                    return false;
                }

                let len = subtypes.len();

                for idx in 0..len {
                    let ty = subtypes[idx].clone();
                    let other_ty = other_subtypes[idx].clone();

                    if !arg_allows(sa, ty, other_ty, self_ty.clone()) {
                        return false;
                    }
                }

                true
            }

            _ => false,
        },

        SourceType::Lambda(_, _) => {
            // for now expect the exact same params and return types
            // possible improvement: allow super classes for params,
            //                             sub class for return type
            def == arg
        }
    }
}

pub fn check_lit_int(
    sa: &SemAnalysis,
    file: SourceFileId,
    e: &ast::ExprLitIntType,
    negate: bool,
    expected_type: SourceType,
) -> (SourceType, i64) {
    let ty = determine_type_literal_int(e, expected_type);

    let ty_name = ty.name(sa);
    let value = e.value;

    if e.base == IntBase::Dec {
        let max = match ty {
            SourceType::UInt8 => 256,
            SourceType::Int32 => (1u64 << 31),
            SourceType::Int64 => (1u64 << 63),
            _ => unreachable!(),
        };

        if (negate && value > max) || (!negate && value >= max) {
            sa.diag
                .lock()
                .report(file, e.pos, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        let value = if negate {
            (value as i64).wrapping_neg()
        } else {
            value as i64
        };

        (ty, value)
    } else {
        assert!(!negate);

        let max = match ty {
            SourceType::UInt8 => 256 as u64,
            SourceType::Int32 => u32::max_value() as u64,
            SourceType::Int64 => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            sa.diag
                .lock()
                .report(file, e.pos, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        (ty, value as i64)
    }
}

fn determine_suffix_type_int_literal(e: &ast::ExprLitIntType) -> Option<SourceType> {
    match e.suffix {
        IntSuffix::UInt8 => Some(SourceType::UInt8),
        IntSuffix::Int32 => Some(SourceType::Int32),
        IntSuffix::Int64 => Some(SourceType::Int64),
        IntSuffix::None => None,
    }
}

pub fn determine_type_literal_int(
    e: &ast::ExprLitIntType,
    expected_type: SourceType,
) -> SourceType {
    let suffix_type = determine_suffix_type_int_literal(e);

    let default_type = match expected_type {
        SourceType::UInt8 => SourceType::UInt8,
        SourceType::Int32 => SourceType::Int32,
        SourceType::Int64 => SourceType::Int64,
        _ => SourceType::Int64,
    };

    suffix_type.unwrap_or(default_type)
}

pub fn check_lit_float(
    sa: &SemAnalysis,
    file: SourceFileId,
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

        sa.diag
            .lock()
            .report(file, e.pos, ErrorMessage::NumberOverflow(ty.into()));
    }

    (ty, value)
}

struct MethodDescriptor {
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    return_type: SourceType,
}

fn lookup_method(
    sa: &SemAnalysis,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    is_static: bool,
    name: Name,
    args: &[SourceType],
    fct_type_params: &SourceTypeArray,
) -> Option<MethodDescriptor> {
    let candidates = if object_type.is_enum() {
        find_methods_in_enum(sa, object_type, type_param_defs, name, is_static)
    } else if object_type.is_struct() || object_type.is_primitive() {
        find_methods_in_struct(sa, object_type, type_param_defs, name, is_static)
    } else if object_type.cls_id().is_some() {
        find_methods_in_class(sa, object_type, type_param_defs, name, is_static)
    } else {
        Vec::new()
    };

    if candidates.len() == 1 {
        let method_id = candidates[0].fct_id;
        let method = sa.fcts.idx(method_id);
        let method = method.read();

        let container_type_params = &candidates[0].container_type_params;
        let type_params = container_type_params.connect(fct_type_params);

        if args_compatible_fct(sa, &*method, args, &type_params, None) {
            let cmp_type = replace_type_param(sa, method.return_type.clone(), &type_params, None);

            return Some(MethodDescriptor {
                fct_id: method_id,
                type_params: type_params,
                return_type: cmp_type,
            });
        }
    }

    None
}

fn is_simple_enum(sa: &SemAnalysis, ty: SourceType) -> bool {
    match ty {
        SourceType::Enum(enum_id, _) => {
            let enum_ = sa.enums[enum_id].read();
            enum_.simple_enumeration
        }

        _ => false,
    }
}

struct VarAccessPerFunction {
    level: usize,
    start_idx: usize,
    next_context_id: usize,
}

pub struct VarManager {
    vars: Vec<VarDefinition>,
    functions: Vec<VarAccessPerFunction>,
}

impl VarManager {
    pub fn new() -> VarManager {
        VarManager {
            vars: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn has_local_vars(&self) -> bool {
        self.vars.len() > self.current_function().start_idx
    }

    pub fn has_context_vars(&self) -> bool {
        self.current_function().next_context_id > 0
    }

    fn current_function(&self) -> &VarAccessPerFunction {
        self.functions.last().expect("no function entered")
    }

    fn current_function_mut(&mut self) -> &mut VarAccessPerFunction {
        self.functions.last_mut().expect("no function entered")
    }

    fn function_for_var(&mut self, var_id: NestedVarId) -> &mut VarAccessPerFunction {
        for function in self.functions.iter_mut().rev() {
            if var_id.0 >= function.start_idx {
                return function;
            }
        }

        panic!("function not found")
    }

    fn local_var_id(&self, var_id: NestedVarId) -> VarId {
        assert!(var_id.0 >= self.current_function().start_idx);
        VarId(var_id.0 - self.current_function().start_idx)
    }

    fn check_context_allocated(
        &mut self,
        var_id: NestedVarId,
        outer_context_access: &mut bool,
    ) -> IdentType {
        let in_outer_function = var_id.0 < self.current_function().start_idx;

        if in_outer_function {
            let field_id = self.ensure_context_allocated(var_id);
            let distance = self.current_function().level - self.function_for_var(var_id).level;
            *outer_context_access = true;
            IdentType::Context(distance, field_id)
        } else {
            IdentType::Var(self.local_var_id(var_id))
        }
    }

    fn ensure_context_allocated(&mut self, var_id: NestedVarId) -> ContextIdx {
        match self.vars[var_id.0].location {
            VarLocation::Context(field_id) => return field_id,
            VarLocation::Stack => {}
        }

        // Allocate slot in context class.
        let function = self.function_for_var(var_id);
        let context_idx = ContextIdx(function.next_context_id);
        function.next_context_id += 1;
        self.vars[var_id.0].location = VarLocation::Context(context_idx);

        context_idx
    }

    fn add_var(&mut self, name: Name, ty: SourceType, mutable: bool) -> NestedVarId {
        let id = NestedVarId(self.vars.len());

        let var = VarDefinition {
            id,
            name,
            ty,
            mutable,
            location: VarLocation::Stack,
        };

        self.vars.push(var);

        id
    }

    fn get_var(&self, idx: NestedVarId) -> &VarDefinition {
        &self.vars[idx.0]
    }

    fn enter_function(&mut self) {
        self.functions.push(VarAccessPerFunction {
            level: self.functions.len(),
            start_idx: self.vars.len(),
            next_context_id: 0,
        });
    }

    fn leave_function(&mut self) -> VarAccess {
        let function = self.functions.pop().expect("missing function");

        let vars = self
            .vars
            .drain(function.start_idx..)
            .map(|vd| Var {
                ty: vd.ty.clone(),
                location: vd.location,
            })
            .collect();

        VarAccess::new(vars)
    }
}

#[derive(Clone, Debug)]
pub struct VarDefinition {
    pub id: NestedVarId,
    pub name: Name,
    pub ty: SourceType,
    pub mutable: bool,
    pub location: VarLocation,
}
