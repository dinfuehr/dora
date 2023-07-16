use std::collections::HashSet;
use std::convert::TryFrom;
use std::str::Chars;
use std::sync::Arc;
use std::{f32, f64};

use once_cell::unsync::OnceCell;
use parking_lot::RwLock;

use crate::access::{
    class_accessible_from, class_field_accessible_from, const_accessible_from,
    enum_accessible_from, fct_accessible_from, global_accessible_from, is_default_accessible,
    method_accessible_from, module_accessible_from, struct_accessible_from,
    struct_field_accessible_from,
};
use crate::error::msg::ErrorMessage;
use crate::fctbodyck::lookup::MethodLookup;
use crate::program_parser::ParsedModifierList;
use crate::report_sym_shadow_span;
use crate::sema::{
    create_tuple, find_field_in_class, find_impl, find_methods_in_class, find_methods_in_enum,
    find_methods_in_struct, implements_trait, AnalysisData, CallType, ClassDefinition,
    ClassDefinitionId, ContextIdx, ContextInfo, EnumDefinitionId, EnumVariant, FctDefinition,
    FctDefinitionId, FctParent, Field, FieldId, ForTypeInfo, GlobalDefinition, IdentType,
    ModuleDefinitionId, NestedVarId, OuterContextResolver, PackageDefinitionId, Sema, SourceFileId,
    StructDefinition, StructDefinitionId, TypeParamDefinition, TypeParamId, Var, VarAccess, VarId,
    VarLocation, Visibility,
};
use crate::specialize::replace_type_param;
use crate::sym::{ModuleSymTable, Sym};
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeparamck::{self, ErrorReporting};
use crate::{always_returns, expr_always_returns, read_type, AllowSelf};

use crate::interner::Name;
use dora_bytecode::Intrinsic;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, MatchCaseType, MatchPattern};
use dora_parser::Span;
use fixedbitset::FixedBitSet;

pub struct TypeCheck<'a> {
    pub sa: &'a mut Sema,
    pub type_param_defs: &'a TypeParamDefinition,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub analysis: &'a mut AnalysisData,
    pub symtable: &'a mut ModuleSymTable,
    pub param_types: Vec<SourceType>,
    pub return_type: Option<SourceType>,
    pub in_loop: bool,
    pub is_lambda: bool,
    pub has_hidden_self_argument: bool,
    pub is_self_available: bool,
    pub vars: &'a mut VarManager,
    pub contains_lambda: bool,
    pub outer_context_classes: &'a mut Vec<OuterContextResolver>,
    pub outer_context_access_in_function: bool,
    pub outer_context_access_from_lambda: bool,
}

impl<'a> TypeCheck<'a> {
    pub fn check_fct(&mut self, fct: &FctDefinition, ast: &ast::Function) {
        self.check_common(|self_| {
            self_.add_type_params(fct);
            self_.add_params(ast);
            self_.check_body(ast);
        })
    }

    pub fn check_initializer(&mut self, global: &GlobalDefinition, expr: &ast::Expr) {
        // Global initializer never has self.
        self.analysis.set_has_self(false);

        self.check_common(|self_| {
            let expr_ty = self_.check_expr(expr, global.ty.clone());

            if !global.ty.is_error()
                && !expr_ty.is_error()
                && !global.ty.allows(self_.sa, expr_ty.clone())
            {
                let name = self_.sa.interner.str(global.name).to_string();
                let global_ty = self_.ty_name(&global.ty);
                let expr_ty = self_.ty_name(&expr_ty);
                let msg = ErrorMessage::AssignType(name, global_ty, expr_ty);
                self_.sa.report(self_.file_id, global.span, msg);
            }
        })
    }

    fn check_common<F>(&mut self, fct: F)
    where
        F: FnOnce(&mut TypeCheck<'a>),
    {
        self.outer_context_classes.push(Arc::new(RwLock::new(None)));
        let start_level = self.symtable.levels();
        self.symtable.push_level();
        self.vars.enter_function();

        fct(self);
        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.prepare_local_and_context_vars();
        self.outer_context_classes
            .pop()
            .expect("missing context class");
    }

    fn check_body(&mut self, ast: &ast::Function) {
        let block = ast.block.as_ref().expect("missing block");
        let fct_return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        if let ast::ExprData::Block(ref block) = block.as_ref() {
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

                self.check_expr(value, fct_return_type.clone())
            } else {
                SourceType::Unit
            };

            if !returns {
                self.check_fct_return_type(fct_return_type, block.span, return_type);
            }
        } else {
            unreachable!()
        }
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

        let needs_outer_context_slot = self.is_lambda
            && (self.outer_context_access_in_function || self.outer_context_access_from_lambda);

        if needs_outer_context_slot {
            let name = self.sa.interner.intern("outer_context");

            fields.push(Field {
                id: FieldId(0),
                name,
                ty: OnceCell::with_value(SourceType::Ptr),
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
                ty: OnceCell::with_value(var.ty.clone()),
                mutable: true,
                visibility: Visibility::Module,
            });
        }

        let name = self.sa.interner.intern("$Context");

        let class = ClassDefinition::new_without_source(
            self.package_id,
            self.module_id,
            Some(self.file_id),
            None,
            name,
            Visibility::Public,
            fields,
        );
        class
            .type_params
            .set(self.type_param_defs.clone())
            .expect("already initialized");
        let class_id = self.sa.classes.alloc(class);
        self.sa.classes[class_id].id = Some(class_id);
        self.analysis.context_cls_id = Some(class_id);
        self.analysis.context_has_outer_context_slot = Some(needs_outer_context_slot);
        let mut context_info = self
            .outer_context_classes
            .last()
            .expect("missing entry")
            .write();
        *context_info = Some(ContextInfo {
            has_outer_context_slot: needs_outer_context_slot,
            context_cls_id: class_id,
        });
    }

    fn add_type_params(&mut self, fct: &FctDefinition) {
        for (id, name) in fct.type_params.names() {
            self.symtable.insert(name, Sym::TypeParam(id));
        }
    }

    fn add_params(&mut self, ast: &ast::Function) {
        self.add_hidden_parameter_self();

        let self_count = if self.has_hidden_self_argument { 1 } else { 0 };
        assert_eq!(ast.params.len() + self_count, self.param_types.len());

        for (ind, (param, ty)) in ast
            .params
            .iter()
            .zip(self.param_types.iter().skip(self_count))
            .enumerate()
        {
            // is this last argument of function with variadic arguments?
            let ty = if ind == ast.params.len() - 1
                && ast.params.last().expect("missing param").variadic
            {
                // type of variable is Array[T]
                self.sa.known.array_ty(ty.clone())
            } else {
                ty.clone()
            };

            let name = self
                .sa
                .interner
                .intern(&param.name.as_ref().expect("missing name").name_as_string);

            let var_id = self.vars.add_var(name, ty, param.mutable);
            self.analysis
                .map_vars
                .insert(param.id, self.vars.local_var_id(var_id));

            // params are only allowed to replace functions, vars cannot be replaced
            let replaced_sym = self.symtable.insert(name, Sym::Var(var_id));
            if let Some(replaced_sym) = replaced_sym {
                report_sym_shadow_span(self.sa, name, self.file_id, param.span, replaced_sym)
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        self.analysis.set_has_self(self.has_hidden_self_argument);

        if !self.has_hidden_self_argument {
            return;
        }

        // Only functions can use `self`.
        let self_ty = self.param_types[0].clone();
        let name = self.sa.interner.intern("self");

        assert!(!self.vars.has_local_vars());
        self.vars.add_var(name, self_ty, false);
    }

    fn add_local(&mut self, id: NestedVarId, span: Span) {
        let name = self.vars.get_var(id).name;
        match self.symtable.insert(name, Sym::Var(id)) {
            Some(Sym::Var(_)) | None => {}
            Some(sym) => report_sym_shadow_span(self.sa, name, self.file_id, span, sym),
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
            let tyname = s.pattern.to_name().unwrap();
            self.sa
                .report(self.file_id, s.span, ErrorMessage::VarNeedsTypeInfo(tyname));

            return;
        }

        // update type of variable, necessary when stmt has initializer expression but no type
        self.check_stmt_let_pattern(&s.pattern, defined_type.clone());

        if s.expr.is_some() {
            if !expr_type.is_error()
                && !defined_type.is_error()
                && !defined_type.allows(self.sa, expr_type.clone())
            {
                let name = s.pattern.to_name().unwrap();
                let defined_type = self.ty_name(&defined_type);
                let expr_type = self.ty_name(&expr_type);
                let msg = ErrorMessage::AssignType(name, defined_type, expr_type);
                self.sa.report(self.file_id, s.span, msg);
            }

        // let variable binding needs to be assigned
        } else {
            self.sa
                .report(self.file_id, s.span, ErrorMessage::LetMissingInitialization);
        }
    }

    fn read_type(&mut self, t: &ast::TypeData) -> SourceType {
        read_type(
            self.sa,
            &self.symtable,
            self.file_id,
            t,
            self.type_param_defs,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error)
    }

    fn check_stmt_let_pattern(&mut self, pattern: &ast::LetPattern, ty: SourceType) {
        match pattern {
            ast::LetPattern::Ident(ref ident) => {
                let name = self
                    .sa
                    .interner
                    .intern(&ident.name.as_ref().expect("missing name").name_as_string);
                let var_id = self.vars.add_var(name, ty, ident.mutable);

                self.add_local(var_id, ident.span);
                self.analysis
                    .map_vars
                    .insert(ident.id, self.vars.local_var_id(var_id));
            }

            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }

            ast::LetPattern::Tuple(ref tuple) => {
                if !ty.is_tuple_or_unit() && !ty.is_error() {
                    let ty_name = self.ty_name(&ty);
                    self.sa.report(
                        self.file_id,
                        tuple.span,
                        ErrorMessage::LetPatternExpectedTuple(ty_name),
                    );
                    return;
                }

                if ty.is_unit() {
                    // () doesn't have any subparts
                    if tuple.parts.len() != 0 {
                        self.sa.report(
                            self.file_id,
                            tuple.span,
                            ErrorMessage::LetPatternShouldBeUnit,
                        );
                    }
                    return;
                }

                if ty.is_error() {
                    for part in &tuple.parts {
                        self.check_stmt_let_pattern(part, SourceType::Error);
                    }
                    return;
                }

                let subtypes = ty.tuple_subtypes();

                if subtypes.len() != tuple.parts.len() {
                    let ty_name = self.ty_name(&ty);
                    self.sa.report(
                        self.file_id,
                        tuple.span,
                        ErrorMessage::LetPatternExpectedTupleWithLength(
                            ty_name,
                            subtypes.len(),
                            tuple.parts.len(),
                        ),
                    );
                    return;
                }

                for (part, subtype) in tuple.parts.iter().zip(subtypes.iter()) {
                    self.check_stmt_let_pattern(&*part, subtype.clone());
                }
            }
        }
    }

    fn check_expr_for(&mut self, stmt: &ast::ExprForType, _expected_ty: SourceType) -> SourceType {
        let object_type = self.check_expr(&stmt.expr, SourceType::Any);

        if object_type.is_error() {
            self.symtable.push_level();
            self.check_stmt_let_pattern(&stmt.pattern, SourceType::Error);
            self.check_expr(&stmt.block, SourceType::Any);
            self.symtable.pop_level();
            return SourceType::Unit;
        }

        if let Some((for_type_info, ret_type)) =
            self.type_supports_iterator_protocol(object_type.clone())
        {
            self.symtable.push_level();
            // set variable type to return type of next
            self.check_stmt_let_pattern(&stmt.pattern, ret_type);
            // store fct ids for code generation
            self.analysis.map_fors.insert(stmt.id, for_type_info);
            self.check_loop_body(&stmt.block);
            self.symtable.pop_level();
            return SourceType::Unit;
        }

        if let Some((make_iterator, iterator_type)) =
            self.type_supports_make_iterator(object_type.clone())
        {
            if let Some((mut for_type_info, ret_type)) =
                self.type_supports_iterator_protocol(iterator_type.clone())
            {
                self.symtable.push_level();

                // set variable type to return type of next
                self.check_stmt_let_pattern(&stmt.pattern, ret_type);

                // store fct ids for code generation
                for_type_info.make_iterator = Some(make_iterator);
                self.analysis.map_fors.insert(stmt.id, for_type_info);

                self.check_loop_body(&stmt.block);
                self.symtable.pop_level();
                return SourceType::Unit;
            }
        }

        let name = self.ty_name(&object_type);
        let msg = ErrorMessage::TypeNotUsableInForIn(name);
        self.sa.report(self.file_id, stmt.expr.span(), msg);

        // set invalid error type
        self.symtable.push_level();
        self.check_stmt_let_pattern(&stmt.pattern, SourceType::Error);
        self.check_loop_body(&stmt.block);
        self.symtable.pop_level();
        SourceType::Unit
    }

    fn check_loop_body(&mut self, expr: &ast::ExprData) {
        let old_in_loop = self.in_loop;
        self.in_loop = true;
        self.check_expr(expr, SourceType::Any);
        self.in_loop = old_in_loop;
    }

    fn type_supports_make_iterator(
        &mut self,
        object_type: SourceType,
    ) -> Option<(FctDefinitionId, SourceType)> {
        let make_iterator_name = self.sa.interner.intern("makeIterator");

        let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .no_error_reporting()
            .method(object_type)
            .name(make_iterator_name)
            .args(&[])
            .find();

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

        let lookup_next = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .no_error_reporting()
            .method(object_type.clone())
            .name(next_name)
            .args(&[])
            .find();

        if !lookup_next.find() {
            return None;
        }

        let next_result_type = lookup_next.found_ret().unwrap();

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
                next: lookup_next.found_fct_id().expect("fct_id missing"),
                iterator_type: object_type,
                next_type: next_result_type,
                value_type: value_type.clone(),
            },
            value_type,
        ))
    }

    fn check_expr_while(
        &mut self,
        stmt: &ast::ExprWhileType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let expr_type = self.check_expr(&stmt.cond, SourceType::Bool);

        if !expr_type.is_error() && !expr_type.is_bool() {
            let expr_type = self.ty_name(&expr_type);
            let msg = ErrorMessage::WhileCondType(expr_type);
            self.sa.report(self.file_id, stmt.span, msg);
        }

        self.check_loop_body(&stmt.block);
        SourceType::Unit
    }

    fn check_expr_return(
        &mut self,
        expr: &ast::ExprReturnType,
        _expected_ty: SourceType,
    ) -> SourceType {
        if let Some(ref return_type) = self.return_type {
            let expected_ty = return_type.clone();

            let expr_type = expr
                .expr
                .as_ref()
                .map(|expr| self.check_expr(&expr, expected_ty.clone()))
                .unwrap_or(SourceType::Unit);

            self.check_fct_return_type(expected_ty, expr.span, expr_type);
        } else {
            self.sa
                .report(self.file_id, expr.span, ErrorMessage::InvalidReturn);

            if let Some(ref expr) = expr.expr {
                self.check_expr(expr.as_ref(), SourceType::Any);
            }
        }

        SourceType::Unit
    }

    fn check_fct_return_type(
        &mut self,
        fct_return_type: SourceType,
        span: Span,
        expr_type: SourceType,
    ) {
        if !expr_type.is_error() && !fct_return_type.allows(self.sa, expr_type.clone()) {
            let fct_type = self.ty_name(&fct_return_type);
            let expr_type = self.ty_name(&expr_type);

            let msg = ErrorMessage::ReturnType(fct_type, expr_type);

            self.sa.report(self.file_id, span, msg);
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
                .report(self.file_id, node.span, ErrorMessage::EnumExpected);
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
            self.check_expr_match_pattern(
                expr_enum_id,
                expr_type_params.clone(),
                case,
                pattern,
                &mut used_variants,
            );

            let case_ty = self.check_expr(&case.value, expected_ty.clone());

            if result_type.is_error() {
                result_type = case_ty;
            } else if case_ty.is_error() {
                // ignore this case
            } else if !result_type.allows(self.sa, case_ty.clone()) {
                let result_type_name = self.ty_name(&result_type);
                let case_ty_name = self.ty_name(&case_ty);
                let msg =
                    ErrorMessage::MatchBranchTypesIncompatible(result_type_name, case_ty_name);
                self.sa.report(self.file_id, case.value.span(), msg);
            }

            self.symtable.pop_level();
        }

        used_variants.toggle_range(..);

        if used_variants.count_ones(..) != 0 {
            let msg = ErrorMessage::MatchUncoveredVariant;
            self.sa.report(self.file_id, node.span, msg);
        }

        self.analysis.set_ty(node.id, result_type.clone());

        result_type
    }

    fn check_expr_match_pattern(
        &mut self,
        expr_enum_id: Option<EnumDefinitionId>,
        expr_type_params: SourceTypeArray,
        case: &MatchCaseType,
        pattern: &MatchPattern,
        used_variants: &mut FixedBitSet,
    ) {
        match pattern.data {
            ast::MatchPatternData::Underscore => {
                let mut negated_used_variants = used_variants.clone();
                negated_used_variants.toggle_range(..);

                if negated_used_variants.count_ones(..) == 0 {
                    let msg = ErrorMessage::MatchUnreachablePattern;
                    self.sa.report(self.file_id, case.span, msg);
                }

                used_variants.insert_range(..);
            }

            ast::MatchPatternData::Ident(ref ident) => {
                let sym = self.read_path(&ident.path);

                let mut used_idents: HashSet<Name> = HashSet::new();

                match sym {
                    Ok(Sym::EnumVariant(enum_id, variant_idx)) => {
                        if Some(enum_id) == expr_enum_id {
                            if used_variants.contains(variant_idx as usize) {
                                let msg = ErrorMessage::MatchUnreachablePattern;
                                self.sa.report(self.file_id, case.span, msg);
                            }

                            used_variants.insert(variant_idx as usize);
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
                            let variant = &enum_.variants[variant_idx as usize];

                            let given_params = if let Some(ref params) = ident.params {
                                params.len()
                            } else {
                                0
                            };

                            if given_params == 0 && ident.params.is_some() {
                                let msg = ErrorMessage::MatchPatternNoParens;
                                self.sa.report(self.file_id, case.span, msg);
                            }

                            let expected_params = variant.types.len();

                            if given_params != expected_params {
                                let msg = ErrorMessage::MatchPatternWrongNumberOfParams(
                                    given_params,
                                    expected_params,
                                );
                                self.sa.report(self.file_id, case.span, msg);
                            }

                            if let Some(ref params) = ident.params {
                                for (idx, param) in params.iter().enumerate() {
                                    if let Some(ident) = &param.name {
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

                                        let iname = self.sa.interner.intern(&ident.name_as_string);

                                        if used_idents.insert(iname) == false {
                                            let msg = ErrorMessage::VarAlreadyInPattern;
                                            self.sa.report(self.file_id, param.span, msg);
                                        }

                                        let var_id = self.vars.add_var(iname, ty, param.mutable);
                                        self.add_local(var_id, param.span);
                                        self.analysis
                                            .map_vars
                                            .insert(param.id, self.vars.local_var_id(var_id));
                                    }
                                }
                            }
                        } else {
                            let msg = ErrorMessage::EnumVariantExpected;
                            self.sa.report(self.file_id, ident.path.span, msg);
                        }
                    }

                    Ok(_) => {
                        let msg = ErrorMessage::EnumVariantExpected;
                        self.sa.report(self.file_id, ident.path.span, msg);
                    }

                    Err(()) => {}
                }
            }
        }
    }

    fn check_expr_if(&mut self, expr: &ast::ExprIfType, expected_ty: SourceType) -> SourceType {
        let expr_type = self.check_expr(&expr.cond, SourceType::Any);

        if !expr_type.is_bool() && !expr_type.is_error() {
            let expr_type = self.ty_name(&expr_type);
            let msg = ErrorMessage::IfCondType(expr_type);
            self.sa.report(self.file_id, expr.span, msg);
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
                let then_type_name = self.ty_name(&then_type);
                let else_type_name = self.ty_name(&else_type);
                let msg = ErrorMessage::IfBranchTypesIncompatible(then_type_name, else_type_name);
                self.sa.report(self.file_id, expr.span, msg);
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
        let interned_name = self.sa.interner.intern(&e.name);
        let sym = self.symtable.get(interned_name);

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
                let const_ = &self.sa.consts[const_id];

                self.analysis.set_ty(e.id, const_.ty());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                const_.ty()
            }

            Some(Sym::EnumVariant(enum_id, variant_idx)) => self.check_enum_value_without_args_id(
                e.id,
                e.span,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_idx,
            ),

            None => {
                self.sa.report(
                    self.file_id,
                    e.span,
                    ErrorMessage::UnknownIdentifier(e.name.clone()),
                );
                SourceType::Error
            }

            _ => {
                self.sa
                    .report(self.file_id, e.span, ErrorMessage::ValueExpected);
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
                .report(self.file_id, e.span, ErrorMessage::LvalueExpected);
        }

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    fn check_expr_assign_ident(&mut self, e: &ast::ExprBinType) {
        self.analysis.set_ty(e.id, SourceType::Unit);

        let lhs_ident = e.lhs.to_ident().unwrap();
        let sym = self.symtable.get_string(self.sa, &lhs_ident.name);

        let lhs_type = match sym {
            Some(Sym::Var(var_id)) => {
                if !self.vars.get_var(var_id).mutable {
                    self.sa
                        .report(self.file_id, e.span, ErrorMessage::LetReassigned);
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
                        .report(self.file_id, e.span, ErrorMessage::LetReassigned);
                }

                self.analysis
                    .map_idents
                    .insert(e.lhs.id(), IdentType::Global(global_id));
                global_var.ty.clone()
            }

            None => {
                self.sa.report(
                    self.file_id,
                    lhs_ident.span,
                    ErrorMessage::UnknownIdentifier(lhs_ident.name.clone()),
                );

                return;
            }

            _ => {
                self.sa
                    .report(self.file_id, lhs_ident.span, ErrorMessage::LvalueExpected);

                return;
            }
        };

        let rhs_type = self.check_expr(&e.rhs, lhs_type.clone());

        if !lhs_type.is_error()
            && !rhs_type.is_error()
            && !lhs_type.allows(self.sa, rhs_type.clone())
        {
            let ident = e.lhs.to_ident().unwrap();
            let lhs_type = self.ty_name(&lhs_type);
            let rhs_type = self.ty_name(&rhs_type);

            self.analysis.set_ty(e.id, SourceType::Unit);

            let msg = ErrorMessage::AssignType(ident.name.clone(), lhs_type, rhs_type);
            self.sa.report(self.file_id, e.span, msg);
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

        let name = self.sa.interner.intern("set");
        arg_types.push(value_type);

        if let Some(descriptor) = self.find_method(
            e.span,
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
            Some(ident) => ident.name.clone(),

            None => {
                let msg = ErrorMessage::NameExpected;
                self.sa.report(self.file_id, e.span, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return;
            }
        };

        let interned_name = self.sa.interner.intern(&name);

        let object_type = self.check_expr(&field_expr.lhs, SourceType::Any);

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.sa, object_type.clone(), interned_name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis
                    .map_idents
                    .insert_or_replace(e.lhs.id(), ident_type);

                let cls = &self.sa.classes[cls_ty.cls_id().expect("no class")];
                let field = &cls.fields[field_id];

                let class_type_params = cls_ty.type_params();

                let fty = replace_type_param(self.sa, field.ty(), &class_type_params, None);

                if !e.initializer && !field.mutable {
                    self.sa
                        .report(self.file_id, e.span, ErrorMessage::LetReassigned);
                }

                let rhs_type = self.check_expr(&e.rhs, fty.clone());

                if !fty.allows(self.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let object_type = self.ty_name(&object_type);
                    let lhs_type = self.ty_name(&fty);
                    let rhs_type = self.ty_name(&rhs_type);

                    let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                    self.sa.report(self.file_id, e.span, msg);
                }

                self.analysis.set_ty(e.id, SourceType::Unit);
                return;
            }
        }

        if object_type.is_struct() {
            self.sa
                .report(self.file_id, e.span, ErrorMessage::StructFieldImmutable);

            // We want to see syntax expressions in the assignment expressions even when we can't
            // find the given field.
            self.check_expr(&e.rhs, SourceType::Any);

            self.analysis.set_ty(e.id, SourceType::Unit);
            return;
        }

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        self.check_expr(&e.rhs, SourceType::Any);

        // field not found, report error
        let expr_name = self.ty_name(&object_type);
        let msg = ErrorMessage::UnknownField(name, expr_name);
        self.sa.report(self.file_id, field_expr.op_span, msg);

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    fn find_method(
        &mut self,
        span: Span,
        object_type: SourceType,
        is_static: bool,
        name: Name,
        args: &[SourceType],
        fct_type_params: &SourceTypeArray,
    ) -> Option<MethodDescriptor> {
        let descriptor = lookup_method(
            self.sa,
            object_type.clone(),
            self.type_param_defs,
            is_static,
            name,
            args,
            fct_type_params,
        );

        if descriptor.is_none() {
            let type_name = self.ty_name(&object_type);
            let name = self.sa.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<String>>();
            let msg = if is_static {
                ErrorMessage::UnknownStaticMethod(type_name, name, param_names)
            } else {
                ErrorMessage::UnknownMethod(type_name, name, param_names)
            };

            self.sa.report(self.file_id, span, msg);
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
        let name = self.sa.interner.intern(name);
        let call_types = [];

        if !ty.is_error() {
            if let Some(descriptor) = lookup_method(
                self.sa,
                ty.clone(),
                self.type_param_defs,
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

            let ty = self.ty_name(&ty);
            let msg = ErrorMessage::UnOpType(op.as_str().into(), ty);

            self.sa.report(self.file_id, e.span, msg);
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
            ast::BinOp::Mod => self.check_expr_bin_method(e, e.op, "modulo", lhs_type, rhs_type),
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
        let name = self.sa.interner.intern(name);
        let call_types = [rhs_type.clone()];

        if let Some(descriptor) = lookup_method(
            self.sa,
            lhs_type.clone(),
            self.type_param_defs,
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
            let lhs_type = self.ty_name(&lhs_type);
            let rhs_type = self.ty_name(&rhs_type);
            let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            self.sa.report(self.file_id, e.span, msg);

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
                    let lhs_type = self.ty_name(&lhs_type);
                    let rhs_type = self.ty_name(&rhs_type);
                    self.sa.report(
                        self.file_id,
                        e.span,
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
            let lhs_type = self.ty_name(&lhs_type);
            let rhs_type = self.ty_name(&rhs_type);
            let msg = ErrorMessage::BinOpType("equals".into(), lhs_type, rhs_type);

            self.sa.report(self.file_id, e.span, msg);

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
            let lhs_type = self.ty_name(&lhs_type);
            let rhs_type = self.ty_name(&rhs_type);
            let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

            self.sa.report(self.file_id, e.span, msg);
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
            let sym = self.symtable.get_string(self.sa, &expr_ident.name);

            self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, &arg_types)
        } else if let Some(expr_dot) = callee.to_dot() {
            let object_type = self.check_expr(&expr_dot.lhs, SourceType::Any);

            let method_name = match expr_dot.rhs.to_ident() {
                Some(ident) => ident.name.clone(),

                None => {
                    let msg = ErrorMessage::NameExpected;
                    self.sa.report(self.file_id, e.span, msg);

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
                self.sa.report(self.file_id, e.callee.span(), msg);
            }

            let expr_type = self.check_expr(callee, SourceType::Any);
            self.check_expr_call_expr(e, expr_type, &arg_types)
        }
    }

    fn check_expr_call_sym(
        &mut self,
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        callee: &ast::ExprData,
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
                    self.sa.report(self.file_id, e.callee.span(), msg);
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
        variant_idx: u32,
        arg_types: &[SourceType],
    ) -> SourceType {
        let enum_ = self.sa.enums.idx(enum_id);
        let enum_ = enum_.read();
        let variant = &enum_.variants[variant_idx as usize];

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.report(self.file_id, e.span, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.sa,
            self.type_param_defs,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.span),
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
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::EnumArgsIncompatible(
                enum_name,
                variant_name,
                variant_types,
                arg_types,
            );
            self.sa.report(self.file_id, e.span, msg);
        } else if variant.types.is_empty() {
            let enum_name = self.sa.interner.str(enum_.name).to_string();
            let variant_name = self.sa.interner.str(variant.name).to_string();
            let msg = ErrorMessage::EnumArgsNoParens(enum_name, variant_name);
            self.sa.report(self.file_id, e.span, msg);
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
        name: String,
        arg_types: &[SourceType],
    ) -> SourceType {
        let mut fcts = Vec::new();
        let interned_name = self.sa.interner.intern(&name);

        for trait_ty in self.type_param_defs.bounds_for_type_param(tp_id) {
            let trait_id = trait_ty.trait_id().expect("trait expected");
            let trait_ = self.sa.traits[trait_id].read();

            if let Some(fct_id) = trait_.find_method(self.sa, interned_name, true) {
                fcts.push((trait_id, fct_id));
            }
        }

        if fcts.len() != 1 {
            let msg = if fcts.len() > 1 {
                ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam
            } else {
                ErrorMessage::UnknownStaticMethodWithTypeParam
            };

            self.sa.report(self.file_id, e.span, msg);

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
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::ParamTypesIncompatible(name, fct_params, arg_types);
            self.sa.report(self.file_id, e.span, msg);
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
            e.span,
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
        let type_params_count = self.type_param_defs.len();
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
            let fct_params = params.iter().map(|a| self.ty_name(&a)).collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::LambdaParamTypesIncompatible(fct_params, arg_types);
            self.sa.report(self.file_id, e.span, msg);
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
            self.sa.report(self.file_id, e.span, msg);
        }

        let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .span(e.span)
            .callee(fct_id)
            .args(&arg_types)
            .fct_type_params(&type_params)
            .find();

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
        method_name: String,
        fct_type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        let interned_method_name = self.sa.interner.intern(&method_name);
        let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .span(e.span)
            .static_method(object_type)
            .name(interned_method_name)
            .args(arg_types)
            .fct_type_params(&fct_type_params)
            .find();

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
                self.sa.report(self.file_id, e.span, msg);
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
        method_name: String,
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

        let interned_method_name = self.sa.interner.intern(&method_name);

        let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .no_error_reporting()
            .method(object_type.clone())
            .name(interned_method_name)
            .fct_type_params(&fct_type_params)
            .args(arg_types)
            .find();

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
                self.sa.report(self.file_id, e.span, msg);
            }

            return_type
        } else if lookup.found_fct_id().is_none() {
            // No method with this name found, so this might actually be a field
            self.check_expr_call_field(e, object_type, method_name, fct_type_params, arg_types)
        } else {
            // Lookup the method again, but this time with error reporting
            let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
                .method(object_type)
                .name(interned_method_name)
                .fct_type_params(&fct_type_params)
                .span(e.span)
                .args(arg_types)
                .find();

            assert!(!lookup.find());

            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_field(
        &mut self,
        e: &ast::ExprCallType,
        object_type: SourceType,
        method_name: String,
        type_params: SourceTypeArray,
        arg_types: &[SourceType],
    ) -> SourceType {
        let interned_method_name = self.sa.interner.intern(&method_name);
        if let Some((actual_type, field_id, field_type)) =
            find_field_in_class(self.sa, object_type.clone(), interned_method_name)
        {
            self.analysis.set_ty(e.callee.id(), field_type.clone());
            self.analysis
                .map_idents
                .insert_or_replace(e.callee.id(), IdentType::Field(actual_type, field_id));

            let cls_id = object_type.cls_id().expect("class expected");

            if !class_field_accessible_from(self.sa, cls_id, field_id, self.module_id) {
                let cls = &self.sa.classes[cls_id];
                let field = &cls.fields[field_id];

                let name = self.sa.interner.str(field.name).to_string();
                let msg = ErrorMessage::NotAccessible(name);
                self.sa.report(self.file_id, e.span, msg);
            }

            return self.check_expr_call_expr(e, field_type, arg_types);
        }

        if let Some(struct_id) = object_type.struct_id() {
            let struct_ = &self.sa.structs[struct_id];
            if let Some(&field_id) = struct_.field_names.get(&interned_method_name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &struct_.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params();
                let field_type = replace_type_param(self.sa, field.ty(), &struct_type_params, None);

                if !struct_field_accessible_from(self.sa, struct_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.report(self.file_id, e.span, msg);
                }

                self.analysis.set_ty(e.id, field_type.clone());
                return self.check_expr_call_expr(e, field_type, arg_types);
            }
        }

        // No field with that name as well, so report method
        let lookup = MethodLookup::new(self.sa, self.file_id, self.type_param_defs)
            .method(object_type)
            .name(interned_method_name)
            .fct_type_params(&type_params)
            .span(e.span)
            .args(arg_types)
            .find();
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
            let struct_ = &self.sa.structs[struct_id];
            let msg = ErrorMessage::NotAccessible(struct_.name(self.sa));
            self.sa.report(self.file_id, e.span, msg);
        }

        let struct_ = &self.sa.structs[struct_id];

        if !is_default_accessible(self.sa, struct_.module_id, self.module_id)
            && !struct_.all_fields_are_public()
            && is_struct_accessible
        {
            let msg = ErrorMessage::StructConstructorNotAccessible(struct_.name(self.sa));
            self.sa.report(self.file_id, e.span, msg);
        }

        let ty = SourceType::Struct(struct_id, type_params.clone());
        let type_params_ok = typeparamck::check_struct(
            self.sa,
            self.type_param_defs,
            struct_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.span),
        );

        if !type_params_ok {
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        if !check_expr_call_struct_args(self.sa, struct_, type_params.clone(), arg_types) {
            let struct_name = self.sa.interner.str(struct_.name).to_string();
            let field_types = struct_
                .fields
                .iter()
                .map(|field| field.ty().name_struct(self.sa, &*struct_))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::StructArgsIncompatible(struct_name, field_types, arg_types);
            self.sa.report(self.file_id, e.span, msg);
        }

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Struct(struct_id, type_params)));

        self.analysis.set_ty(e.id, ty.clone());
        ty
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
            let cls = &self.sa.classes[cls_id];
            let msg = ErrorMessage::NotAccessible(cls.name(self.sa));
            self.sa.report(self.file_id, e.span, msg);
        }

        let type_params = if expected_ty.is_cls_id(cls_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        if !typeparamck::check_class(
            self.sa,
            self.type_param_defs,
            cls_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, e.span),
        ) {
            return SourceType::Error;
        };

        let cls = &self.sa.classes[cls_id];
        let cls_ty = self.sa.cls_with_type_list(cls_id, type_params.clone());

        if !is_default_accessible(self.sa, cls.module_id, self.module_id)
            && !cls.all_fields_are_public()
            && is_class_accessible
        {
            let msg = ErrorMessage::ClassConstructorNotAccessible(cls.name(self.sa));
            self.sa.report(self.file_id, e.span, msg);
        }

        if !check_expr_call_class_args(self.sa, cls, type_params.clone(), arg_types) {
            let class_name = cls.name(self.sa);
            let field_types = cls
                .fields
                .iter()
                .map(|field| field.ty().name_cls(self.sa, &*cls))
                .collect::<Vec<_>>();
            let arg_types = arg_types
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let msg = ErrorMessage::ParamTypesIncompatible(class_name, field_types, arg_types);
            self.sa.report(self.file_id, e.span, msg);
        }

        self.analysis
            .map_calls
            .insert(e.id, Arc::new(CallType::Class2Ctor(cls.id(), type_params)));

        self.analysis.set_ty(e.id, cls_ty.clone());
        cls_ty
    }

    fn check_expr_call_generic(
        &mut self,
        e: &ast::ExprCallType,
        tp_id: TypeParamId,
        name: String,
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
        name: String,
        args: &[SourceType],
    ) -> SourceType {
        let mut found_fcts = Vec::new();
        let interned_name = self.sa.interner.intern(&name);

        for trait_ty in self.type_param_defs.bounds_for_type_param(id) {
            let trait_id = trait_ty.trait_id().expect("trait expected");
            let trait_ = self.sa.traits[trait_id].read();

            if let Some(fid) =
                trait_.find_method_with_replace(self.sa, false, interned_name, None, args)
            {
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
            let type_name = self.ty_name(&object_type);
            let param_names = args
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<String>>();
            let msg = if found_fcts.len() == 0 {
                ErrorMessage::UnknownMethodForTypeParam(type_name, name, param_names)
            } else {
                ErrorMessage::MultipleCandidatesForTypeParam(type_name, name, param_names)
            };

            self.sa.report(self.file_id, e.span, msg);
            self.analysis.set_ty(e.id, SourceType::Error);

            SourceType::Error
        }
    }

    fn check_expr_call_path(
        &mut self,
        e: &ast::ExprCallType,
        expected_ty: SourceType,
        callee: &ast::ExprData,
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
            method_name_expr.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa.report(self.file_id, method_expr.span(), msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        };

        let interned_method_name = self.sa.interner.intern(&method_name);

        match sym {
            Some(Sym::Class(cls_id)) => {
                if typeparamck::check_class(
                    self.sa,
                    self.type_param_defs,
                    cls_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.span),
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
                let struct_ = &self.sa.structs[struct_id];

                if typeparamck::check_struct(
                    self.sa,
                    self.type_param_defs,
                    struct_id,
                    &container_type_params,
                    ErrorReporting::Yes(self.file_id, e.span),
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

                if let Some(&variant_idx) = enum_.name_to_value.get(&interned_method_name) {
                    if !container_type_params.is_empty() && !type_params.is_empty() {
                        let msg = ErrorMessage::NoTypeParamsExpected;
                        self.sa.report(self.file_id, callee_as_path.lhs.span(), msg);
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
                        variant_idx,
                        &arg_types,
                    )
                } else {
                    if typeparamck::check_enum(
                        self.sa,
                        self.type_param_defs,
                        enum_id,
                        &container_type_params,
                        ErrorReporting::Yes(self.file_id, e.span),
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
                    self.sa.report(self.file_id, callee_as_path.lhs.span(), msg);
                }

                self.check_expr_call_generic_static_method(e, id, method_name, &arg_types)
            }

            Some(Sym::Module(module_id)) => {
                if !container_type_params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa.report(self.file_id, callee_as_path.lhs.span(), msg);
                }

                let sym = {
                    let module = &self.sa.modules[module_id].read();
                    let table = module.table.read();

                    table.get(interned_method_name)
                };

                self.check_expr_call_sym(e, expected_ty, callee, sym, type_params, arg_types)
            }

            _ => {
                let msg = ErrorMessage::ClassExpected;
                self.sa.report(self.file_id, e.span, msg);

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
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa.report(self.file_id, e.rhs.span(), msg);
            return SourceType::Error;
        };

        match sym {
            Some(Sym::Enum(id)) => self.check_enum_value_without_args(
                e.id,
                e.op_span,
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
                self.sa.report(self.file_id, e.lhs.span(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                SourceType::Error
            }
        }
    }

    fn read_path_expr(&mut self, expr: &ast::ExprData) -> Result<Option<Sym>, ()> {
        if let Some(expr_path) = expr.to_path() {
            let sym = self.read_path_expr(&expr_path.lhs)?;

            let element_name = if let Some(ident) = expr_path.rhs.to_ident() {
                ident.name.clone()
            } else {
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa.report(self.file_id, expr_path.rhs.span(), msg);
                return Err(());
            };

            let interned_element_name = self.sa.interner.intern(&element_name);

            match sym {
                Some(Sym::Module(module_id)) => {
                    let module = &self.sa.modules[module_id].read();
                    let symtable = module.table.read();
                    let sym = symtable.get(interned_element_name);

                    Ok(sym)
                }

                _ => {
                    let msg = ErrorMessage::ExpectedModule;
                    self.sa.report(self.file_id, expr.span(), msg);
                    Err(())
                }
            }
        } else if let Some(expr_ident) = expr.to_ident() {
            let sym = self.symtable.get_string(self.sa, &expr_ident.name);

            Ok(sym)
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            self.sa.report(self.file_id, expr.span(), msg);
            Err(())
        }
    }

    fn read_path(&mut self, path: &ast::PathData) -> Result<Sym, ()> {
        let names = &path.names;
        let mut sym = self.symtable.get_string(self.sa, &names[0].name_as_string);

        for ident in &names[1..] {
            match sym {
                Some(Sym::Module(module_id)) => {
                    if !module_accessible_from(self.sa, module_id, self.module_id) {
                        let module = &self.sa.modules[module_id].read();
                        let msg = ErrorMessage::NotAccessible(module.name(self.sa));
                        self.sa.report(self.file_id, path.span, msg);
                    }

                    let iname = self.sa.interner.intern(&ident.name_as_string);

                    let module = &self.sa.modules[module_id].read();
                    let symtable = module.table.read();
                    sym = symtable.get(iname);
                }

                Some(Sym::Enum(enum_id)) => {
                    let enum_ = self.sa.enums[enum_id].read();

                    if !enum_accessible_from(self.sa, enum_id, self.module_id) {
                        let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
                        self.sa.report(self.file_id, path.span, msg);
                    }

                    let iname = self.sa.interner.intern(&ident.name_as_string);

                    if let Some(&variant_idx) = enum_.name_to_value.get(&iname) {
                        sym = Some(Sym::EnumVariant(enum_id, variant_idx));
                    } else {
                        let name = ident.name_as_string.clone();
                        self.sa.report(
                            self.file_id.into(),
                            path.span,
                            ErrorMessage::UnknownEnumVariant(name),
                        );
                        return Err(());
                    }
                }

                Some(_) => {
                    let msg = ErrorMessage::ExpectedModule;
                    self.sa.report(self.file_id, path.span, msg);
                    return Err(());
                }

                None => {
                    let name = names[0].name_as_string.clone();
                    let msg = ErrorMessage::UnknownIdentifier(name);
                    self.sa.report(self.file_id, path.span, msg);
                    return Err(());
                }
            }
        }

        if let Some(sym) = sym {
            Ok(sym)
        } else {
            let name = names[0].name_as_string.clone();
            let msg = ErrorMessage::UnknownIdentifier(name);
            self.sa.report(self.file_id, path.span, msg);

            Err(())
        }
    }

    fn check_expr_path_module(
        &mut self,
        e: &ast::ExprPathType,
        expected_ty: SourceType,
        module_id: ModuleDefinitionId,
        element_name: String,
    ) -> SourceType {
        let module = &self.sa.modules.idx(module_id);
        let module = module.read();
        let table = module.table.read();

        let interned_element_name = self.sa.interner.intern(&element_name);

        let sym = table.get(interned_element_name);

        match sym {
            Some(Sym::Global(global_id)) => {
                if !global_accessible_from(self.sa, global_id, self.module_id) {
                    let global = &self.sa.globals.idx(global_id);
                    let global = global.read();
                    let msg = ErrorMessage::NotAccessible(global.name(self.sa));
                    self.sa.report(self.file_id, e.op_span, msg);
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
                    let const_ = &self.sa.consts[const_id];
                    let msg = ErrorMessage::NotAccessible(const_.name(self.sa));
                    self.sa.report(self.file_id, e.op_span, msg);
                }

                let const_ = &self.sa.consts[const_id];
                self.analysis.set_ty(e.id, const_.ty());

                self.analysis
                    .map_idents
                    .insert(e.id, IdentType::Const(const_id));

                const_.ty()
            }

            Some(Sym::EnumVariant(enum_id, variant_idx)) => self.check_enum_value_without_args_id(
                e.id,
                e.op_span,
                expected_ty,
                enum_id,
                SourceTypeArray::empty(),
                variant_idx,
            ),

            None => {
                let module = module.name(self.sa);
                self.sa.report(
                    self.file_id,
                    e.span,
                    ErrorMessage::UnknownIdentifierInModule(module, element_name),
                );
                SourceType::Error
            }

            _ => {
                self.sa
                    .report(self.file_id, e.span, ErrorMessage::ValueExpected);
                SourceType::Error
            }
        }
    }

    fn check_enum_value_without_args(
        &mut self,
        expr_id: ast::NodeId,
        expr_span: Span,
        _expected_ty: SourceType,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        name: String,
    ) -> SourceType {
        let enum_ = self.sa.enums[enum_id].read();

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.report(self.file_id, expr_span, msg);
        }

        let type_params_ok = typeparamck::check_enum(
            self.sa,
            self.type_param_defs,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, expr_span),
        );

        let interned_name = self.sa.interner.intern(&name);

        if let Some(&value) = enum_.name_to_value.get(&interned_name) {
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
                self.sa.report(self.file_id, expr_span, msg);
            }

            self.analysis.map_idents.insert(
                expr_id,
                IdentType::EnumValue(enum_id, type_params.clone(), value),
            );
        } else {
            self.sa.report(
                self.file_id,
                expr_span,
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
            let sym = self.symtable.get_string(self.sa, &ident.name);

            match sym {
                Some(Sym::EnumVariant(enum_id, variant_idx)) => self
                    .check_enum_value_without_args_id(
                        e.id,
                        e.op_span,
                        expected_ty,
                        enum_id,
                        type_params,
                        variant_idx,
                    ),

                _ => {
                    self.sa
                        .report(self.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else if let Some(path) = e.callee.to_path() {
            let container_name = if let Some(container_expr) = path.lhs.to_ident() {
                container_expr.name.clone()
            } else {
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa.report(self.file_id, path.lhs.span(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            };

            let method_name = if let Some(ident) = path.rhs.to_ident() {
                ident.name.clone()
            } else {
                let msg = ErrorMessage::ExpectedSomeIdentifier;
                self.sa.report(self.file_id, path.rhs.span(), msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            };

            let sym = self.symtable.get_string(self.sa, &container_name);

            match sym {
                Some(Sym::Enum(enum_id)) => self.check_enum_value_without_args(
                    e.id,
                    e.op_span,
                    expected_ty,
                    enum_id,
                    type_params,
                    method_name,
                ),

                _ => {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    self.sa.report(self.file_id, e.op_span, msg);

                    self.analysis.set_ty(e.id, SourceType::Error);
                    SourceType::Error
                }
            }
        } else {
            self.sa
                .report(self.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }
    }

    fn check_enum_value_without_args_id(
        &mut self,
        expr_id: ast::NodeId,
        expr_span: Span,
        expected_ty: SourceType,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant_idx: u32,
    ) -> SourceType {
        let enum_ = self.sa.enums[enum_id].read();

        if !enum_accessible_from(self.sa, enum_id, self.module_id) {
            let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
            self.sa.report(self.file_id, expr_span, msg);
        }

        let type_params = if expected_ty.is_enum_id(enum_id) && type_params.is_empty() {
            expected_ty.type_params()
        } else {
            type_params
        };

        let type_params_ok = typeparamck::check_enum(
            self.sa,
            self.type_param_defs,
            enum_id,
            &type_params,
            ErrorReporting::Yes(self.file_id, expr_span),
        );

        let variant = &enum_.variants[variant_idx as usize];

        if !variant.types.is_empty() {
            let enum_name = self.sa.interner.str(enum_.name).to_string();
            let variant_name = self.sa.interner.str(variant.name).to_string();
            let variant_types = variant
                .types
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<_>>();
            let arg_types = Vec::new();
            let msg = ErrorMessage::EnumArgsIncompatible(
                enum_name,
                variant_name,
                variant_types,
                arg_types,
            );
            self.sa.report(self.file_id, expr_span, msg);
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
            Some(ident) => ident.name.clone(),

            None => {
                let msg = ErrorMessage::NameExpected;
                self.sa.report(self.file_id, e.op_span, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let interned_name = self.sa.interner.intern(&name);

        if let Some(struct_id) = object_type.struct_id() {
            let struct_ = &self.sa.structs[struct_id];
            if let Some(&field_id) = struct_.field_names.get(&interned_name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let field = &struct_.fields[field_id.to_usize()];
                let struct_type_params = object_type.type_params();
                let fty = replace_type_param(self.sa, field.ty(), &struct_type_params, None);

                if !struct_field_accessible_from(self.sa, struct_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.report(self.file_id, e.op_span, msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        if object_type.cls_id().is_some() {
            if let Some((cls_ty, field_id, _)) =
                find_field_in_class(self.sa, object_type.clone(), interned_name)
            {
                let ident_type = IdentType::Field(cls_ty.clone(), field_id);
                self.analysis.map_idents.insert_or_replace(e.id, ident_type);

                let cls_id = cls_ty.cls_id().expect("no class");
                let cls = &self.sa.classes[cls_id];
                let field = &cls.fields[field_id];
                let class_type_params = cls_ty.type_params();
                let fty = replace_type_param(self.sa, field.ty(), &class_type_params, None);

                if !class_field_accessible_from(self.sa, cls_id, field_id, self.module_id) {
                    let name = self.sa.interner.str(field.name).to_string();
                    let msg = ErrorMessage::NotAccessible(name);
                    self.sa.report(self.file_id, e.op_span, msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        // field not found, report error
        if !object_type.is_error() {
            let expr_name = self.ty_name(&object_type);
            let msg = ErrorMessage::UnknownField(name, expr_name);
            self.sa.report(self.file_id, e.op_span, msg);
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
            Some(literal) => {
                let (ty, value_i64, _) =
                    check_lit_int(self.sa, self.file_id, literal, false, SourceType::Any);

                if ty.is_float() {
                    self.sa
                        .report(self.file_id, literal.span, ErrorMessage::IndexExpected);
                }

                self.analysis.set_literal_value(literal.id, value_i64, 0.0);

                value_i64 as u64
            }

            None => {
                let msg = ErrorMessage::IndexExpected;
                self.sa.report(self.file_id, e.op_span, msg);

                self.analysis.set_ty(e.id, SourceType::Error);
                return SourceType::Error;
            }
        };

        let subtypes = object_type.tuple_subtypes();

        if index >= subtypes.len() as u64 {
            let msg = ErrorMessage::IllegalTupleIndex(index, self.ty_name(&object_type));
            self.sa.report(self.file_id, e.op_span, msg);

            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        let ty = subtypes[usize::try_from(index).unwrap()].clone();
        self.analysis.set_ty(e.id, ty.clone());

        ty
    }

    fn check_expr_this(&mut self, e: &ast::ExprSelfType, _expected_ty: SourceType) -> SourceType {
        if !self.is_self_available {
            let msg = ErrorMessage::ThisUnavailable;
            self.sa.report(self.file_id, e.span, msg);
            self.analysis.set_ty(e.id, SourceType::Error);
            return SourceType::Error;
        }

        assert!(self.is_self_available);
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

        let mut params_with_ctxt = vec![SourceType::Ptr];
        params_with_ctxt.append(&mut params);

        let name = self.sa.interner.intern("<closure>");

        let mut lambda = FctDefinition::new(
            self.package_id,
            self.module_id,
            self.file_id,
            node,
            ParsedModifierList::default(),
            name,
            FctParent::Function,
        );
        lambda.param_types = params_with_ctxt;
        lambda.return_type = ret;
        lambda.type_params = self.type_param_defs.clone();
        let lambda_fct_id = self.sa.add_fct(lambda);
        self.analysis.map_lambdas.insert(node.id, lambda_fct_id);

        {
            let lambda = self.sa.fcts.idx(lambda_fct_id);

            let mut analysis = AnalysisData::new();
            analysis.outer_context_infos = self.outer_context_classes.clone();

            {
                let lambda = lambda.read();

                let mut typeck = TypeCheck {
                    sa: self.sa,
                    type_param_defs: &lambda.type_params,
                    package_id: self.package_id,
                    module_id: self.module_id,
                    file_id: self.file_id,
                    analysis: &mut analysis,
                    symtable: &mut self.symtable,
                    in_loop: false,
                    is_lambda: true,
                    param_types: lambda.param_types.clone(),
                    return_type: Some(lambda.return_type.clone()),
                    has_hidden_self_argument: true,
                    is_self_available: self.is_self_available,
                    vars: self.vars,
                    contains_lambda: false,
                    outer_context_classes: self.outer_context_classes,
                    outer_context_access_in_function: false,
                    outer_context_access_from_lambda: false,
                };

                typeck.check_fct(&*lambda, &node);
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
                &self.type_param_defs,
                check_type.clone(),
            );

            if !implements {
                let object_type = self.ty_name(&object_type);
                let check_type = self.ty_name(&check_type);

                self.sa.report(
                    self.file_id,
                    e.span,
                    ErrorMessage::TypeNotImplementingTrait(object_type, check_type),
                );
            }

            self.analysis.set_ty(e.id, check_type.clone());
            check_type
        } else if !check_type.is_error() {
            let name = self.ty_name(&check_type);
            self.sa
                .report(self.file_id, e.span, ErrorMessage::TraitExpected(name));
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
        let (ty, value_i64, value_f64) =
            check_lit_int(self.sa, self.file_id, e, negate, expected_ty);

        self.analysis.set_ty(e.id, ty.clone());
        self.analysis.set_literal_value(e.id, value_i64, value_f64);

        ty
    }

    fn check_expr_lit_float(
        &mut self,
        e: &ast::ExprLitFloatType,
        negate: bool,
        _expected_ty: SourceType,
    ) -> SourceType {
        let (ty, value) = check_lit_float(self.sa, self.file_id, e, negate);

        self.analysis.set_ty(e.id, ty.clone());
        self.analysis.set_literal_value(e.id, 0, value);

        ty
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
        let value = check_lit_char(self.sa, self.file_id, e);

        self.analysis.set_ty(e.id, SourceType::Char);
        self.analysis.set_literal_char(e.id, value);

        SourceType::Char
    }

    fn check_expr_lit_str(
        &mut self,
        e: &ast::ExprLitStrType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let value = check_lit_str(self.sa, self.file_id, e);

        let str_ty = self.sa.cls(self.sa.known.classes.string());
        self.analysis.set_ty(e.id, str_ty.clone());
        self.analysis.set_literal_string(e.id, value);

        str_ty
    }

    fn check_expr_template(
        &mut self,
        e: &ast::ExprTemplateType,
        expected_ty: SourceType,
    ) -> SourceType {
        let stringable_trait = self.sa.known.traits.stringable();
        let stringable_trait_ty = SourceType::new_trait(stringable_trait);

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = self.check_expr(part, SourceType::Any);

                if part_expr.is_error() {
                    continue;
                }

                if let SourceType::TypeParam(id) = part_expr {
                    if self
                        .type_param_defs
                        .implements_trait(id, stringable_trait_ty.clone())
                    {
                        continue;
                    }
                } else {
                    let stringable_impl_id = find_impl(
                        self.sa,
                        part_expr.clone(),
                        &self.type_param_defs,
                        stringable_trait_ty.clone(),
                    );

                    if let Some(stringable_impl_id) = stringable_impl_id {
                        let impl_ = self.sa.impls[stringable_impl_id].read();
                        let name = self.sa.interner.intern("toString");
                        let to_string_id = impl_
                            .instance_names
                            .get(&name)
                            .cloned()
                            .expect("method toString() not found");

                        self.analysis.map_templates.insert(part.id(), to_string_id);
                        continue;
                    }
                }

                let ty = self.ty_name(&part_expr);
                self.sa.report(
                    self.file_id,
                    part.span(),
                    ErrorMessage::ExpectedStringable(ty),
                );
            } else {
                match part.as_ref() {
                    ast::ExprData::LitStr(ref e) => {
                        self.check_expr_lit_str(e, expected_ty.clone());
                    }

                    _ => unreachable!(),
                }
            }
        }

        let str_ty = self.sa.cls(self.sa.known.classes.string());
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    fn check_expr(&mut self, e: &ast::ExprData, expected_ty: SourceType) -> SourceType {
        match *e {
            ast::ExprData::LitChar(ref expr) => self.check_expr_lit_char(expr, expected_ty),
            ast::ExprData::LitInt(ref expr) => self.check_expr_lit_int(expr, false, expected_ty),
            ast::ExprData::LitFloat(ref expr) => {
                self.check_expr_lit_float(expr, false, expected_ty)
            }
            ast::ExprData::LitStr(ref expr) => self.check_expr_lit_str(expr, expected_ty),
            ast::ExprData::Template(ref expr) => self.check_expr_template(expr, expected_ty),
            ast::ExprData::LitBool(ref expr) => self.check_expr_lit_bool(expr, expected_ty),
            ast::ExprData::Ident(ref expr) => self.check_expr_ident(expr, expected_ty),
            ast::ExprData::Un(ref expr) => self.check_expr_un(expr, expected_ty),
            ast::ExprData::Bin(ref expr) => self.check_expr_bin(expr, expected_ty),
            ast::ExprData::Call(ref expr) => self.check_expr_call(expr, expected_ty),
            ast::ExprData::TypeParam(ref expr) => self.check_expr_type_param(expr, expected_ty),
            ast::ExprData::Path(ref expr) => self.check_expr_path(expr, expected_ty),
            ast::ExprData::Dot(ref expr) => self.check_expr_dot(expr, expected_ty),
            ast::ExprData::This(ref expr) => self.check_expr_this(expr, expected_ty),
            ast::ExprData::Conv(ref expr) => self.check_expr_conv(expr, expected_ty),
            ast::ExprData::Lambda(ref expr) => self.check_expr_lambda(expr, expected_ty),
            ast::ExprData::Block(ref expr) => self.check_expr_block(expr, expected_ty),
            ast::ExprData::If(ref expr) => self.check_expr_if(expr, expected_ty),
            ast::ExprData::Tuple(ref expr) => self.check_expr_tuple(expr, expected_ty),
            ast::ExprData::Paren(ref expr) => self.check_expr_paren(expr, expected_ty),
            ast::ExprData::Match(ref expr) => self.check_expr_match(expr, expected_ty),
            ast::ExprData::For(ref expr) => self.check_expr_for(expr, expected_ty),
            ast::ExprData::While(ref expr) => self.check_expr_while(expr, expected_ty),
            ast::ExprData::Return(ref expr) => self.check_expr_return(expr, expected_ty),
            ast::ExprData::Break(..) | ast::ExprData::Continue(..) => {
                self.check_expr_break_and_continue(e, expected_ty)
            }
            ast::ExprData::Error { .. } => SourceType::Error,
        }
    }

    fn check_expr_break_and_continue(
        &mut self,
        expr: &ast::ExprData,
        _expected_ty: SourceType,
    ) -> SourceType {
        if !self.in_loop {
            self.sa
                .report(self.file_id, expr.span(), ErrorMessage::OutsideLoop);
        }

        SourceType::Unit
    }

    fn ty_name(&self, ty: &SourceType) -> String {
        ty.name_with_type_params(self.sa, self.type_param_defs)
    }
}

impl<'a> Visitor for TypeCheck<'a> {
    fn visit_expr(&mut self, _e: &ast::ExprData) {
        unreachable!();
    }

    fn visit_stmt(&mut self, s: &ast::StmtData) {
        match *s {
            ast::StmtData::Let(ref stmt) => self.check_stmt_let(stmt),

            ast::StmtData::Expr(ref stmt) => {
                self.check_expr(&stmt.expr, SourceType::Any);
            }
        }
    }
}

pub fn args_compatible_fct(
    sa: &Sema,
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

fn args_compatible(
    sa: &Sema,
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

fn arg_allows(sa: &Sema, def: SourceType, arg: SourceType, self_ty: Option<SourceType>) -> bool {
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

fn check_lit_str(sa: &Sema, file_id: SourceFileId, e: &ast::ExprLitStrType) -> String {
    let mut value = e.value.as_str();
    assert!(value.starts_with("\"") || value.starts_with("}"));
    value = &value[1..];

    let mut it = value.chars();
    let mut result = String::new();

    while it.as_str() != "\"" && it.as_str() != "${" && !it.as_str().is_empty() {
        let ch = parse_escaped_char(sa, file_id, e.span.start() + 1, &mut it);
        result.push(ch);
    }

    result
}

pub fn check_lit_char(sa: &Sema, file_id: SourceFileId, e: &ast::ExprLitCharType) -> char {
    let mut value = e.value.as_str();
    assert!(value.starts_with("\'"));
    value = &value[1..];

    if value.is_empty() {
        // unclosed char was already reported
        return '\0';
    } else if value == "\'" {
        // empty char literal ''
        sa.report(file_id, e.span, ErrorMessage::InvalidCharLiteral);
        return '\0';
    }

    let mut it = value.chars();
    let result = parse_escaped_char(sa, file_id, e.span.start() + 1, &mut it);

    // Check whether the char literal ends now.
    if it.as_str() != "\'" {
        sa.report(file_id, e.span, ErrorMessage::InvalidCharLiteral);
    }

    result
}

fn parse_escaped_char(sa: &Sema, file_id: SourceFileId, offset: u32, it: &mut Chars) -> char {
    let ch = it.next().expect("missing char");
    if ch == '\\' {
        if let Some(ch) = it.next() {
            match ch {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\"' => '\"',
                '\'' => '\'',
                '0' => '\0',
                '$' => '$',
                _ => {
                    let count = 1 + ch.len_utf8() as u32;
                    sa.report(
                        file_id,
                        Span::new(offset, count),
                        ErrorMessage::InvalidEscapeSequence,
                    );
                    '\0'
                }
            }
        } else {
            sa.report(
                file_id,
                Span::new(offset, 1),
                ErrorMessage::InvalidEscapeSequence,
            );
            '\0'
        }
    } else {
        ch
    }
}

pub fn check_lit_int(
    sa: &Sema,
    file: SourceFileId,
    e: &ast::ExprLitIntType,
    negate: bool,
    expected_type: SourceType,
) -> (SourceType, i64, f64) {
    let (base, value, suffix) = parse_lit_int(&e.value);
    let suffix_type = determine_suffix_type_int_literal(sa, file, e.span, &suffix);

    let ty = suffix_type.unwrap_or_else(|| match expected_type {
        SourceType::UInt8 if !negate => SourceType::UInt8,
        SourceType::Int32 => SourceType::Int32,
        SourceType::Int64 => SourceType::Int64,
        _ => SourceType::Int64,
    });

    if ty.is_float() {
        let value = value.parse::<f64>().expect("unparsable float");
        let value = if negate { -value } else { value };

        if base != 10 {
            sa.report(file, e.span, ErrorMessage::InvalidNumberFormat);
        }

        return (ty, 0, value);
    }

    if negate && ty == SourceType::UInt8 {
        sa.report(file, e.span, ErrorMessage::NegativeUnsigned);
    }

    let ty_name = ty.name(sa);
    let parsed_value = u64::from_str_radix(&value, base);

    let value = match parsed_value {
        Ok(value) => value,
        Err(_) => {
            sa.report(file, e.span, ErrorMessage::NumberLimitOverflow);
            return (ty, 0, 0.0);
        }
    };

    if base == 10 {
        let max = match ty {
            SourceType::UInt8 => 256,
            SourceType::Int32 => 1u64 << 31,
            SourceType::Int64 => 1u64 << 63,
            _ => unreachable!(),
        };

        if (negate && value > max) || (!negate && value >= max) {
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        let value = if negate {
            (value as i64).wrapping_neg()
        } else {
            value as i64
        };

        (ty, value, 0.0)
    } else {
        assert!(!negate);

        let max = match ty {
            SourceType::UInt8 => 256 as u64,
            SourceType::Int32 => u32::max_value() as u64,
            SourceType::Int64 => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        (ty, value as i64, 0.0)
    }
}

fn parse_lit_int(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}

fn determine_suffix_type_int_literal(
    sa: &Sema,
    file: SourceFileId,
    span: Span,
    suffix: &str,
) -> Option<SourceType> {
    match suffix {
        "u8" => Some(SourceType::UInt8),
        "i32" => Some(SourceType::Int32),
        "i64" => Some(SourceType::Int64),
        "f32" => Some(SourceType::Float32),
        "f64" => Some(SourceType::Float64),
        "" => None,
        _ => {
            sa.report(file, span, ErrorMessage::UnknownSuffix);
            None
        }
    }
}

pub fn check_lit_float(
    sa: &Sema,
    file: SourceFileId,
    e: &ast::ExprLitFloatType,
    negate: bool,
) -> (SourceType, f64) {
    let (base, value, suffix) = parse_lit_float(&e.value);

    if base != 10 {
        sa.report(file, e.span, ErrorMessage::InvalidNumberFormat);
    }

    let ty = match suffix.as_str() {
        "f32" => SourceType::Float32,
        "f64" => SourceType::Float64,
        "" => SourceType::Float64,
        _ => {
            sa.report(file, e.span, ErrorMessage::UnknownSuffix);
            SourceType::Float64
        }
    };

    let (min, max) = match ty {
        SourceType::Float32 => (f32::MIN as f64, f32::MAX as f64),
        SourceType::Float64 => (f64::MIN, f64::MAX),
        _ => unreachable!(),
    };

    let value = value.parse::<f64>().expect("unparsable float");
    let value = if negate { -value } else { value };

    if value < min || value > max {
        let name = match ty {
            SourceType::Float32 => "Float32",
            SourceType::Float64 => "Float64",
            _ => unreachable!(),
        };
        sa.report(file, e.span, ErrorMessage::NumberOverflow(name.into()));
    }

    (ty, value)
}

fn parse_lit_float(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();
    let mut allow_scientific = true;

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) || ch == '.' || ch == '-' || ch == '+' {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else if (ch == 'e' || ch == 'E') && allow_scientific {
            filtered_value.push(ch);
            it.next();
            allow_scientific = false;
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}

struct MethodDescriptor {
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    return_type: SourceType,
}

fn lookup_method(
    sa: &Sema,
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

fn is_simple_enum(sa: &Sema, ty: SourceType) -> bool {
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

fn check_expr_call_struct_args(
    sa: &Sema,
    struct_: &StructDefinition,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> bool {
    if struct_.fields.len() != arg_types.len() {
        return false;
    }

    for (def_ty, arg_ty) in struct_.fields.iter().zip(arg_types) {
        let def_ty = replace_type_param(sa, def_ty.ty(), &type_params, None);

        if !def_ty.allows(sa, arg_ty.clone()) {
            return false;
        }
    }

    true
}

fn check_expr_call_class_args(
    sa: &Sema,
    cls: &ClassDefinition,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> bool {
    if cls.fields.len() != arg_types.len() {
        return false;
    }

    for (def_ty, arg_ty) in cls.fields.iter().zip(arg_types) {
        let def_ty = replace_type_param(sa, def_ty.ty(), &type_params, None);

        if !def_ty.allows(sa, arg_ty.clone()) {
            return false;
        }
    }

    true
}
