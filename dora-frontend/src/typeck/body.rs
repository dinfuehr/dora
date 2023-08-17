use std::convert::TryFrom;
use std::str::Chars;
use std::sync::Arc;
use std::{f32, f64};

use once_cell::unsync::OnceCell;
use parking_lot::RwLock;

use crate::access::{
    class_field_accessible_from, const_accessible_from, enum_accessible_from,
    global_accessible_from, module_accessible_from, struct_field_accessible_from,
};
use crate::error::msg::ErrorMessage;
use crate::program_parser::ParsedModifierList;
use crate::report_sym_shadow_span;
use crate::sema::{
    find_field_in_class, find_impl, find_methods_in_class, find_methods_in_enum,
    find_methods_in_struct, implements_trait, AnalysisData, CallType, ClassDefinition, ContextIdx,
    ContextInfo, EnumDefinitionId, FctDefinition, FctDefinitionId, FctParent, Field, FieldId,
    GlobalDefinition, IdentType, ModuleDefinitionId, NestedVarId, OuterContextResolver,
    PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Var, VarAccess, VarId,
    VarLocation, Visibility,
};
use crate::specialize::replace_type_param;
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::ty::{SourceType, SourceTypeArray};
use crate::typeck::{check_expr, check_expr_call_enum_args};
use crate::typeparamck::{self, ErrorReporting};
use crate::{always_returns, expr_always_returns, read_type, AllowSelf};

use crate::interner::Name;
use dora_bytecode::Intrinsic;
use dora_parser::ast;
use dora_parser::Span;

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
            let expr_ty = check_expr(self_, expr, global.ty.clone());

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

                check_expr(self, value, fct_return_type.clone())
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
            self.symtable.insert(name, SymbolKind::TypeParam(id));
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
            let replaced_sym = self.symtable.insert(name, SymbolKind::Var(var_id));
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

    pub(super) fn add_local(&mut self, id: NestedVarId, span: Span) {
        let name = self.vars.get_var(id).name;
        let existing_symbol = self.symtable.insert(name, SymbolKind::Var(id));

        if let Some(existing_symbol) = existing_symbol {
            if !existing_symbol.kind().is_var() {
                report_sym_shadow_span(self.sa, name, self.file_id, span, existing_symbol)
            }
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
            .map(|expr| check_expr(self, &expr, defined_type.clone()))
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

    pub(super) fn read_type(&mut self, t: &ast::TypeData) -> SourceType {
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

    pub(super) fn check_stmt_let_pattern(&mut self, pattern: &ast::LetPattern, ty: SourceType) {
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

    pub(super) fn check_fct_return_type(
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
            Some(SymbolKind::Var(var_id)) => {
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

            Some(SymbolKind::Global(global_id)) => {
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

        let rhs_type = check_expr(self, &e.rhs, lhs_type.clone());

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
        let expr_type = check_expr(self, &call.callee, SourceType::Any);

        let mut arg_types: Vec<SourceType> = call
            .args
            .iter()
            .map(|arg| check_expr(self, arg, SourceType::Any))
            .collect();

        let value_type = check_expr(self, &e.rhs, SourceType::Any);

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

        let object_type = check_expr(self, &field_expr.lhs, SourceType::Any);

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

                let rhs_type = check_expr(self, &e.rhs, fty.clone());

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
            check_expr(self, &e.rhs, SourceType::Any);

            self.analysis.set_ty(e.id, SourceType::Unit);
            return;
        }

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        check_expr(self, &e.rhs, SourceType::Any);

        // field not found, report error
        let expr_name = self.ty_name(&object_type);
        let msg = ErrorMessage::UnknownField(name, expr_name);
        self.sa.report(self.file_id, field_expr.op_span, msg);

        self.analysis.set_ty(e.id, SourceType::Unit);
    }

    pub(super) fn find_method(
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

    pub(super) fn check_expr_un(
        &mut self,
        e: &ast::ExprUnType,
        expected_ty: SourceType,
    ) -> SourceType {
        if e.op == ast::UnOp::Neg && e.opnd.is_lit_int() {
            let expr_type =
                self.check_expr_lit_int(e.opnd.to_lit_int().unwrap(), true, expected_ty);
            self.analysis.set_ty(e.id, expr_type.clone());
            return expr_type;
        }

        let opnd = check_expr(self, &e.opnd, SourceType::Any);

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

    pub(super) fn check_expr_bin(
        &mut self,
        e: &ast::ExprBinType,
        _expected_ty: SourceType,
    ) -> SourceType {
        if e.op.is_any_assign() {
            self.check_expr_assign(e);
            return SourceType::Unit;
        }

        let lhs_type = check_expr(self, &e.lhs, SourceType::Any);
        let rhs_type = check_expr(self, &e.rhs, SourceType::Any);

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

    pub(super) fn check_enum_value_with_args(
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

        if !check_expr_call_enum_args(self, enum_id, type_params.clone(), variant, arg_types) {
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

    pub(super) fn check_expr_path(
        &mut self,
        e: &ast::ExprPathType,
        expected_ty: SourceType,
    ) -> SourceType {
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
            Some(SymbolKind::Enum(id)) => self.check_enum_value_without_args(
                e.id,
                e.op_span,
                expected_ty,
                id,
                type_params,
                element_name,
            ),

            Some(SymbolKind::Module(module_id)) => {
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

    pub(super) fn read_path_expr(
        &mut self,
        expr: &ast::ExprData,
    ) -> Result<Option<SymbolKind>, ()> {
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
                Some(SymbolKind::Module(module_id)) => {
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

    pub(super) fn read_path(&mut self, path: &ast::PathData) -> Result<SymbolKind, ()> {
        let names = &path.names;
        let mut sym = self.symtable.get_string(self.sa, &names[0].name_as_string);

        for ident in &names[1..] {
            match sym {
                Some(SymbolKind::Module(module_id)) => {
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

                Some(SymbolKind::Enum(enum_id)) => {
                    let enum_ = self.sa.enums[enum_id].read();

                    if !enum_accessible_from(self.sa, enum_id, self.module_id) {
                        let msg = ErrorMessage::NotAccessible(enum_.name(self.sa));
                        self.sa.report(self.file_id, path.span, msg);
                    }

                    let iname = self.sa.interner.intern(&ident.name_as_string);

                    if let Some(&variant_idx) = enum_.name_to_value.get(&iname) {
                        sym = Some(SymbolKind::EnumVariant(enum_id, variant_idx));
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
            Some(SymbolKind::Global(global_id)) => {
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

            Some(SymbolKind::Const(const_id)) => {
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

            Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => self
                .check_enum_value_without_args_id(
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

    pub(super) fn check_expr_type_param(
        &mut self,
        e: &ast::ExprTypeParamType,
        expected_ty: SourceType,
    ) -> SourceType {
        let type_params: Vec<SourceType> = e.args.iter().map(|p| self.read_type(p)).collect();
        let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

        if let Some(ident) = e.callee.to_ident() {
            let sym = self.symtable.get_string(self.sa, &ident.name);

            match sym {
                Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => self
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
                Some(SymbolKind::Enum(enum_id)) => self.check_enum_value_without_args(
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

    pub(super) fn check_enum_value_without_args_id(
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

    pub(super) fn check_expr_dot(
        &mut self,
        e: &ast::ExprDotType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let object_type = check_expr(self, &e.lhs, SourceType::Any);

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
                    self.sa.report(self.file_id, e.rhs.span(), msg);
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
                    self.sa.report(self.file_id, e.rhs.span(), msg);
                }

                self.analysis.set_ty(e.id, fty.clone());
                return fty;
            }
        }

        // field not found, report error
        if !object_type.is_error() {
            let expr_name = self.ty_name(&object_type);
            let msg = ErrorMessage::UnknownField(name, expr_name);
            self.sa.report(self.file_id, e.rhs.span(), msg);
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
                self.sa.report(self.file_id, e.rhs.span(), msg);

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

    pub(super) fn check_expr_this(
        &mut self,
        e: &ast::ExprSelfType,
        _expected_ty: SourceType,
    ) -> SourceType {
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

    pub(super) fn check_expr_lambda(
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

    pub(super) fn check_expr_conv(
        &mut self,
        e: &ast::ExprConvType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let object_type = check_expr(self, &e.object, SourceType::Any);
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

    pub(super) fn check_expr_lit_int(
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

    pub(super) fn check_expr_lit_float(
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

    pub(super) fn check_expr_lit_bool(
        &mut self,
        e: &ast::ExprLitBoolType,
        _expected_ty: SourceType,
    ) -> SourceType {
        self.analysis.set_ty(e.id, SourceType::Bool);

        SourceType::Bool
    }

    pub(super) fn check_expr_lit_char(
        &mut self,
        e: &ast::ExprLitCharType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let value = check_lit_char(self.sa, self.file_id, e);

        self.analysis.set_ty(e.id, SourceType::Char);
        self.analysis.set_literal_char(e.id, value);

        SourceType::Char
    }

    pub(super) fn check_expr_lit_str(
        &mut self,
        e: &ast::ExprLitStrType,
        _expected_ty: SourceType,
    ) -> SourceType {
        let value = check_lit_str(self.sa, self.file_id, e);

        let str_ty = SourceType::Class(self.sa.known.classes.string(), SourceTypeArray::empty());
        self.analysis.set_ty(e.id, str_ty.clone());
        self.analysis.set_literal_string(e.id, value);

        str_ty
    }

    pub(super) fn check_expr_template(
        &mut self,
        e: &ast::ExprTemplateType,
        expected_ty: SourceType,
    ) -> SourceType {
        let stringable_trait = self.sa.known.traits.stringable();
        let stringable_trait_ty = SourceType::new_trait(stringable_trait);

        for (idx, part) in e.parts.iter().enumerate() {
            if idx % 2 != 0 {
                let part_expr = check_expr(self, part, SourceType::Any);

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

        let str_ty = SourceType::Class(self.sa.known.classes.string(), SourceTypeArray::empty());
        self.analysis.set_ty(e.id, str_ty.clone());

        str_ty
    }

    pub(super) fn check_expr_break_and_continue(
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

    pub(super) fn ty_name(&self, ty: &SourceType) -> String {
        ty.name_with_type_params(self.sa, self.type_param_defs)
    }

    pub(super) fn visit_stmt(&mut self, s: &ast::StmtData) {
        match *s {
            ast::StmtData::Let(ref stmt) => self.check_stmt_let(stmt),

            ast::StmtData::Expr(ref stmt) => {
                check_expr(self, &stmt.expr, SourceType::Any);
            }
        }
    }
}

pub(super) fn args_compatible_fct(
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

pub(super) fn args_compatible(
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

pub(super) struct MethodDescriptor {
    pub fct_id: FctDefinitionId,
    pub type_params: SourceTypeArray,
    pub return_type: SourceType,
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

    pub(super) fn local_var_id(&self, var_id: NestedVarId) -> VarId {
        assert!(var_id.0 >= self.current_function().start_idx);
        VarId(var_id.0 - self.current_function().start_idx)
    }

    pub(super) fn check_context_allocated(
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

    pub(super) fn add_var(&mut self, name: Name, ty: SourceType, mutable: bool) -> NestedVarId {
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

    pub(super) fn get_var(&self, idx: NestedVarId) -> &VarDefinition {
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
