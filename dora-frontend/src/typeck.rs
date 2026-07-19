use std::cell::OnceCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::str::Chars;
use std::{f32, f64};

use dora_bytecode::ConstValue;
use dora_parser::Span;
use dora_parser::ast::{self, AstCommaList, SyntaxNode, SyntaxNodeBase};

use crate::error::DescriptorArgs;
use crate::error::diagnostics::{
    ASSIGN_TYPE, DiagnosticDescriptor, INVALID_CHAR_LITERAL, INVALID_ESCAPE_SEQUENCE,
    INVALID_NUMBER_FORMAT, NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS, NEGATIVE_UNSIGNED,
    NUMBER_LIMIT_OVERFLOW, NUMBER_OVERFLOW, RETURN_TYPE, UNKNOWN_SUFFIX, UNUSED_VARIABLE,
};
use crate::interner::Name;
use crate::sema::{
    Body, CallArg, ClassDefinition, ConstValue as SemaConstValue, ContextData, ContextFieldId,
    ContextId, Element, Expr, ExprId, ExprMapId, FctDefinition, FctParent, FieldDefinition,
    FieldIndex, GlobalDefinition, IdentType, LambdaExpr, ModuleDefinitionId, NestedScopeId,
    NestedVarId, PackageDefinitionId, Param, PatternId, ScopeId, Sema, SourceFileId, StmtId,
    TypeParamDefinition, TypeRefId, Var, VarAccess, VarId, VarLocation, Visibility, check_type_ref,
    convert_trait_type_ref, convert_type_ref, new_identity_type_params, parse_type_ref,
};
use crate::sym::ModuleSymTable;
use crate::typeck::constck::ConstCheck;
use crate::typeck::expr::check_expr;
use crate::typeck::pattern::check_pattern;
use crate::typeck::stmt::check_stmt;
use crate::typeck::type_params::check_type_params;
use crate::{
    ParsedType, SourceType, SymbolKind, always_returns, args, expr_always_returns,
    report_sym_shadow_span,
};

mod constck;
mod expr;
mod lookup;
mod pattern;
mod stmt;
#[cfg(test)]
mod tests;
mod type_params;

pub use lookup::find_method_call_candidates;

pub fn check(sa: &mut Sema) {
    let mut contexts = Vec::new();
    let mut lambda_definitions = Vec::new();

    for (_id, fct) in sa.fcts.iter() {
        if fct.has_body(sa) {
            check_function(sa, fct, &mut contexts, &mut lambda_definitions);
        }
    }

    for (_const_id, const_) in sa.consts.iter() {
        let (_, value) = {
            if const_.has_expr(sa) {
                let body = const_.body();
                let mut constck = ConstCheck {
                    sa,
                    const_: &*const_,
                    body,
                };
                constck.check_expr(body.root_expr_id())
            } else {
                (SourceType::Error, ConstValue::None)
            }
        };

        const_.value.set(value).expect("already initialized");
    }

    for (_id, global) in sa.globals.iter() {
        check_global(sa, global, &mut contexts, &mut lambda_definitions);
    }

    create_context_classes(sa, &mut contexts);
    sa.contexts = contexts;
    create_lambda_functions(sa, lambda_definitions);
}

fn check_function(
    sa: &Sema,
    fct: &FctDefinition,
    contexts: &mut Vec<ContextData>,
    lambda_definitions: &mut Vec<FctDefinition>,
) {
    let analysis = fct.body();
    let mut symtable = ModuleSymTable::new(sa, fct.module_id);
    let mut vars = VarManager::new();
    let mut active_contexts = Vec::new();

    let self_ty = match fct.parent {
        FctParent::None => None,
        FctParent::Extension(id) => Some(sa.extension(id).ty().clone()),
        FctParent::Impl(id) => Some(sa.impl_(id).extended_ty()),
        FctParent::Trait(..) => Some(SourceType::This),
        FctParent::Function => unreachable!(),
    };

    let mut typeck = TypeCheck {
        sa,
        type_param_definition: fct.type_param_definition(),
        package_id: fct.package_id,
        module_id: fct.module_id,
        file_id: fct.file_id,
        body: analysis,
        symtable: &mut symtable,
        param_types: fct.params_with_self().to_owned(),
        is_variadic: fct.params.is_variadic,
        return_type: Some(fct.return_type()),
        in_loop: false,
        parent: fct.parent.clone(),
        has_hidden_self_argument: fct.has_hidden_self_argument(),
        is_self_available: fct.has_hidden_self_argument(),
        is_mutating: fct.is_mutating,
        self_ty,
        is_lambda: false,
        vars: &mut vars,
        lambda_definitions,
        contexts,
        active_contexts: &mut active_contexts,
        start_context_idx: 0,
        needs_context_slot_in_lambda_object: false,
        element: fct,
    };

    typeck.check_fct();
}

fn check_global(
    sa: &Sema,
    global: &GlobalDefinition,
    contexts: &mut Vec<ContextData>,
    lambda_definitions: &mut Vec<FctDefinition>,
) {
    {
        if !global.has_initial_value() {
            return;
        }

        let analysis = global.body();
        let mut symtable = ModuleSymTable::new(sa, global.module_id);
        let mut vars = VarManager::new();
        let mut active_contexts = Vec::new();

        let mut typeck = TypeCheck {
            sa,
            type_param_definition: global.type_param_definition(),
            package_id: global.package_id,
            module_id: global.module_id,
            file_id: global.file_id,
            body: analysis,
            symtable: &mut symtable,
            in_loop: false,
            is_lambda: false,
            param_types: Vec::new(),
            is_variadic: false,
            return_type: None,
            parent: FctParent::None,
            has_hidden_self_argument: false,
            is_self_available: false,
            is_mutating: false,
            self_ty: None,
            vars: &mut vars,
            lambda_definitions,
            contexts,
            active_contexts: &mut active_contexts,
            start_context_idx: 0,
            needs_context_slot_in_lambda_object: false,
            element: global,
        };

        typeck.check_initializer(&*global, analysis.root_expr_id());
    }
}

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
    pub type_param_definition: &'a Rc<TypeParamDefinition>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub body: &'a Body,
    pub symtable: &'a mut ModuleSymTable,
    pub param_types: Vec<Param>,
    pub is_variadic: bool,
    pub return_type: Option<SourceType>,
    pub in_loop: bool,
    pub is_lambda: bool,
    pub parent: FctParent,
    pub has_hidden_self_argument: bool,
    pub is_self_available: bool,
    pub is_mutating: bool,
    pub self_ty: Option<SourceType>,
    pub vars: &'a mut VarManager,
    pub element: &'a dyn Element,
    // All potential contexts and the stack of contexts active at this point.
    pub contexts: &'a mut Vec<ContextData>,
    pub active_contexts: &'a mut Vec<ContextId>,
    pub start_context_idx: usize,
    pub needs_context_slot_in_lambda_object: bool,
    // Lambda functions discovered while checking functions.
    pub lambda_definitions: &'a mut Vec<FctDefinition>,
}

impl<'a> TypeCheck<'a> {
    pub fn report(
        &self,
        span: Span,
        desc: &'static DiagnosticDescriptor,
        args: crate::error::DescriptorArgs,
    ) {
        self.sa.report(self.file_id, span, desc, args);
    }

    pub fn report_stmt_id(
        &self,
        id: StmtId,
        desc: &'static DiagnosticDescriptor,
        args: DescriptorArgs,
    ) {
        let ptr = self.body.stmts().syntax_node_ptr(id);
        let node = self.sa.syntax::<SyntaxNode>(self.file_id, ptr);
        self.sa.report(self.file_id, node.span(), desc, args);
    }

    pub fn expr(&self, expr_id: ExprId) -> &'a Expr {
        self.body.expr(expr_id)
    }

    pub fn call_args(&self, call_expr_id: ExprId) -> &[CallArg] {
        match self.expr(call_expr_id) {
            Expr::Call(expr) => &expr.args,
            Expr::MethodCall(expr) => &expr.args,
            _ => panic!("call expression expected"),
        }
    }

    pub fn check_fct(&mut self) {
        self.check_common(|self_| {
            self_.add_type_params();
            self_.add_params();
            let block_expr_id = self.body.root_expr_id();
            self_.check_body(block_expr_id);
        })
    }

    pub fn check_lambda(&mut self, expr: &LambdaExpr) {
        self.check_common(|self_| {
            self_.add_type_params();
            self_.add_params();
            self_.check_body(expr.block);
        })
    }

    pub fn check_initializer(&mut self, global: &GlobalDefinition, expr_id: ExprId) {
        // Global initializer never has self.
        self.body.set_has_self(false);

        self.check_common(move |self_| {
            let expr_ty = check_expr(self_, expr_id, global.ty());

            if !global.ty().is_error()
                && !expr_ty.is_error()
                && !global.ty().allows(self_.sa, expr_ty.clone())
            {
                let global_ty = self_.ty_name(&global.ty());
                let expr_ty = self_.ty_name(&expr_ty);
                self_.report(global.span, &ASSIGN_TYPE, args!(global_ty, expr_ty));
            }
        })
    }

    fn check_common<F>(&mut self, fct: F)
    where
        F: FnOnce(&mut TypeCheck<'a>),
    {
        let start_level = self.symtable.levels();
        let start_context_count = self.active_contexts.len();
        self.enter_function_scope();
        self.symtable.push_level();

        fct(self);

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.leave_function_scope();
        assert_eq!(self.active_contexts.len(), start_context_count);
    }

    fn check_body(&mut self, block_expr_id: ExprId) {
        let fct_return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        let block_expr = self.body.expr(block_expr_id).as_block();
        let stmts = block_expr.stmts.clone();
        let tail_expr = block_expr.expr;

        let mut returns = false;

        for stmt_id in stmts {
            check_stmt(self, stmt_id);

            if always_returns(self.body, stmt_id) {
                returns = true;
            }
        }

        let return_type = if let Some(expr_id) = tail_expr {
            if expr_always_returns(self.body, expr_id) {
                returns = true;
            }

            check_expr(self, expr_id, fct_return_type.clone())
        } else {
            SourceType::Unit
        };

        if !returns {
            let block_span = self.expr_span(block_expr_id);
            self.check_fct_return_type(fct_return_type, block_span, return_type);
        }
    }

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        if !self.vars.is_context_var(var_id) {
            return IdentType::Var(self.vars.local_var_id(var_id));
        }

        let field_id = self.vars.ensure_context_allocated(var_id);
        let NestedScopeId(context_idx) = self.vars.get_var(var_id).scope_id;
        let context_id = self.active_contexts[context_idx];

        // We need parent slots from the context of the variable up to (not including)
        // the first context of this function. There is no need for parent slots for
        // contexts within this function because those are available in local registers.
        for context_idx in context_idx + 1..self.start_context_idx {
            let context_id = self.active_contexts[context_idx];
            self.contexts[context_id.0].require_parent_slot();
        }

        assert!(self.is_lambda);
        self.needs_context_slot_in_lambda_object = true;
        IdentType::Context(context_id, field_id)
    }

    fn enter_function_scope(&mut self) {
        self.start_context_idx = self.active_contexts.len();
        self.enter_context();
        self.vars.enter_function_scope();
    }

    pub fn enter_block_scope(&mut self) {
        self.enter_context();
        self.vars.enter_block_scope();
    }

    fn enter_context(&mut self) {
        let context_id = ContextId(self.contexts.len());
        let parent = self.active_contexts.last().copied();
        self.contexts.push(ContextData::new(parent));
        self.active_contexts.push(context_id);
    }

    fn leave_function_scope(&mut self) {
        let context_id = self.active_contexts.pop().expect("missing context");

        if self.vars.has_context_vars() {
            self.setup_context_class(context_id);
        }

        let needs_context_slot_in_lambda_object = self.needs_context_slot_in_lambda_object
            || self.contexts[context_id.0].has_parent_slot();

        if needs_context_slot_in_lambda_object {
            assert!(self.is_lambda);
        }

        self.body
            .set_needs_context_slot_in_lambda_object(needs_context_slot_in_lambda_object);
        self.body.set_function_context_id(context_id);

        // Store var definitions for all local and context vars defined in this function.
        let vars = self.vars.leave_function_scope();

        for var in &vars {
            let Some(span) = var.span else {
                continue;
            };

            let name = self.sa.interner.str(var.name);
            if !var.used && !name.starts_with('_') {
                self.sa.warn(
                    self.file_id,
                    span,
                    &UNUSED_VARIABLE,
                    args!(name.to_string()),
                );
            }
        }

        let vars = vars
            .into_iter()
            .map(|vd| Var {
                ty: vd.ty.clone(),
                location: vd.location,
            })
            .collect();

        self.body.set_vars(VarAccess::new(vars));
    }

    pub fn leave_block_scope<T: ExprMapId>(&mut self, id: T) -> ContextId {
        let context_id = self.active_contexts.pop().expect("missing context");

        if self.vars.has_context_vars() {
            self.setup_context_class(context_id);
        }

        self.body.insert_block_context_id(id, context_id);

        self.vars.leave_block_scope();

        context_id
    }

    fn setup_context_class(&mut self, context_id: ContextId) {
        let scope = self.vars.current_scope();
        let number_fields = scope.next_field_id;
        let field_offset = usize::from(self.contexts[context_id.0].has_parent_slot());
        let mut fields = Vec::with_capacity(number_fields + field_offset);
        let map: Vec<OnceCell<NestedVarId>> = vec![OnceCell::new(); number_fields];

        for &var_id in &scope.vars {
            let var = self.vars.get_var(var_id);
            match var.location {
                VarLocation::Context(_scope_id, field_idx) => {
                    let ContextFieldId(field_id) = field_idx;
                    assert!(map[field_id].set(var.id).is_ok());
                }
                VarLocation::Stack => {}
            }
        }

        for var_id in map {
            let var_id = var_id.get().cloned().expect("missing field");
            let var = self.vars.get_var(var_id);

            let field = FieldDefinition {
                id: None,
                name: Some(var.name),
                span: None,
                index: FieldIndex(fields.len() + field_offset),
                parsed_ty: ParsedType::new_ty(var.ty.clone()),
                mutable: true,
                visibility: Visibility::Module,
                file_id: Some(self.file_id),
                module_id: self.module_id,
                package_id: self.package_id,
            };

            fields.push(field);
        }

        let name = self.sa.generate_context_name();
        let name = self.sa.interner.intern(&name);

        let class = ClassDefinition::new_without_source(
            self.package_id,
            self.module_id,
            Some(self.file_id),
            None,
            name,
            Visibility::Public,
            self.type_param_definition.clone(),
        );

        self.contexts[context_id.0].set_class_data(class, fields);
    }

    fn add_type_params(&mut self) {
        for (id, name) in self.type_param_definition.names() {
            self.symtable.insert(name, SymbolKind::TypeParam(id));
        }
    }

    fn add_params(&mut self) {
        self.add_hidden_parameter_self();

        let self_count = if self.has_hidden_self_argument { 1 } else { 0 };

        let param_types = self
            .param_types
            .iter()
            .skip(self_count)
            .map(|p| p.ty())
            .collect::<Vec<_>>();

        let param_pattern_ids = self.body.param_pattern_ids();
        let real_params_count = param_pattern_ids.len();
        assert_eq!(self.param_types.len(), param_pattern_ids.len() + self_count);

        let mut bound_params = HashSet::new();

        for (ind, (param_ty, &pattern_id)) in param_types
            .into_iter()
            .zip(param_pattern_ids.iter())
            .enumerate()
        {
            // Is this last argument of function with variadic arguments?
            let ty = if ind == real_params_count - 1 && self.is_variadic {
                // The type of variable is Array[T].
                self.sa.known.array_ty(param_ty)
            } else {
                param_ty
            };

            self.body.set_ty(pattern_id, ty.clone());

            let local_bound_params = check_pattern(self, pattern_id, ty);

            for (name, data) in local_bound_params {
                if !bound_params.insert(name) {
                    let name = self.sa.interner.str(name).to_string();
                    self.report(data.span, &NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS, args!(name));
                }
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        self.body.set_has_self(self.has_hidden_self_argument);

        if !self.has_hidden_self_argument {
            return;
        }

        // Only functions can use `self`.
        let hidden_self_ty = if self.is_lambda {
            assert_eq!(SourceType::Ptr, self.param_types[0].ty());
            SourceType::Ptr
        } else {
            let ty = self.self_ty.clone().expect("self expected");
            // For mutating methods on value types, self is passed by reference.
            if self.is_mutating && (ty.is_struct() || ty.is_tuple()) {
                SourceType::Ref(Box::new(ty))
            } else {
                ty
            }
        };

        assert!(!self.vars.has_vars());
        let name = self.sa.interner.intern("self");
        self.vars.add_var(name, hidden_self_ty, false, None);
    }

    pub(super) fn read_type(&mut self, id: TypeRefId) -> SourceType {
        let type_refs = self.body.type_refs();
        parse_type_ref(
            self.sa,
            type_refs,
            &self.symtable,
            self.file_id,
            self.element,
            id,
        );
        let ty = convert_type_ref(self.sa, type_refs, self.element, id);
        let allow_self = self.self_ty.is_some();
        let ty = check_type_ref(self.sa, type_refs, self.element, id, ty, allow_self);
        crate::parsety::expand_st(self.sa, self.element, ty, self.self_ty.clone())
    }

    /// Read a trait type for qualified path expressions like `[T as Trait]::Item`.
    /// Unlike `read_type`, this doesn't require all associated type bindings to be specified.
    pub(super) fn read_trait_type_for_qualified_path(
        &mut self,
        id: TypeRefId,
    ) -> Option<crate::TraitType> {
        let type_refs = self.body.type_refs();
        parse_type_ref(
            self.sa,
            type_refs,
            &self.symtable,
            self.file_id,
            self.element,
            id,
        );
        // Don't require all bindings for qualified paths
        convert_trait_type_ref(self.sa, type_refs, self.element, id, false)
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

            self.report(span, &RETURN_TYPE, args!(fct_type, expr_type));
        }
    }

    pub(super) fn ty_name(&self, ty: &SourceType) -> String {
        ty.name_with_type_params(self.sa, self.type_param_definition)
    }

    pub fn syntax<T: SyntaxNodeBase>(&self, id: ExprId) -> T {
        let ptr = self.body.exprs().syntax_node_ptr(id);
        self.sa.file(self.file_id).ast().syntax_by_ptr(ptr)
    }

    pub fn expr_span(&self, id: ExprId) -> Span {
        let ptr = self.body.exprs().syntax_node_ptr(id);
        let node = self.sa.syntax::<SyntaxNode>(self.file_id, ptr);
        node.span()
    }

    pub fn pattern_span(&self, id: PatternId) -> Span {
        let ptr = self.body.patterns().syntax_node_ptr(id);
        let node = self.sa.syntax::<SyntaxNode>(self.file_id, ptr);
        node.span()
    }

    pub fn pattern_syntax<T: SyntaxNodeBase>(&self, id: PatternId) -> T {
        let ptr = self.body.patterns().syntax_node_ptr(id);
        self.sa.file(self.file_id).ast().syntax_by_ptr(ptr)
    }
}

pub(super) fn add_local(
    sa: &Sema,
    symtable: &mut ModuleSymTable,
    vars: &VarManager,
    id: NestedVarId,
    file_id: SourceFileId,
    span: Span,
) {
    let name = vars.get_var(id).name;
    let existing_symbol = symtable.insert(name, SymbolKind::Var(id));

    if let Some(existing_symbol) = existing_symbol {
        if !existing_symbol.kind().is_var() {
            report_sym_shadow_span(sa, name, file_id, span, existing_symbol)
        }
    }
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
                        &INVALID_ESCAPE_SEQUENCE,
                        args!(),
                    );
                    '\0'
                }
            }
        } else {
            sa.report(
                file_id,
                Span::new(offset, 1),
                &INVALID_ESCAPE_SEQUENCE,
                args!(),
            );
            '\0'
        }
    } else {
        ch
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
            sa.report(file, span, &UNKNOWN_SUFFIX, args!());
            None
        }
    }
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

pub fn check_lit_int_from_text(
    sa: &Sema,
    file: SourceFileId,
    text: &str,
    span: Span,
    negate: bool,
    expected_type: SourceType,
) -> (SourceType, SemaConstValue) {
    let (base, value, suffix) = parse_lit_int(text);
    let suffix_type = determine_suffix_type_int_literal(sa, file, span, &suffix);

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
            sa.report(file, span, &INVALID_NUMBER_FORMAT, args!());
        }

        return (ty, SemaConstValue::Float(value));
    }

    if negate && ty == SourceType::UInt8 {
        sa.report(file, span, &NEGATIVE_UNSIGNED, args!());
    }

    let ty_name = ty.name(sa);
    let parsed_value = u64::from_str_radix(&value, base);

    let value = match parsed_value {
        Ok(value) => value,
        Err(_) => {
            sa.report(file, span, &NUMBER_LIMIT_OVERFLOW, args!());
            return (ty, SemaConstValue::Int(0));
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
            sa.report(file, span, &NUMBER_OVERFLOW, args!(ty_name));
        }

        let value = if negate {
            (value as i64).wrapping_neg()
        } else {
            value as i64
        };

        (ty, SemaConstValue::Int(value))
    } else {
        assert!(!negate);

        let max = match ty {
            SourceType::UInt8 => 256 as u64,
            SourceType::Int32 => u32::max_value() as u64,
            SourceType::Int64 => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            sa.report(file, span, &NUMBER_OVERFLOW, args!(ty_name));
        }

        let value = match ty {
            SourceType::UInt8 => i64::from(value as u8),
            SourceType::Int32 => i64::from(value as i32),
            SourceType::Int64 => value as i64,
            _ => unreachable!(),
        };

        (ty, SemaConstValue::Int(value))
    }
}

pub fn check_lit_float_from_text(
    sa: &Sema,
    file: SourceFileId,
    text: &str,
    span: Span,
    negate: bool,
) -> (SourceType, f64) {
    let (base, value, suffix) = parse_lit_float(text);

    if base != 10 {
        sa.report(file, span, &INVALID_NUMBER_FORMAT, args!());
    }

    let ty = match suffix.as_str() {
        "f32" => SourceType::Float32,
        "f64" => SourceType::Float64,
        "" => SourceType::Float64,
        _ => {
            sa.report(file, span, &UNKNOWN_SUFFIX, args!());
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
        sa.report(file, span, &NUMBER_OVERFLOW, args!(name.to_string()));
    }

    (ty, value)
}

pub fn check_lit_char_from_text(sa: &Sema, file_id: SourceFileId, text: &str, span: Span) -> char {
    let mut value = text;
    assert!(value.starts_with("\'"));
    value = &value[1..];

    if value.is_empty() {
        // unclosed char was already reported
        return '\0';
    } else if value == "\'" {
        // empty char literal ''
        sa.report(file_id, span, &INVALID_CHAR_LITERAL, args!());
        return '\0';
    }

    let mut it = value.chars();
    let result = parse_escaped_char(sa, file_id, span.start() + 1, &mut it);

    // Check whether the char literal ends now.
    if it.as_str() != "\'" {
        sa.report(file_id, span, &INVALID_CHAR_LITERAL, args!());
    }

    result
}

pub fn check_lit_str_from_text(sa: &Sema, file_id: SourceFileId, text: &str, span: Span) -> String {
    let mut value = text;
    assert!(value.starts_with("\"") || value.starts_with("}"));
    value = &value[1..];

    let mut it = value.chars();
    let mut result = String::new();

    while it.as_str() != "\"" && it.as_str() != "${" && !it.as_str().is_empty() {
        let ch = parse_escaped_char(sa, file_id, span.start() + 1, &mut it);
        result.push(ch);
    }

    result
}

pub(super) fn is_simple_enum(sa: &Sema, ty: SourceType) -> bool {
    match ty {
        SourceType::Enum(enum_id, _) => {
            let enum_ = &sa.enum_(enum_id);
            enum_.is_simple_enum()
        }

        _ => false,
    }
}

struct VarAccessPerScope {
    id: NestedScopeId,
    next_field_id: usize,
    vars: Vec<NestedVarId>,
}

struct VarAccessPerFunction {
    id: usize,
    start_scope_id: usize,
    start_var_id: usize,
}

pub struct VarManager {
    // Stack of variables of all nested functions.
    vars: Vec<VarDefinition>,

    // Stack of all nested scopes. Mostly functions but also
    // loop bodies have scopes.
    scopes: Vec<VarAccessPerScope>,

    // Start of functions.
    functions: Vec<VarAccessPerFunction>,
}

impl VarManager {
    pub fn new() -> VarManager {
        VarManager {
            vars: Vec::new(),
            scopes: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn has_vars(&self) -> bool {
        self.vars.len() > self.current_function().start_var_id
    }

    pub fn has_context_vars(&self) -> bool {
        self.current_scope().next_field_id > 0
    }

    fn current_scope(&self) -> &VarAccessPerScope {
        self.scopes.last().expect("no scope entered")
    }

    fn current_scope_mut(&mut self) -> &mut VarAccessPerScope {
        self.scopes.last_mut().expect("no scope entered")
    }

    fn current_function(&self) -> &VarAccessPerFunction {
        self.functions.last().expect("missing function")
    }

    fn scope_for_var(&mut self, var_id: NestedVarId) -> &mut VarAccessPerScope {
        let NestedScopeId(idx) = self.get_var(var_id).scope_id;
        &mut self.scopes[idx]
    }

    pub(super) fn local_var_id(&self, var_id: NestedVarId) -> VarId {
        assert!(var_id.0 >= self.current_function().start_var_id);
        VarId(var_id.0 - self.current_function().start_var_id)
    }

    pub(super) fn nested_var_id(&self, var_id: VarId) -> NestedVarId {
        NestedVarId(var_id.0 + self.current_function().start_var_id)
    }

    pub(super) fn is_context_var(&self, var_id: NestedVarId) -> bool {
        var_id.0 < self.current_function().start_var_id
    }

    fn ensure_context_allocated(&mut self, var_id: NestedVarId) -> ContextFieldId {
        match self.get_var(var_id).location {
            VarLocation::Context(_scope_id, field_id) => return field_id,
            VarLocation::Stack => {}
        }

        // Allocate slot in context class.
        let scope = self.scope_for_var(var_id);
        let field_idx = ContextFieldId(scope.next_field_id);
        scope.next_field_id += 1;
        let NestedScopeId(nested_id) = scope.id;
        let function_id = self.get_var(var_id).function_id;
        let function_scope_id = self.functions[function_id].start_scope_id;
        let scope_id = ScopeId(nested_id - function_scope_id);
        self.vars[var_id.0].location = VarLocation::Context(scope_id, field_idx);

        field_idx
    }

    pub(super) fn add_var(
        &mut self,
        name: Name,
        ty: SourceType,
        mutable: bool,
        span: Option<Span>,
    ) -> NestedVarId {
        let id = NestedVarId(self.vars.len());

        let var = VarDefinition {
            id,
            name,
            ty,
            mutable,
            location: VarLocation::Stack,
            scope_id: self.current_scope().id,
            function_id: self.current_function().id,
            span,
            used: false,
        };

        self.vars.push(var);
        self.current_scope_mut().vars.push(id);

        id
    }

    pub(super) fn get_var(&self, idx: NestedVarId) -> &VarDefinition {
        &self.vars[idx.0]
    }

    pub(super) fn mark_used(&mut self, idx: NestedVarId) {
        self.vars[idx.0].used = true;
    }

    fn enter_function_scope(&mut self) {
        let scope_id = self.scopes.len();

        self.scopes.push(VarAccessPerScope {
            id: NestedScopeId(scope_id),
            next_field_id: 0,
            vars: Vec::new(),
        });
        self.functions.push(VarAccessPerFunction {
            id: self.functions.len(),
            start_scope_id: scope_id,
            start_var_id: self.vars.len(),
        });
    }

    fn enter_block_scope(&mut self) {
        self.scopes.push(VarAccessPerScope {
            id: NestedScopeId(self.scopes.len()),
            next_field_id: 0,
            vars: Vec::new(),
        });
    }

    fn leave_function_scope(&mut self) -> Vec<VarDefinition> {
        let _ = self.scopes.pop().expect("missing scope");
        let function = self.functions.pop().expect("missing function");

        self.vars.drain(function.start_var_id..).collect()
    }

    fn leave_block_scope(&mut self) {
        self.scopes.pop().expect("missing scope");
    }
}

#[derive(Clone, Debug)]
pub struct VarDefinition {
    pub id: NestedVarId,
    pub name: Name,
    pub ty: SourceType,
    pub mutable: bool,
    pub location: VarLocation,
    pub scope_id: NestedScopeId,
    pub function_id: usize,
    pub span: Option<Span>,
    pub used: bool,
}

fn create_context_classes(sa: &mut Sema, contexts: &mut [ContextData]) {
    for context in contexts.iter_mut() {
        let Some(class_definition) = context.class_definition() else {
            continue;
        };

        let class_id = sa.classes.alloc(class_definition);
        sa.classes[class_id].id = Some(class_id);
        context.set_class_id(class_id);
    }

    for context_idx in 0..contexts.len() {
        if !contexts[context_idx].has_class_id() {
            continue;
        }

        let context_id = ContextId(context_idx);
        let mut fields = contexts[context_idx].fields();

        if contexts[context_idx].has_parent_slot() {
            let parent_id = enclosing_context_class(contexts, context_id);
            let parent_class_id = contexts[parent_id.0].class_id();
            let parent_type_param_count = sa
                .class(parent_class_id)
                .type_param_definition()
                .type_param_count();
            let context_class_id = contexts[context_idx].class_id();
            let context_class = sa.class(context_class_id);
            let context_type_param_count = context_class.type_param_definition().type_param_count();
            assert_eq!(parent_type_param_count, context_type_param_count);

            let parent_field = FieldDefinition {
                id: None,
                name: Some(sa.interner.intern("parent_context")),
                span: None,
                index: FieldIndex(0),
                parsed_ty: ParsedType::new_ty(SourceType::Class(
                    parent_class_id,
                    new_identity_type_params(0, parent_type_param_count),
                )),
                mutable: true,
                visibility: Visibility::Module,
                file_id: context_class.file_id,
                module_id: context_class.module_id,
                package_id: context_class.package_id,
            };
            fields.insert(0, parent_field);
        }

        let field_ids = fields
            .into_iter()
            .map(|field| {
                let field_id = sa.fields.alloc(field);
                sa.fields[field_id].id = Some(field_id);
                field_id
            })
            .collect::<Vec<_>>();
        assert!(
            sa.class(contexts[context_idx].class_id())
                .field_ids
                .set(field_ids)
                .is_ok()
        );
    }
}

fn enclosing_context_class(contexts: &[ContextData], context_id: ContextId) -> ContextId {
    let mut parent_id = contexts[context_id.0]
        .parent()
        .expect("missing parent context");

    while !contexts[parent_id.0].has_class_id() {
        parent_id = contexts[parent_id.0]
            .parent()
            .expect("missing parent context class");
    }

    parent_id
}

fn create_lambda_functions(sa: &mut Sema, lambda_definitions: Vec<FctDefinition>) {
    assert!(sa.lambda_fct_ids.is_empty());
    sa.lambda_fct_ids.reserve(lambda_definitions.len());

    for lambda_definition in lambda_definitions {
        let fct_id = sa.fcts.alloc(lambda_definition);
        sa.fcts[fct_id].id = Some(fct_id);
        sa.lambda_fct_ids.push(fct_id);
    }
}

pub fn call_arg_span(ck: &TypeCheck, call_expr_id: ExprId, index: usize) -> Span {
    match ck.expr(call_expr_id) {
        Expr::Call(_) => {
            let node = ck.syntax::<ast::AstCallExpr>(call_expr_id);
            node.arg_list()
                .items()
                .nth(index)
                .map(|arg| arg.span())
                .unwrap_or_else(|| ck.expr_span(call_expr_id))
        }
        Expr::MethodCall(_) => {
            let node = ck.syntax::<ast::AstMethodCallExpr>(call_expr_id);
            node.arg_list()
                .items()
                .nth(index)
                .map(|arg| arg.span())
                .unwrap_or_else(|| ck.expr_span(call_expr_id))
        }
        _ => ck.expr_span(call_expr_id),
    }
}

pub fn call_arg_name_span(ck: &TypeCheck, call_expr_id: ExprId, index: usize) -> Option<Span> {
    match ck.expr(call_expr_id) {
        Expr::Call(_) => {
            let node = ck.syntax::<ast::AstCallExpr>(call_expr_id);
            node.arg_list()
                .items()
                .nth(index)
                .and_then(|arg| arg.name())
                .map(|name| name.span())
        }
        Expr::MethodCall(_) => {
            let node = ck.syntax::<ast::AstMethodCallExpr>(call_expr_id);
            node.arg_list()
                .items()
                .nth(index)
                .and_then(|arg| arg.name())
                .map(|name| name.span())
        }
        _ => None,
    }
}
