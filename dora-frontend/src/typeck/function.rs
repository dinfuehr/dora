use std::cell::OnceCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::str::Chars;
use std::{f32, f64};

use crate::ParsedType;
use crate::args;
use crate::error::DescriptorArgs;
use crate::error::diagnostics::{
    ASSIGN_TYPE, DiagnosticDescriptor, INVALID_CHAR_LITERAL, INVALID_ESCAPE_SEQUENCE,
    INVALID_NUMBER_FORMAT, NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS, NEGATIVE_UNSIGNED,
    NUMBER_LIMIT_OVERFLOW, NUMBER_OVERFLOW, RETURN_TYPE, UNKNOWN_SUFFIX,
};
use crate::sema::{
    Body, CallArg, ClassDefinition, ConstValue, ContextFieldId, Element, Expr, ExprId, ExprMapId,
    FctParent, FieldDefinition, FieldIndex, GlobalDefinition, IdentType, LambdaExpr,
    LazyContextClassCreationData, LazyContextData, LazyLambdaCreationData, ModuleDefinitionId,
    NestedScopeId, NestedVarId, OuterContextIdx, PackageDefinitionId, Param, PatternId, ScopeId,
    Sema, SourceFileId, StmtId, TypeParamDefinition, TypeRefId, Var, VarAccess, VarId, VarLocation,
    Visibility, check_type_ref, convert_type_ref, parse_type_ref,
};
use crate::typeck::{check_expr, check_pattern, check_stmt};
use crate::{
    ModuleSymTable, SourceType, SymbolKind, always_returns, expr_always_returns,
    report_sym_shadow_span,
};

use crate::interner::Name;
use dora_parser::Span;
use dora_parser::ast::{SyntaxNode, SyntaxNodeBase};

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
    pub self_ty: Option<SourceType>,
    pub vars: &'a mut VarManager,
    pub element: &'a dyn Element,
    // All nested contexts. There will be entries for all nested function/lambda
    // and block scopes even when we eventually learn that we don't need
    // a context class for some of them.
    pub context_classes: &'a mut Vec<LazyContextData>,
    pub start_context_id: usize,
    pub needs_context_slot_in_lambda_object: bool,
    // Lazily create contexts and lambdas discovered while checking functions.
    pub lazy_context_class_creation: &'a mut Vec<LazyContextClassCreationData>,
    pub lazy_lambda_creation: &'a mut Vec<LazyLambdaCreationData>,
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

    pub fn report_id(&self, id: ExprId, desc: &'static DiagnosticDescriptor, args: DescriptorArgs) {
        let ptr = self.body.exprs().syntax_node_ptr(id);
        let node = self.sa.syntax::<SyntaxNode>(self.file_id, ptr);
        self.sa.report(self.file_id, node.span(), desc, args);
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
        self.enter_function_scope();
        self.symtable.push_level();

        fct(self);

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.leave_function_scope();
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
        let ident_type = self.vars.maybe_allocate_in_context(var_id);

        match ident_type {
            IdentType::Context(context_id, _field_id) => {
                // We need parent slots from the context of the variable up to (not including)
                // the first context of this function.
                // There is no need for parent slots for contexts within this function because
                // we can always load that context out of the lambda object which is passed as
                // the first argument.
                let indices = context_id.0 + 1..self.start_context_id;
                let range = &self.context_classes[indices];
                for context_class in range {
                    context_class.require_parent_slot();
                }
                // This lambda needs the caller context.
                assert!(self.is_lambda);
                self.needs_context_slot_in_lambda_object = true;
                ident_type
            }

            IdentType::Var(..) => ident_type,

            _ => unreachable!(),
        }
    }

    fn enter_function_scope(&mut self) {
        self.start_context_id = self.context_classes.len();
        self.context_classes.push(LazyContextData::new());
        self.vars.enter_function_scope();
    }

    pub fn enter_block_scope(&mut self) {
        self.context_classes.push(LazyContextData::new());
        self.vars.enter_block_scope();
    }

    fn leave_function_scope(&mut self) {
        let lazy_context_data = self.context_classes.pop().expect("missing context class");

        if self.vars.has_context_vars() {
            self.setup_context_class(lazy_context_data.clone());
        }

        let needs_context_slot_in_lambda_object =
            self.needs_context_slot_in_lambda_object || lazy_context_data.has_parent_slot();

        if needs_context_slot_in_lambda_object {
            assert!(self.is_lambda);
        }

        self.body
            .set_needs_context_slot_in_lambda_object(needs_context_slot_in_lambda_object);
        self.body.set_function_context_data(lazy_context_data);

        // Store var definitions for all local and context vars defined in this function.
        let vars = self.vars.leave_function_scope();

        let vars = vars
            .into_iter()
            .map(|vd| Var {
                ty: vd.ty.clone(),
                location: vd.location,
            })
            .collect();

        self.body.set_vars(VarAccess::new(vars));
    }

    pub fn leave_block_scope<T: ExprMapId>(&mut self, id: T) -> LazyContextData {
        let lazy_context_data = self.context_classes.pop().expect("missing context class");

        if self.vars.has_context_vars() {
            self.setup_context_class(lazy_context_data.clone());
        }

        self.body
            .insert_block_context(id, lazy_context_data.clone());

        self.vars.leave_block_scope();

        lazy_context_data
    }

    fn setup_context_class(&mut self, lazy_context_data: LazyContextData) {
        let scope = self.vars.current_scope();
        let number_fields = scope.next_field_id;
        let mut fields = Vec::with_capacity(number_fields);
        let map: Vec<OnceCell<NestedVarId>> = vec![OnceCell::new(); number_fields];

        if lazy_context_data.has_parent_slot() {
            let name = self.sa.interner.intern("parent_context");
            let field = FieldDefinition {
                id: None,
                name: Some(name),
                span: None,
                index: FieldIndex(fields.len()),
                parsed_ty: ParsedType::new_ty(SourceType::Ptr),
                mutable: true,
                visibility: Visibility::Module,
                file_id: Some(self.file_id),
                module_id: self.module_id,
                package_id: self.package_id,
            };

            fields.push(field);
        }

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
                index: FieldIndex(fields.len()),
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

        self.lazy_context_class_creation
            .push(LazyContextClassCreationData {
                context: lazy_context_data.clone(),
                class_definition: class,
                fields,
            });
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
            self.self_ty.clone().expect("self expected")
        };

        assert!(!self.vars.has_vars());
        let name = self.sa.interner.intern("self");
        self.vars.add_var(name, hidden_self_ty, false);
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
) -> (SourceType, ConstValue) {
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

        return (ty, ConstValue::Float(value));
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
            return (ty, ConstValue::Int(0));
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

        (ty, ConstValue::Int(value))
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

        (ty, ConstValue::Int(value as i64))
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

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        if var_id.0 < self.current_function().start_var_id {
            let field_id = self.ensure_context_allocated(var_id);
            let NestedScopeId(level) = self.scope_for_var(var_id).id;
            IdentType::Context(OuterContextIdx(level), field_id)
        } else {
            IdentType::Var(self.local_var_id(var_id))
        }
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

    pub(super) fn add_var(&mut self, name: Name, ty: SourceType, mutable: bool) -> NestedVarId {
        let id = NestedVarId(self.vars.len());

        let var = VarDefinition {
            id,
            name,
            ty,
            mutable,
            location: VarLocation::Stack,
            scope_id: self.current_scope().id,
            function_id: self.current_function().id,
        };

        self.vars.push(var);
        self.current_scope_mut().vars.push(id);

        id
    }

    pub(super) fn get_var(&self, idx: NestedVarId) -> &VarDefinition {
        &self.vars[idx.0]
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
}
