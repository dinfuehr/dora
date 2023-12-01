use std::cell::OnceCell;
use std::str::Chars;
use std::{f32, f64};

use crate::error::msg::ErrorMessage;
use crate::sema::{
    AnalysisData, ClassDefinition, ContextIdx, FctDefinition, Field, FieldId, GlobalDefinition,
    IdentType, LazyContextData, LazyLambdaId, ModuleDefinitionId, NestedVarId, OuterContextIdx,
    PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Var, VarAccess, VarId,
    VarLocation, Visibility,
};
use crate::typeck::{check_expr, check_stmt};
use crate::{
    always_returns, check_type, expr_always_returns, replace_type, report_sym_shadow_span,
    AliasReplacement, AllowSelf, ModuleSymTable, SourceType, SourceTypeArray, SymbolKind,
};

use crate::interner::Name;
use dora_parser::ast;
use dora_parser::Span;

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
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
    pub self_ty: Option<SourceType>,
    pub vars: &'a mut VarManager,
    pub lazy_context_class_creation: &'a mut Vec<(LazyContextData, ClassDefinition)>,
    pub lazy_lambda_creation: &'a mut Vec<(LazyLambdaId, FctDefinition)>,
    pub outer_context_classes: &'a mut Vec<LazyContextData>,
    pub needs_parent_context: bool,
}

impl<'a> TypeCheck<'a> {
    pub fn check_fct(&mut self, ast: &ast::Function) {
        self.check_common(|self_| {
            self_.add_type_params();
            self_.add_params(ast);
            self_.check_body(ast);
        })
    }

    pub fn check_initializer(&mut self, global: &GlobalDefinition, expr: &ast::Expr) {
        // Global initializer never has self.
        self.analysis.set_has_self(false);

        self.check_common(|self_| {
            let expr_ty = check_expr(self_, expr, global.ty());

            if !global.ty().is_error()
                && !expr_ty.is_error()
                && !global.ty().allows(self_.sa, expr_ty.clone())
            {
                let name = self_.sa.interner.str(global.name).to_string();
                let global_ty = self_.ty_name(&global.ty());
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
        self.outer_context_classes.push(LazyContextData::new());
        let start_level = self.symtable.levels();
        self.enter_function_scope();
        self.symtable.push_level();

        fct(self);

        self.symtable.pop_level();
        assert_eq!(self.symtable.levels(), start_level);

        self.leave_function_scope();
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
                check_stmt(self, stmt);

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

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        let ident_type = self.vars.maybe_allocate_in_context(var_id);

        match ident_type {
            IdentType::Context(..) => {
                self.needs_parent_context = true;
                ident_type
            }

            IdentType::Var(..) => ident_type,

            _ => unreachable!(),
        }
    }

    fn enter_function_scope(&mut self) {
        self.vars.enter_function_scope();
    }

    fn leave_function_scope(&mut self) {
        if self.vars.has_context_vars() {
            self.setup_context_class();
        }

        let lazy_context_data = self
            .outer_context_classes
            .last()
            .cloned()
            .expect("missing outer context");

        assert!(self
            .analysis
            .function_context_data
            .set(lazy_context_data)
            .is_ok());

        // Store var definitions for all local and context vars defined in this function.
        self.analysis.vars = self.vars.leave_function_scope();

        assert!(self
            .analysis
            .needs_parent_context
            .set(self.needs_parent_context)
            .is_ok());
    }

    fn setup_context_class(&mut self) {
        let function = self.vars.current_scope();
        let start_index = function.start_idx;
        let number_fields = function.next_context_id;
        let mut fields = Vec::with_capacity(number_fields);
        let mut map: Vec<Option<NestedVarId>> = vec![None; number_fields];

        // As soon as a lambda needs a context object, this also means that some
        // lambda in it accessed parent scope variables. This also means we need
        // a slot for the parent context as well.
        if self.is_lambda {
            let name = self.sa.interner.intern("parent_context");
            let field = Field {
                id: FieldId(0),
                name,
                ty: OnceCell::new(),
                mutable: true,
                visibility: Visibility::Module,
            };
            assert!(field.ty.set(SourceType::Ptr).is_ok());

            fields.push(field);
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
            let field = Field {
                id,
                name: var.name,
                ty: OnceCell::new(),
                mutable: true,
                visibility: Visibility::Module,
            };
            assert!(field.ty.set(var.ty.clone()).is_ok());

            fields.push(field);
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

        let lazy_context_data = self
            .outer_context_classes
            .last()
            .cloned()
            .expect("missing outer context");

        lazy_context_data.set_has_parent_context_slot(self.is_lambda);

        self.lazy_context_class_creation
            .push((lazy_context_data.clone(), class));
    }

    fn add_type_params(&mut self) {
        for (id, name) in self.type_param_defs.names() {
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

        assert!(!self.vars.has_vars());
        self.vars.add_var(name, self_ty, false);
    }

    pub(super) fn read_type(&mut self, t: &ast::TypeData) -> SourceType {
        let allow_self = if self.self_ty.is_some() {
            AllowSelf::Yes
        } else {
            AllowSelf::No
        };

        let ty = check_type(
            self.sa,
            &self.symtable,
            self.file_id,
            t,
            self.type_param_defs,
            allow_self,
        );

        replace_type(
            self.sa,
            ty,
            None,
            self.self_ty.clone(),
            AliasReplacement::ReplaceWithActualType,
        )
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

    pub(super) fn ty_name(&self, ty: &SourceType) -> String {
        ty.name_with_type_params(self.sa, self.type_param_defs)
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

pub(super) fn args_compatible_fct(
    sa: &Sema,
    callee: &FctDefinition,
    args: &[SourceType],
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> bool {
    let arg_types = callee.params_without_self();
    let variadic_arguments = callee.is_variadic.get();
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
        let def_arg = replace_type(
            sa,
            def_arg.clone(),
            Some(&type_params),
            self_ty.clone(),
            AliasReplacement::None,
        );

        if !arg_allows(sa, def_arg, args[ind].clone(), self_ty.clone()) {
            return false;
        }
    }

    if let Some(rest_ty) = rest_ty {
        let ind = def.len();
        let rest_ty = replace_type(
            sa,
            rest_ty,
            Some(&type_params),
            self_ty.clone(),
            AliasReplacement::None,
        );

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
        SourceType::Error => true,
        SourceType::Any => unreachable!(),
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

        SourceType::Lambda(_, _) => def == arg,

        SourceType::TypeAlias(id) => arg_allows(sa, sa.alias(id).ty(), arg, self_ty.clone()),
    }
}

pub fn check_lit_str(sa: &Sema, file_id: SourceFileId, e: &ast::ExprLitStrType) -> String {
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
    level: usize,
    start_idx: usize,
    next_context_id: usize,
}

pub struct VarManager {
    // Stack of variables of all nested functions.
    vars: Vec<VarDefinition>,

    // Stack of all nested scopes. Mostly functions but also
    // loop bodies have scopes.
    scopes: Vec<VarAccessPerScope>,

    // Start of functions.
    functions: Vec<usize>,
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
        self.vars.len() > self.current_scope().start_idx
    }

    pub fn has_context_vars(&self) -> bool {
        self.current_scope().next_context_id > 0
    }

    fn current_scope(&self) -> &VarAccessPerScope {
        self.scopes.last().expect("no scope entered")
    }

    fn current_function(&self) -> usize {
        self.functions.last().cloned().expect("missing function")
    }

    fn scope_for_var(&mut self, var_id: NestedVarId) -> &mut VarAccessPerScope {
        for function in self.scopes.iter_mut().rev() {
            if var_id.0 >= function.start_idx {
                return function;
            }
        }

        panic!("function not found")
    }

    pub(super) fn local_var_id(&self, var_id: NestedVarId) -> VarId {
        assert!(var_id.0 >= self.current_scope().start_idx);
        VarId(var_id.0 - self.current_scope().start_idx)
    }

    pub(super) fn maybe_allocate_in_context(&mut self, var_id: NestedVarId) -> IdentType {
        if var_id.0 < self.current_function() {
            let field_id = self.ensure_context_allocated(var_id);
            let level = self.scope_for_var(var_id).level;
            IdentType::Context(OuterContextIdx(level), field_id)
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
        let scope = self.scope_for_var(var_id);
        let context_idx = ContextIdx(scope.next_context_id);
        scope.next_context_id += 1;
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

    fn enter_function_scope(&mut self) {
        self.scopes.push(VarAccessPerScope {
            level: self.scopes.len(),
            start_idx: self.vars.len(),
            next_context_id: 0,
        });
        self.functions.push(self.vars.len());
    }

    fn leave_function_scope(&mut self) -> VarAccess {
        let scope = self.scopes.pop().expect("missing scope");
        let function_start = self.functions.pop().expect("missing function");
        assert_eq!(scope.start_idx, function_start);

        let vars = self
            .vars
            .drain(scope.start_idx..)
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
