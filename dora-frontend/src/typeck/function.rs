use std::cell::OnceCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::str::Chars;
use std::{f32, f64};

use crate::error::msg::ErrorMessage;
use crate::sema::{
    AnalysisData, ClassDefinition, ConstValue, ContextFieldId, Element, FctDefinition, FctParent,
    Field, FieldId, GlobalDefinition, IdentType, LazyContextClassCreationData, LazyContextData,
    LazyLambdaCreationData, ModuleDefinitionId, NestedScopeId, NestedVarId, OuterContextIdx,
    PackageDefinitionId, Param, ScopeId, Sema, SourceFileId, TypeParamDefinition, Var, VarAccess,
    VarId, VarLocation, Visibility,
};
use crate::typeck::{check_expr, check_pattern, check_stmt, CallArguments};
use crate::{
    always_returns, expr_always_returns, replace_type, report_sym_shadow_span, ModuleSymTable,
    SourceType, SourceTypeArray, SymbolKind,
};
use crate::{parsety, ParsedType};

use crate::interner::Name;
use dora_parser::Span;
use dora_parser::{ast, NodeId};

pub struct TypeCheck<'a> {
    pub sa: &'a Sema,
    pub type_param_definition: &'a Rc<TypeParamDefinition>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub analysis: &'a mut AnalysisData,
    pub symtable: &'a mut ModuleSymTable,
    pub param_types: Vec<Param>,
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
                let global_ty = self_.ty_name(&global.ty());
                let expr_ty = self_.ty_name(&expr_ty);
                let msg = ErrorMessage::AssignType(global_ty, expr_ty);
                self_.sa.report(self_.file_id, global.span, msg);
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

        assert!(self
            .analysis
            .needs_context_slot_in_lambda_object
            .set(needs_context_slot_in_lambda_object)
            .is_ok());

        assert!(self
            .analysis
            .function_context_data
            .set(lazy_context_data)
            .is_ok());

        // Store var definitions for all local and context vars defined in this function.
        let vars = self.vars.leave_function_scope();

        let vars = vars
            .into_iter()
            .map(|vd| Var {
                ty: vd.ty.clone(),
                location: vd.location,
            })
            .collect();

        self.analysis.vars = VarAccess::new(vars);
    }

    pub fn leave_block_scope(&mut self, id: NodeId) -> LazyContextData {
        let lazy_context_data = self.context_classes.pop().expect("missing context class");

        if self.vars.has_context_vars() {
            self.setup_context_class(lazy_context_data.clone());
        }

        self.analysis
            .map_block_contexts
            .insert(id, lazy_context_data.clone());

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
            let field = Field {
                id: FieldId(0),
                name: Some(name),
                parsed_ty: ParsedType::new_ty(SourceType::Ptr),
                mutable: true,
                visibility: Visibility::Module,
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

            let id = FieldId(fields.len());
            let field = Field {
                id,
                name: Some(var.name),
                parsed_ty: ParsedType::new_ty(var.ty.clone()),
                mutable: true,
                visibility: Visibility::Module,
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
            fields,
        );

        self.lazy_context_class_creation
            .push(LazyContextClassCreationData {
                context: lazy_context_data.clone(),
                class_definition: class,
            });
    }

    fn add_type_params(&mut self) {
        for (id, name) in self.type_param_definition.names() {
            self.symtable.insert(name, SymbolKind::TypeParam(id));
        }
    }

    fn add_params(&mut self, ast: &ast::Function) {
        self.add_hidden_parameter_self();

        let self_count = if self.has_hidden_self_argument { 1 } else { 0 };
        assert_eq!(ast.params.len() + self_count, self.param_types.len());

        let param_types = self
            .param_types
            .iter()
            .skip(self_count)
            .map(|p| p.ty())
            .collect::<Vec<_>>();

        let mut bound_params = HashSet::new();

        for (ind, (ast_param, param_ty)) in
            ast.params.iter().zip(param_types.into_iter()).enumerate()
        {
            // is this last argument of function with variadic arguments?
            let ty = if ind == ast.params.len() - 1
                && ast.params.last().expect("missing param").variadic
            {
                // type of variable is Array[T]
                self.sa.known.array_ty(param_ty)
            } else {
                param_ty
            };

            self.analysis.set_ty(ast_param.id, ty.clone());

            let local_bound_params = check_pattern(self, &ast_param.pattern, ty);

            for (name, data) in local_bound_params {
                if !bound_params.insert(name) {
                    let name = self.sa.interner.str(name).to_string();
                    self.sa.report(
                        self.file_id,
                        data.span,
                        ErrorMessage::NameBoundMultipleTimesInParams(name),
                    );
                }
            }
        }
    }

    fn add_hidden_parameter_self(&mut self) {
        self.analysis.set_has_self(self.has_hidden_self_argument);

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

    pub(super) fn read_type(&mut self, t: &ast::Type) -> SourceType {
        let parsed_ty = ParsedType::new_ast(t.clone());
        parsety::parse_type(
            self.sa,
            &self.symtable,
            self.file_id,
            self.element,
            self.self_ty.is_some(),
            &parsed_ty,
        );

        parsety::check_type(self.sa, self.element, &parsed_ty);
        let expanded_ty =
            parsety::expand_type(self.sa, self.element, &parsed_ty, self.self_ty.clone());

        replace_type(self.sa, expanded_ty, None, self.self_ty.clone())
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
        ty.name_with_type_params(self.sa, self.type_param_definition)
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

pub(super) fn check_args_compatible_fct<S>(
    ck: &TypeCheck,
    callee: &FctDefinition,
    args: CallArguments,
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
    extra_specialization: S,
) where
    S: FnMut(SourceType) -> SourceType,
{
    check_args_compatible(
        ck,
        callee.params.regular_params(),
        callee.params.variadic_param(),
        &args,
        type_params,
        self_ty,
        extra_specialization,
    );
}

pub(super) fn check_args_compatible<S>(
    ck: &TypeCheck,
    regular_params: &[Param],
    variadic_param: Option<&Param>,
    args: &CallArguments,
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
    mut extra_specialization: S,
) where
    S: FnMut(SourceType) -> SourceType,
{
    for arg in &args.arguments {
        if let Some(ref name) = arg.name {
            ck.sa
                .report(ck.file_id, name.span, ErrorMessage::UnexpectedNamedArgument);
        }
    }

    for (param, arg) in regular_params.iter().zip(&args.arguments) {
        let param_ty = extra_specialization(param.ty().clone());
        let param_ty = replace_type(ck.sa, param_ty, Some(&type_params), self_ty.clone());
        let arg_ty = ck.analysis.ty(arg.id);

        if !arg_allows(ck.sa, param_ty.clone(), arg_ty.clone(), self_ty.clone())
            && !arg_ty.is_error()
        {
            let exp = ck.ty_name(&param_ty);
            let got = ck.ty_name(&arg_ty);

            ck.sa.report(
                ck.file_id,
                arg.expr.span(),
                ErrorMessage::WrongTypeForArgument(exp, got),
            );
        }
    }

    let no_regular_params = regular_params.len();

    if args.arguments.len() < no_regular_params {
        ck.sa.report(
            ck.file_id,
            args.span,
            ErrorMessage::MissingArguments(no_regular_params, args.arguments.len()),
        );
    } else {
        if let Some(variadic_param) = variadic_param {
            let variadic_ty = replace_type(
                ck.sa,
                variadic_param.ty(),
                Some(&type_params),
                self_ty.clone(),
            );

            for arg in &args.arguments[no_regular_params..] {
                let arg_ty = ck.analysis.ty(arg.id);

                if !arg_allows(ck.sa, variadic_ty.clone(), arg_ty.clone(), self_ty.clone())
                    && !arg_ty.is_error()
                {
                    let exp = ck.ty_name(&variadic_ty);
                    let got = ck.ty_name(&arg_ty);

                    ck.sa.report(
                        ck.file_id,
                        arg.expr.span(),
                        ErrorMessage::WrongTypeForArgument(exp, got),
                    );
                }
            }
        } else {
            for arg in &args.arguments[no_regular_params..] {
                ck.sa
                    .report(ck.file_id, arg.span, ErrorMessage::SuperfluousArgument);
            }
        }
    }
}

pub(super) fn check_args_compatible_fct2<S>(
    ck: &TypeCheck,
    callee: &FctDefinition,
    args: CallArguments,
    extra_specialization: S,
) where
    S: FnMut(SourceType) -> SourceType,
{
    check_args_compatible2(
        ck,
        callee.params.regular_params(),
        callee.params.variadic_param(),
        &args,
        extra_specialization,
    );
}

pub(super) fn check_args_compatible2<S>(
    ck: &TypeCheck,
    regular_params: &[Param],
    variadic_param: Option<&Param>,
    args: &CallArguments,
    mut extra_specialization: S,
) where
    S: FnMut(SourceType) -> SourceType,
{
    for arg in &args.arguments {
        if let Some(ref name) = arg.name {
            ck.sa
                .report(ck.file_id, name.span, ErrorMessage::UnexpectedNamedArgument);
        }
    }

    for (param, arg) in regular_params.iter().zip(&args.arguments) {
        let param_ty = extra_specialization(param.ty().clone());
        let arg_ty = ck.analysis.ty(arg.id);

        if !arg_allows(ck.sa, param_ty.clone(), arg_ty.clone(), None) && !arg_ty.is_error() {
            let exp = ck.ty_name(&param_ty);
            let got = ck.ty_name(&arg_ty);

            ck.sa.report(
                ck.file_id,
                arg.expr.span(),
                ErrorMessage::WrongTypeForArgument(exp, got),
            );
        }
    }

    let no_regular_params = regular_params.len();

    if args.arguments.len() < no_regular_params {
        ck.sa.report(
            ck.file_id,
            args.span,
            ErrorMessage::MissingArguments(no_regular_params, args.arguments.len()),
        );
    } else {
        if let Some(variadic_param) = variadic_param {
            let variadic_ty = extra_specialization(variadic_param.ty());

            for arg in &args.arguments[no_regular_params..] {
                let arg_ty = ck.analysis.ty(arg.id);

                if !arg_allows(ck.sa, variadic_ty.clone(), arg_ty.clone(), None)
                    && !arg_ty.is_error()
                {
                    let exp = ck.ty_name(&variadic_ty);
                    let got = ck.ty_name(&arg_ty);

                    ck.sa.report(
                        ck.file_id,
                        arg.expr.span(),
                        ErrorMessage::WrongTypeForArgument(exp, got),
                    );
                }
            }
        } else {
            for arg in &args.arguments[no_regular_params..] {
                ck.sa
                    .report(ck.file_id, arg.span, ErrorMessage::SuperfluousArgument);
            }
        }
    }
}

pub(super) fn arg_allows(
    sa: &Sema,
    def: SourceType,
    arg: SourceType,
    self_ty: Option<SourceType>,
) -> bool {
    if arg.is_error() {
        return true;
    }

    match def {
        SourceType::Error => true,
        SourceType::Any => unreachable!(),
        SourceType::Unit
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Struct(..)
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Enum(..)
        | SourceType::TraitObject(..) => def == arg,
        SourceType::Ptr => panic!("ptr should not occur in fct definition."),
        SourceType::This => {
            if let Some(real) = self_ty.clone() {
                arg_allows(sa, real, arg, self_ty)
            } else {
                def == arg
            }
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

        SourceType::Alias(id, type_params) => {
            assert!(type_params.is_empty());
            let alias = sa.alias(id);
            arg_allows(sa, alias.ty(), arg, self_ty.clone())
        }

        SourceType::Assoc { .. } | SourceType::GenericAssoc { .. } => def == arg,
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
) -> (SourceType, ConstValue) {
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

        return (ty, ConstValue::Float(value));
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
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
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
            sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        (ty, ConstValue::Int(value as i64))
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
