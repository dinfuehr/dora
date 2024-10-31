use std::collections::HashMap;
use std::sync::Arc;

use dora_parser::{ast, Span};

use crate::access::{
    class_accessible_from, class_field_accessible_from, fct_accessible_from, is_default_accessible,
    method_accessible_from, struct_accessible_from, struct_field_accessible_from,
};
use crate::interner::Name;
use crate::sema::{
    find_field_in_class, new_identity_type_params, CallType, ClassDefinitionId, ElementWithFields,
    EnumDefinitionId, EnumVariant, FctDefinitionId, IdentType, Sema, StructDefinitionId,
    TraitDefinition, TypeParamDefinition, TypeParamId,
};
use crate::specialize::replace_type;
use crate::sym::SymbolKind;
use crate::typeck::{
    args_compatible, args_compatible_fct, check_enum_value_with_args, check_expr,
    find_method_call_candidates, read_path_expr, CallArguments, MethodLookup, TypeCheck,
};
use crate::typeparamck::{self, ErrorReporting};
use crate::{
    empty_sta, specialize_type, ty::error as ty_error, ErrorMessage, SourceType, SourceTypeArray,
    TraitType,
};

pub(super) fn check_expr_call(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expected_ty: SourceType,
) -> SourceType {
    let (callee, type_params) = if let Some(expr_type_params) = e.callee.to_type_param() {
        let type_params: Vec<SourceType> = expr_type_params
            .args
            .iter()
            .map(|p| ck.read_type(p))
            .collect();
        let type_params: SourceTypeArray = SourceTypeArray::with(type_params);
        (&expr_type_params.callee, type_params)
    } else {
        (&e.callee, SourceTypeArray::empty())
    };

    let arguments = create_call_arguments(ck, e);

    if let Some(expr_ident) = callee.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        check_expr_call_sym(ck, e, expected_ty, callee, sym, type_params, arguments)
    } else if let Some(expr_dot) = callee.to_dot() {
        let object_type = check_expr(ck, &expr_dot.lhs, SourceType::Any);

        let method_name = match expr_dot.rhs.to_ident() {
            Some(ident) => ident.name.clone(),

            None => {
                let msg = ErrorMessage::NameExpected;
                ck.sa.report(ck.file_id, e.span, msg);

                ck.analysis.set_ty(e.id, ty_error());
                return ty_error();
            }
        };
        let arg_types = arguments.assume_all_positional(ck);
        check_expr_call_method(ck, e, object_type, method_name, type_params, &arg_types)
    } else if let Some(_expr_path) = callee.to_path() {
        check_expr_call_path(ck, e, expected_ty, callee, type_params, arguments)
    } else {
        if !type_params.is_empty() {
            let msg = ErrorMessage::NoTypeParamsExpected;
            ck.sa.report(ck.file_id, e.callee.span(), msg);
        }

        let expr_type = check_expr(ck, callee, SourceType::Any);
        let arg_types = arguments.assume_all_positional(ck);
        check_expr_call_expr(ck, e, expr_type, &arg_types)
    }
}

fn create_call_arguments(ck: &mut TypeCheck, e: &ast::ExprCallType) -> CallArguments {
    let mut arguments = CallArguments {
        arguments: Vec::with_capacity(e.args.len()),
        span: e.span,
    };

    for arg in e.args.iter() {
        let ty = check_expr(ck, &arg.expr, SourceType::Any);
        ck.analysis.set_ty(arg.id, ty);

        arguments.arguments.push(arg.clone());
    }

    arguments
}

pub(super) fn check_expr_call_enum_args(
    sa: &Sema,
    _enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant: &EnumVariant,
    arg_types: &[SourceType],
) -> bool {
    if variant.parsed_types().len() != arg_types.len() {
        return false;
    }

    for (def_ty, arg_ty) in variant.parsed_types().iter().zip(arg_types) {
        let def_ty = replace_type(sa, def_ty.ty(), Some(&type_params), None);

        if !def_ty.allows(sa, arg_ty.clone()) {
            return false;
        }
    }

    true
}

fn check_expr_call_generic_static_method(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    tp_id: TypeParamId,
    name: String,
    args: &[SourceType],
) -> SourceType {
    let mut matched_methods = Vec::new();
    let interned_name = ck.sa.interner.intern(&name);

    for trait_ty in ck.type_param_definition.bounds_for_type_param(tp_id) {
        let trait_ = ck.sa.trait_(trait_ty.trait_id);

        if let Some(trait_method_id) = trait_.get_method(interned_name, true) {
            matched_methods.push((trait_method_id, trait_ty));
        }
    }

    if matched_methods.len() != 1 {
        let msg = if matched_methods.len() > 1 {
            ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam
        } else {
            ErrorMessage::UnknownStaticMethodWithTypeParam
        };

        ck.sa.report(ck.file_id, e.span, msg);

        ck.analysis.set_ty(e.id, ty_error());
        return ty_error();
    }

    if args.contains(&ty_error()) {
        ck.analysis.set_ty(e.id, ty_error());
        return ty_error();
    }

    let (trait_method_id, trait_ty) = matched_methods.pop().expect("missing method");
    let trait_method = ck.sa.fct(trait_method_id);

    let tp = SourceType::TypeParam(tp_id);

    if !args_compatible_fct(
        ck.sa,
        trait_method,
        args,
        &trait_ty.type_params,
        Some(tp.clone()),
    ) {
        let fct_params = trait_method
            .params_without_self()
            .iter()
            .map(|a| ck.ty_name(&a.ty()))
            .collect::<Vec<_>>();
        let arg_types = args.iter().map(|a| ck.ty_name(a)).collect::<Vec<_>>();
        let msg = ErrorMessage::ParamTypesIncompatible(name, fct_params, arg_types);
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let call_type = CallType::GenericStaticMethod(
        tp_id,
        trait_ty.trait_id,
        trait_method_id,
        trait_ty.type_params.clone(),
    );
    ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

    let return_type = replace_type(
        ck.sa,
        trait_method.return_type(),
        Some(&trait_ty.type_params),
        Some(tp),
    );

    ck.analysis.set_ty(e.id, return_type.clone());

    return_type
}

fn check_expr_call_expr(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expr_type: SourceType,
    arg_types: &[SourceType],
) -> SourceType {
    if expr_type.is_error() {
        ck.analysis.set_ty(e.id, ty_error());
        return ty_error();
    }

    if expr_type.is_lambda() {
        return check_expr_call_expr_lambda(ck, e, expr_type, arg_types);
    }

    let get = ck.sa.interner.intern("get");

    if let Some(descriptor) = find_method(
        ck,
        e.span,
        expr_type.clone(),
        false,
        get,
        arg_types,
        &SourceTypeArray::empty(),
    ) {
        let call_type =
            CallType::Expr(expr_type.clone(), descriptor.fct_id, descriptor.type_params);
        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));

        ck.analysis.set_ty(e.id, descriptor.return_type.clone());

        descriptor.return_type
    } else {
        ck.analysis.set_ty(e.id, ty_error());

        ty_error()
    }
}

fn check_expr_call_expr_lambda(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expr_type: SourceType,
    arg_types: &[SourceType],
) -> SourceType {
    let (params, return_type) = expr_type.to_lambda().expect("lambda expected");

    // Type params are mapped to themselves.
    let type_params_count = ck.type_param_definition.type_param_count();
    let type_params = new_identity_type_params(type_params_count);

    if !args_compatible(ck.sa, params.types(), false, arg_types, &type_params, None) {
        let fct_params = params.iter().map(|a| ck.ty_name(&a)).collect::<Vec<_>>();
        let arg_types = arg_types.iter().map(|a| ck.ty_name(a)).collect::<Vec<_>>();
        let msg = ErrorMessage::LambdaParamTypesIncompatible(fct_params, arg_types);
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let call_type = CallType::Lambda(params, return_type.clone());

    ck.analysis
        .map_calls
        .insert_or_replace(e.id, Arc::new(call_type));

    ck.analysis.set_ty(e.id, return_type.clone());
    return_type
}

fn check_expr_call_fct(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    if !fct_accessible_from(ck.sa, fct_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_definition)
        .span(e.span)
        .callee(fct_id)
        .args(&arg_types)
        .fct_type_params(&type_params)
        .find();

    let ty = if lookup.find() {
        let call_type = CallType::Fct(fct_id, type_params.clone());
        ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

        lookup.found_ret().unwrap()
    } else {
        ty_error()
    };

    ck.analysis.set_ty(e.id, ty.clone());

    ty
}

fn check_expr_call_static_method(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    let interned_method_name = ck.sa.interner.intern(&method_name);
    let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_definition)
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
        ck.analysis.map_calls.insert(e.id, call_type.clone());

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            let msg = ErrorMessage::NotAccessible;
            ck.sa.report(ck.file_id, e.span, msg);
        }

        ck.analysis.set_ty(e.id, return_type.clone());

        return_type
    } else {
        ck.analysis.set_ty(e.id, ty_error());

        ty_error()
    }
}

fn check_expr_call_method(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    if let SourceType::TypeParam(id) = object_type {
        assert_eq!(fct_type_params.len(), 0);
        return check_expr_call_generic_type_param(
            ck,
            e,
            SourceType::TypeParam(id),
            id,
            method_name,
            arg_types,
        );
    } else if object_type.is_self() {
        assert_eq!(fct_type_params.len(), 0);
        return check_expr_call_self(ck, e, method_name, arg_types);
    }

    if object_type.is_error() {
        ck.analysis.set_ty(e.id, ty_error());

        return ty_error();
    }

    let interned_method_name = ck.sa.interner.intern(&method_name);

    let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_definition)
        .no_error_reporting()
        .parent(ck.parent.clone())
        .method(object_type.clone())
        .name(interned_method_name)
        .fct_type_params(&fct_type_params)
        .args(arg_types)
        .find();

    if lookup.find() {
        let fct_id = lookup.found_fct_id().unwrap();
        let fct = ck.sa.fct(fct_id);
        let return_type = lookup.found_ret().unwrap();

        let call_type = if object_type.is_self() {
            CallType::TraitObjectMethod(object_type, fct_id)
        } else if object_type.is_trait() && fct.parent.is_trait() {
            CallType::TraitObjectMethod(object_type, fct_id)
        } else {
            let method_type = lookup.found_class_type().unwrap();
            let container_type_params = lookup.found_container_type_params().clone().unwrap();
            let type_params = container_type_params.connect(&fct_type_params);
            CallType::Method(method_type, fct_id, type_params)
        };

        ck.analysis
            .map_calls
            .insert_or_replace(e.id, Arc::new(call_type));
        ck.analysis.set_ty(e.id, return_type.clone());

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            let msg = ErrorMessage::NotAccessible;
            ck.sa.report(ck.file_id, e.span, msg);
        }

        return_type
    } else if lookup.found_fct_id().is_none() {
        // No method with this name found, so this might actually be a field
        check_expr_call_field(ck, e, object_type, method_name, fct_type_params, arg_types)
    } else {
        // Lookup the method again, but this time with error reporting
        let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_definition)
            .parent(ck.parent.clone())
            .method(object_type)
            .name(interned_method_name)
            .fct_type_params(&fct_type_params)
            .span(e.span)
            .args(arg_types)
            .find();

        assert!(!lookup.find());

        ck.analysis.set_ty(e.id, ty_error());

        ty_error()
    }
}

fn check_expr_call_field(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    object_type: SourceType,
    method_name: String,
    type_params: SourceTypeArray,
    arg_types: &[SourceType],
) -> SourceType {
    let interned_method_name = ck.sa.interner.intern(&method_name);
    if let SourceType::Class(cls_id, ..) = object_type.clone() {
        if let Some((field_id, field_type)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_method_name)
        {
            ck.analysis.set_ty(e.callee.id(), field_type.clone());
            ck.analysis.map_idents.insert_or_replace(
                e.callee.id(),
                IdentType::Field(object_type.clone(), field_id),
            );

            if !class_field_accessible_from(ck.sa, cls_id, field_id, ck.module_id) {
                let msg = ErrorMessage::NotAccessible;
                ck.sa.report(ck.file_id, e.span, msg);
            }

            return check_expr_call_expr(ck, e, field_type, arg_types);
        }
    }

    if let SourceType::Struct(struct_id, struct_type_params) = object_type.clone() {
        let struct_ = ck.sa.struct_(struct_id);
        if let Some(&field_id) = struct_.field_names.get(&interned_method_name) {
            let ident_type = IdentType::StructField(object_type.clone(), field_id);
            ck.analysis.map_idents.insert_or_replace(e.id, ident_type);

            let field = &struct_.fields[field_id.to_usize()];
            let field_type = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

            if !struct_field_accessible_from(ck.sa, struct_id, field_id, ck.module_id) {
                let msg = ErrorMessage::NotAccessible;
                ck.sa.report(ck.file_id, e.span, msg);
            }

            ck.analysis.set_ty(e.id, field_type.clone());
            return check_expr_call_expr(ck, e, field_type, arg_types);
        }
    }

    // No field with that name as well, so report method
    let lookup = MethodLookup::new(ck.sa, ck.file_id, ck.type_param_definition)
        .parent(ck.parent.clone())
        .method(object_type)
        .name(interned_method_name)
        .fct_type_params(&type_params)
        .span(e.span)
        .args(arg_types)
        .find();
    assert!(!lookup.find());

    ck.analysis.set_ty(e.id, ty_error());

    ty_error()
}

fn check_expr_call_struct(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let is_struct_accessible = struct_accessible_from(ck.sa, struct_id, ck.module_id);

    if !is_struct_accessible {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let struct_ = ck.sa.struct_(struct_id);

    if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public()
        && is_struct_accessible
    {
        let msg = ErrorMessage::StructConstructorNotAccessible(struct_.name(ck.sa));
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let ty = SourceType::Struct(struct_id, type_params.clone());
    let type_params_ok = typeparamck::check_struct(
        ck.sa,
        ck.type_param_definition,
        struct_id,
        &type_params,
        ErrorReporting::Yes(ck.file_id, e.span),
    );

    if !type_params_ok {
        ck.analysis.set_ty(e.id, ty_error());
        return ty_error();
    }

    if struct_.requires_named_arguments() {
        check_expr_call_ctor_with_named_fields(ck, struct_, type_params.clone(), &arguments);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, struct_, type_params.clone(), &arguments);
    }

    ck.analysis
        .map_calls
        .insert(e.id, Arc::new(CallType::NewStruct(struct_id, type_params)));

    ck.analysis.set_ty(e.id, ty.clone());
    ty
}

fn check_expr_call_ctor_with_named_fields(
    ck: &mut TypeCheck,
    element: &dyn ElementWithFields,
    type_params: SourceTypeArray,
    arguments: &CallArguments,
) {
    let mut args_by_name: HashMap<Name, Arc<ast::Argument>> = HashMap::new();

    let mut add_named_argument = |arg: &Arc<ast::Argument>, name: Name| {
        if args_by_name.contains_key(&name) {
            ck.sa
                .report(ck.file_id, arg.span, ErrorMessage::DuplicateNamedArgument);
        } else {
            assert!(args_by_name.insert(name, arg.clone()).is_none());
        }
    };

    for arg in &arguments.arguments {
        if let Some(ref name) = arg.name {
            let name = ck.sa.interner.intern(&name.name_as_string);
            add_named_argument(arg, name);
        } else if let Some(ident) = arg.expr.to_ident() {
            let name = ck.sa.interner.intern(&ident.name);
            add_named_argument(arg, name);
        } else if arguments.arguments.len() == 1 && element.fields_len() == 1 {
            add_named_argument(arg, element.field_name(0));
        } else {
            ck.sa.report(
                ck.file_id,
                arg.span,
                ErrorMessage::UnexpectedPositionalArgument,
            );
        }
    }

    for field in element.fields() {
        if let Some(arg) = args_by_name.remove(&field.name) {
            let def_ty = replace_type(ck.sa, field.ty, Some(&type_params), None);
            let arg_ty = ck.analysis.ty(arg.id);

            if !def_ty.allows(ck.sa, arg_ty.clone()) {
                let exp = ck.ty_name(&def_ty);
                let got = ck.ty_name(&arg_ty);

                ck.sa.report(
                    ck.file_id,
                    arg.span,
                    ErrorMessage::WrongTypeForArgument(exp, got),
                );
            }

            ck.analysis.map_argument.insert(arg.id, field.id);
        } else {
            let name = ck.sa.interner.str(field.name).to_string();
            ck.sa.report(
                ck.file_id,
                arguments.span,
                ErrorMessage::MissingNamedArgument(name),
            );
        }
    }

    for (_name, arg) in args_by_name {
        ck.sa
            .report(ck.file_id, arg.span, ErrorMessage::UseOfUnknownArgument);
    }
}

fn check_expr_call_ctor_with_unnamed_fields(
    ck: &mut TypeCheck,
    element: &dyn ElementWithFields,
    type_params: SourceTypeArray,
    arguments: &CallArguments,
) -> bool {
    for (field, argument) in element.fields().zip(&arguments.arguments) {
        let def_ty = replace_type(ck.sa, field.ty, Some(&type_params), None);
        let arg_ty = ck.analysis.ty(argument.id);

        if let Some(ref name) = argument.name {
            ck.sa
                .report(ck.file_id, name.span, ErrorMessage::UnexpectedNamedArgument);
        }

        if !def_ty.allows(ck.sa, arg_ty.clone()) {
            let exp = ck.ty_name(&def_ty);
            let got = ck.ty_name(&arg_ty);

            ck.sa.report(
                ck.file_id,
                argument.expr.span(),
                ErrorMessage::WrongTypeForArgument(exp, got),
            );
        }

        ck.analysis.map_argument.insert(argument.id, field.id);
    }

    let fields = element.fields_len();

    if arguments.arguments.len() < fields {
        ck.sa.report(
            ck.file_id,
            arguments.span,
            ErrorMessage::MissingArguments(fields, arguments.arguments.len()),
        );
    } else {
        for arg in &arguments.arguments[fields..] {
            ck.sa
                .report(ck.file_id, arg.span, ErrorMessage::SuperfluousArgument);
        }
    }

    true
}

fn check_expr_call_class(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expected_ty: SourceType,
    cls_id: ClassDefinitionId,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let is_class_accessible = class_accessible_from(ck.sa, cls_id, ck.module_id);

    if !is_class_accessible {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, e.span, msg);
    }

    let type_params = if expected_ty.cls_id() == Some(cls_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    if !typeparamck::check_class(
        ck.sa,
        ck.type_param_definition,
        cls_id,
        &type_params,
        ErrorReporting::Yes(ck.file_id, e.span),
    ) {
        return ty_error();
    };

    let cls = ck.sa.class(cls_id);
    let cls_ty = SourceType::Class(cls_id, type_params.clone());

    if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public()
        && is_class_accessible
    {
        let msg = ErrorMessage::ClassConstructorNotAccessible(cls.name(ck.sa));
        ck.sa.report(ck.file_id, e.span, msg);
    }

    if cls.requires_named_arguments {
        check_expr_call_ctor_with_named_fields(ck, cls, type_params.clone(), &arguments);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, cls, type_params.clone(), &arguments);
    }

    ck.analysis
        .map_calls
        .insert(e.id, Arc::new(CallType::NewClass(cls.id(), type_params)));

    ck.analysis.set_ty(e.id, cls_ty.clone());
    cls_ty
}

fn find_in_super_traits_self(
    sa: &Sema,
    trait_: &TraitDefinition,
    name: Name,
    matched_methods: &mut Vec<FctDefinitionId>,
) {
    for super_trait_ty in trait_.type_param_definition().bounds_for_self() {
        let super_trait_ = sa.trait_(super_trait_ty.trait_id);

        if let Some(trait_method_id) = super_trait_.get_method(name, false) {
            matched_methods.push(trait_method_id);
        }

        find_in_super_traits_self(sa, super_trait_, name, matched_methods);
    }
}

fn check_expr_call_self(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    name: String,
    args: &[SourceType],
) -> SourceType {
    let mut matched_methods = Vec::new();
    let interned_name = ck.sa.interner.intern(&name);

    {
        let trait_id = ck.parent.trait_id().expect("trait expected");
        let trait_ = ck.sa.trait_(trait_id);

        if let Some(trait_method_id) = trait_.get_method(interned_name, false) {
            matched_methods.push(trait_method_id);
        }
    }

    for trait_ty in ck.type_param_definition.bounds_for_self() {
        let trait_ = ck.sa.trait_(trait_ty.trait_id);

        if let Some(trait_method_id) = trait_.get_method(interned_name, false) {
            matched_methods.push(trait_method_id);
        }

        find_in_super_traits_self(ck.sa, trait_, interned_name, &mut matched_methods);
    }

    if matched_methods.len() == 1 {
        let trait_method_id = matched_methods.pop().expect("missing element");
        let trait_type_params = empty_sta();

        let trait_method = ck.sa.fct(trait_method_id);
        let return_type = trait_method.return_type();

        ck.analysis.set_ty(e.id, return_type.clone());

        // This should likely become a generic call in the future, once
        // the default trait method isn't copied into the impl method anymore.
        let call_type =
            CallType::Method(SourceType::This, trait_method_id, trait_type_params.clone());
        ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

        if !args_compatible_fct(
            ck.sa,
            trait_method,
            args,
            &trait_type_params,
            Some(SourceType::This),
        ) {
            let trait_params = trait_method
                .params_without_self()
                .iter()
                .map(|a| specialize_type(ck.sa, a.ty(), &trait_type_params))
                .map(|a| ck.ty_name(&a))
                .collect::<Vec<String>>();
            let param_names = args.iter().map(|a| ck.ty_name(a)).collect::<Vec<String>>();
            let msg = ErrorMessage::ParamTypesIncompatible(name, trait_params, param_names);
            ck.sa.report(ck.file_id, e.span, msg);
        }

        return_type
    } else {
        let param_names = args.iter().map(|a| ck.ty_name(a)).collect::<Vec<String>>();
        let msg = if matched_methods.is_empty() {
            ErrorMessage::UnknownMethod("Self".into(), name, param_names)
        } else {
            ErrorMessage::MultipleCandidatesForMethod("Self".into(), name, param_names)
        };

        ck.sa.report(ck.file_id, e.span, msg);
        ck.analysis.set_ty(e.id, ty_error());

        ty_error()
    }
}

fn find_in_super_traits(
    sa: &Sema,
    trait_: &TraitDefinition,
    name: Name,
    matched_methods: &mut Vec<(FctDefinitionId, TraitType)>,
) {
    for super_trait_ty in trait_.type_param_definition().bounds_for_self() {
        let super_trait_ = sa.trait_(super_trait_ty.trait_id);

        if let Some(trait_method_id) = super_trait_.get_method(name, false) {
            matched_methods.push((trait_method_id, super_trait_ty));
        }

        find_in_super_traits(sa, super_trait_, name, matched_methods);
    }
}

fn check_expr_call_generic_type_param(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    object_type: SourceType,
    id: TypeParamId,
    name: String,
    args: &[SourceType],
) -> SourceType {
    assert!(object_type.is_type_param());
    let mut matched_methods = Vec::new();
    let interned_name = ck.sa.interner.intern(&name);

    for trait_ty in ck.type_param_definition.bounds_for_type_param(id) {
        let trait_ = ck.sa.trait_(trait_ty.trait_id);

        if let Some(trait_method_id) = trait_.get_method(interned_name, false) {
            matched_methods.push((trait_method_id, trait_ty));
        }

        find_in_super_traits(ck.sa, trait_, interned_name, &mut matched_methods);
    }

    if matched_methods.len() == 1 {
        let (trait_method_id, trait_ty) = matched_methods.pop().expect("missing element");

        let trait_method = ck.sa.fct(trait_method_id);
        let return_type = trait_method.return_type();

        let return_type = replace_type(
            ck.sa,
            return_type,
            Some(&trait_ty.type_params),
            Some(object_type.clone()),
        );

        ck.analysis.set_ty(e.id, return_type.clone());

        let trait_type_params = trait_ty.type_params;

        let call_type = CallType::GenericMethod(
            id,
            trait_method.trait_id(),
            trait_method_id,
            trait_type_params.clone(),
        );
        ck.analysis.map_calls.insert(e.id, Arc::new(call_type));

        if !args_compatible_fct(
            ck.sa,
            trait_method,
            args,
            &trait_type_params,
            Some(object_type.clone()),
        ) {
            let trait_params = trait_method
                .params_without_self()
                .iter()
                .map(|a| specialize_type(ck.sa, a.ty(), &trait_type_params))
                .map(|a| ck.ty_name(&a))
                .collect::<Vec<String>>();
            let param_names = args.iter().map(|a| ck.ty_name(a)).collect::<Vec<String>>();
            let msg = ErrorMessage::ParamTypesIncompatible(name, trait_params, param_names);
            ck.sa.report(ck.file_id, e.span, msg);
        }

        return_type
    } else {
        let msg = if matched_methods.is_empty() {
            ErrorMessage::UnknownMethodForTypeParam
        } else {
            ErrorMessage::MultipleCandidatesForTypeParam
        };

        ck.sa.report(ck.file_id, e.span, msg);
        ck.analysis.set_ty(e.id, ty_error());

        ty_error()
    }
}

fn check_expr_call_path(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expected_ty: SourceType,
    callee: &ast::ExprData,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let callee_as_path = callee.to_path().unwrap();

    let (container_expr, container_type_params) = if let Some(expr_type_params) =
        callee_as_path.lhs.to_type_param()
    {
        let container_type_params: Vec<SourceType> = expr_type_params
            .args
            .iter()
            .map(|p| ck.read_type(p))
            .collect();
        let container_type_params: SourceTypeArray = SourceTypeArray::with(container_type_params);

        (&expr_type_params.callee, container_type_params)
    } else {
        (&callee_as_path.lhs, SourceTypeArray::empty())
    };
    let method_expr = &callee_as_path.rhs;

    let sym = match read_path_expr(ck, container_expr) {
        Ok(sym) => sym,
        Err(()) => {
            ck.analysis.set_ty(e.id, ty_error());
            return ty_error();
        }
    };

    let method_name = if let Some(method_name_expr) = method_expr.to_ident() {
        method_name_expr.name.clone()
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, method_expr.span(), msg);

        ck.analysis.set_ty(e.id, ty_error());
        return ty_error();
    };

    let interned_method_name = ck.sa.interner.intern(&method_name);

    match sym {
        Some(SymbolKind::Class(cls_id)) => {
            if typeparamck::check_class(
                ck.sa,
                ck.type_param_definition,
                cls_id,
                &container_type_params,
                ErrorReporting::Yes(ck.file_id, e.span),
            ) {
                let arg_types = arguments.assume_all_positional(ck);
                check_expr_call_static_method(
                    ck,
                    e,
                    SourceType::Class(cls_id, container_type_params),
                    method_name,
                    type_params,
                    &arg_types,
                )
            } else {
                ty_error()
            }
        }

        Some(SymbolKind::Struct(struct_id)) => {
            let struct_ = ck.sa.struct_(struct_id);

            if typeparamck::check_struct(
                ck.sa,
                ck.type_param_definition,
                struct_id,
                &container_type_params,
                ErrorReporting::Yes(ck.file_id, e.span),
            ) {
                let object_ty = if let Some(ref primitive_ty) = struct_.primitive_ty {
                    assert!(container_type_params.is_empty());
                    primitive_ty.clone()
                } else {
                    SourceType::Struct(struct_id, container_type_params)
                };

                let arg_types = arguments.assume_all_positional(ck);
                check_expr_call_static_method(
                    ck,
                    e,
                    object_ty,
                    method_name,
                    type_params,
                    &arg_types,
                )
            } else {
                ty_error()
            }
        }

        Some(SymbolKind::Enum(enum_id)) => {
            let enum_ = ck.sa.enum_(enum_id);

            if let Some(&variant_idx) = enum_.name_to_value().get(&interned_method_name) {
                if !container_type_params.is_empty() && !type_params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
                }

                let used_type_params = if type_params.is_empty() {
                    container_type_params
                } else {
                    type_params
                };

                let arg_types = arguments.assume_all_positional(ck);

                check_enum_value_with_args(
                    ck,
                    e,
                    expected_ty,
                    enum_id,
                    used_type_params,
                    variant_idx,
                    &arg_types,
                )
            } else {
                if typeparamck::check_enum(
                    ck.sa,
                    ck.type_param_definition,
                    enum_id,
                    &container_type_params,
                    ErrorReporting::Yes(ck.file_id, e.span),
                ) {
                    let object_ty = SourceType::Enum(enum_id, container_type_params);
                    let arg_types = arguments.assume_all_positional(ck);

                    check_expr_call_static_method(
                        ck,
                        e,
                        object_ty,
                        method_name,
                        type_params,
                        &arg_types,
                    )
                } else {
                    ty_error()
                }
            }
        }

        Some(SymbolKind::TypeParam(id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            let arg_types = arguments.assume_all_positional(ck);
            check_expr_call_generic_static_method(ck, e, id, method_name, &arg_types)
        }

        Some(SymbolKind::Module(module_id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            let sym = {
                let module = ck.sa.module(module_id);
                let table = module.table();

                table.get(interned_method_name)
            };

            check_expr_call_sym(ck, e, expected_ty, callee, sym, type_params, arguments)
        }

        Some(SymbolKind::Alias(alias_id)) => {
            if !container_type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, callee_as_path.lhs.span(), msg);
            }

            let alias_ty = ck.sa.alias(alias_id).ty();
            let arg_types = arguments.assume_all_positional(ck);

            check_expr_call_static_method(ck, e, alias_ty, method_name, type_params, &arg_types)
        }

        _ => {
            let msg = ErrorMessage::StaticMethodCallTargetExpected;
            ck.sa.report(ck.file_id, e.span, msg);

            ck.analysis.set_ty(e.id, ty_error());

            ty_error()
        }
    }
}

fn check_expr_call_sym(
    ck: &mut TypeCheck,
    e: &ast::ExprCallType,
    expected_ty: SourceType,
    callee: &ast::ExprData,
    sym: Option<SymbolKind>,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    match sym {
        Some(SymbolKind::Fct(fct_id)) => {
            let arg_types = arguments.assume_all_positional(ck);
            check_expr_call_fct(ck, e, fct_id, type_params, &arg_types)
        }

        Some(SymbolKind::Class(cls_id)) => {
            check_expr_call_class(ck, e, expected_ty, cls_id, type_params, arguments)
        }

        Some(SymbolKind::Struct(struct_id)) => {
            check_expr_call_struct(ck, e, struct_id, type_params, arguments)
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => {
            let arg_types = arguments.assume_all_positional(ck);
            check_enum_value_with_args(
                ck,
                e,
                expected_ty,
                enum_id,
                type_params,
                variant_idx,
                &arg_types,
            )
        }

        _ => {
            if !type_params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, e.callee.span(), msg);
            }

            let expr_type = check_expr(ck, callee, SourceType::Any);
            let arg_types = arguments.assume_all_positional(ck);
            check_expr_call_expr(ck, e, expr_type, &arg_types)
        }
    }
}

pub(super) fn find_method(
    ck: &mut TypeCheck,
    span: Span,
    object_type: SourceType,
    is_static: bool,
    name: Name,
    args: &[SourceType],
    fct_type_params: &SourceTypeArray,
) -> Option<MethodDescriptor> {
    let descriptor = lookup_method(
        ck.sa,
        object_type.clone(),
        ck.type_param_definition,
        is_static,
        name,
        args,
        fct_type_params,
    );

    if descriptor.is_none() {
        let type_name = ck.ty_name(&object_type);
        let name = ck.sa.interner.str(name).to_string();
        let param_names = args.iter().map(|a| ck.ty_name(a)).collect::<Vec<String>>();
        let msg = if is_static {
            ErrorMessage::UnknownStaticMethod(type_name, name, param_names)
        } else {
            ErrorMessage::UnknownMethod(type_name, name, param_names)
        };

        ck.sa.report(ck.file_id, span, msg);
    }

    descriptor
}

pub(super) struct MethodDescriptor {
    pub fct_id: FctDefinitionId,
    pub type_params: SourceTypeArray,
    pub return_type: SourceType,
}

pub(super) fn lookup_method(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    is_static: bool,
    name: Name,
    args: &[SourceType],
    fct_type_params: &SourceTypeArray,
) -> Option<MethodDescriptor> {
    let candidates = find_method_call_candidates(sa, object_type, type_param_defs, name, is_static);

    if candidates.len() == 1 {
        let method_id = candidates[0].fct_id;
        let method = sa.fct(method_id);

        let container_type_params = &candidates[0].container_type_params;
        let type_params = container_type_params.connect(fct_type_params);

        if args_compatible_fct(sa, method, args, &type_params, None) {
            let cmp_type = replace_type(sa, method.return_type(), Some(&type_params), None);

            return Some(MethodDescriptor {
                fct_id: method_id,
                type_params: type_params,
                return_type: cmp_type,
            });
        }
    }

    None
}
