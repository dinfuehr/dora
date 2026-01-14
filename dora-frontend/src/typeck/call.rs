use std::collections::HashMap;
use std::sync::Arc;

use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::access::{
    class_accessible_from, class_field_accessible_from, enum_accessible_from, fct_accessible_from,
    is_default_accessible, method_accessible_from, struct_accessible_from,
    struct_field_accessible_from,
};
use crate::args;
use crate::error::diagnostics::{
    CLASS_CONSTRUCTOR_NOT_ACCESSIBLE, DUPLICATE_NAMED_ARGUMENT, EXPECTED_MODULE,
    INDEX_GET_NOT_IMPLEMENTED, INVALID_LEFT_SIDE_OF_SEPARATOR, MISSING_ARGUMENTS,
    MISSING_NAMED_ARGUMENT, MULTIPLE_CANDIDATES_FOR_METHOD,
    MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM, MULTIPLE_CANDIDATES_FOR_TYPE_PARAM,
    NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE, STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
    SUPERFLUOUS_ARGUMENT, UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT, UNEXPECTED_NAMED_ARGUMENT,
    UNEXPECTED_POSITIONAL_ARGUMENT, UNKNOWN_METHOD, UNKNOWN_METHOD_FOR_TYPE_PARAM,
    UNKNOWN_STATIC_METHOD, UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM, USE_OF_UNKNOWN_ARGUMENT,
    WRONG_TYPE_FOR_ARGUMENT,
};
use crate::interner::Name;
use crate::sema::{
    CallType, ClassDefinitionId, Element, ElementWithFields, EnumDefinitionId, FctDefinitionId,
    IdentType, Param, Sema, StructDefinitionId, TraitDefinition, TypeParamId, find_field_in_class,
    find_impl, new_identity_type_params,
};
use crate::specialize::replace_type;
use crate::specialize_ty_for_call;
use crate::sym::SymbolKind;
use crate::typeck::{
    CallArguments, TypeCheck, check_args_compatible, check_args_compatible_fct,
    check_args_compatible_fct2, check_expr, check_type_params, find_method_call_candidates,
};
use crate::{
    CallSpecializationData, SourceType, SourceTypeArray, TraitType, empty_sta,
    specialize_ty_for_generic, specialize_type, ty::error as ty_error,
};

fn check_expr_call_generic_static_method(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    tp_id: TypeParamId,
    name: String,
    pure_fct_type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let mut matched_methods = Vec::new();
    let interned_name = ck.sa.interner.intern(&name);

    for trait_ty in ck.type_param_definition.bounds_for_type_param(tp_id) {
        let trait_ = ck.sa.trait_(trait_ty.trait_id);

        if let Some(trait_method_id) = trait_.get_method(interned_name, true) {
            matched_methods.push((trait_method_id, trait_ty));
        }
    }

    if matched_methods.len() != 1 {
        let desc = if matched_methods.len() > 1 {
            &MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM
        } else {
            &UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM
        };

        ck.report(e.span(), desc, args!());

        ck.body.set_ty(expr_ast_id, ty_error());
        return ty_error();
    }

    let (trait_method_id, trait_ty) = matched_methods.pop().expect("missing method");
    let trait_method = ck.sa.fct(trait_method_id);

    let tp = SourceType::TypeParam(tp_id);
    let combined_fct_type_params = trait_ty.type_params.connect(&pure_fct_type_params);

    if check_type_params(
        ck.sa,
        ck.element,
        &ck.type_param_definition,
        trait_method,
        &combined_fct_type_params,
        ck.file_id,
        e.span(),
        |ty| {
            specialize_ty_for_generic(
                ck.sa,
                ty,
                ck.element,
                tp_id,
                &trait_ty,
                &combined_fct_type_params,
                &tp,
            )
        },
    ) {
        check_args_compatible_fct2(ck, trait_method, arguments, |ty| {
            specialize_ty_for_generic(
                ck.sa,
                ty,
                ck.element,
                tp_id,
                &trait_ty,
                &combined_fct_type_params,
                &tp,
            )
        });

        let call_type = CallType::GenericStaticMethod(
            tp_id,
            trait_ty.trait_id,
            trait_method_id,
            trait_ty.type_params.clone(),
            pure_fct_type_params,
        );
        ck.body.insert_call_type(expr_ast_id, Arc::new(call_type));

        let return_type = specialize_ty_for_generic(
            ck.sa,
            trait_method.return_type(),
            ck.element,
            tp_id,
            &trait_ty,
            &combined_fct_type_params,
            &tp,
        );

        ck.body.set_ty(expr_ast_id, return_type.clone());

        return_type
    } else {
        SourceType::Error
    }
}

pub(super) fn check_expr_call_expr(
    ck: &mut TypeCheck,
    expr: ast::AstExpr,
    expr_type: SourceType,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = expr.id();
    if expr_type.is_error() {
        ck.body.set_ty(expr_ast_id, ty_error());
        return ty_error();
    }

    if expr_type.is_lambda() {
        return check_expr_call_expr_lambda(ck, expr, expr_type, arguments);
    }

    let trait_id = ck.sa.known.traits.index_get();
    let trait_ty = TraitType::from_trait_id(trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        expr_type.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let trait_method_name = ck.sa.interner.intern("get");
        let trait_ = ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id)
            .expect("method not found");

        let call_type = CallType::Expr(expr_type.clone(), method_id, impl_match.bindings.clone());
        ck.body
            .insert_or_replace_call_type(expr_ast_id, Arc::new(call_type));

        let method = ck.sa.fct(method_id);

        check_args_compatible_fct(ck, method, arguments, &impl_match.bindings, None, |ty| ty);

        let return_type = specialize_type(ck.sa, method.return_type(), &impl_match.bindings);
        ck.body.set_ty(expr_ast_id, return_type.clone());

        return_type
    } else {
        let ty = ck.ty_name(&expr_type);
        ck.report(expr.span(), &INDEX_GET_NOT_IMPLEMENTED, args!(ty));

        ck.body.set_ty(expr_ast_id, ty_error());

        ty_error()
    }
}

fn check_expr_call_expr_lambda(
    ck: &mut TypeCheck,
    expr: ast::AstExpr,
    expr_type: SourceType,
    arguments: CallArguments,
) -> SourceType {
    let node_id = expr.id();
    let (params, return_type) = expr_type.to_lambda().expect("lambda expected");

    // Type params are mapped to themselves.
    let type_params_count = ck.type_param_definition.type_param_count();
    let type_params = new_identity_type_params(0, type_params_count);

    let regular_params = params.iter().map(|p| Param::new_ty(p)).collect::<Vec<_>>();

    check_args_compatible(
        ck,
        regular_params.as_slice(),
        None,
        &arguments,
        &type_params,
        None,
        |ty| ty,
    );

    let call_type = CallType::Lambda(params, return_type.clone());

    ck.body
        .insert_or_replace_call_type(node_id, Arc::new(call_type));

    ck.body.set_ty(node_id, return_type.clone());
    return_type
}

fn check_expr_call_fct(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let fct = ck.sa.fct(fct_id);

    if !fct_accessible_from(ck.sa, fct_id, ck.module_id) {
        ck.report(e.span(), &NOT_ACCESSIBLE, args!());
    }

    let ty = if check_type_params(
        ck.sa,
        ck.element,
        &ck.type_param_definition,
        fct,
        &type_params,
        ck.file_id,
        e.span(),
        |ty| specialize_type(ck.sa, ty, &type_params),
    ) {
        check_args_compatible_fct(ck, fct, arguments, &type_params, None, |ty| ty);
        specialize_type(ck.sa, fct.return_type(), &type_params)
    } else {
        ty_error()
    };

    ck.body.set_ty(expr_ast_id, ty.clone());

    let call_type = CallType::Fct(fct_id, type_params.clone());
    ck.body.insert_call_type(expr_ast_id, Arc::new(call_type));

    ty
}

fn check_expr_call_static_method(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let interned_method_name = ck.sa.interner.intern(&method_name);

    let candidates = find_method_call_candidates(
        ck.sa,
        ck.element,
        &ck.symtable,
        object_type.clone(),
        &ck.type_param_definition,
        interned_method_name,
        true,
    );

    if candidates.is_empty() {
        let type_name = ck.ty_name(&object_type);
        ck.report(
            e.span(),
            &UNKNOWN_STATIC_METHOD,
            args!(type_name, method_name),
        );
        ck.body.set_ty(expr_ast_id, ty_error());
        ty_error()
    } else if candidates.len() > 1 {
        let type_name = ck.ty_name(&object_type);
        ck.report(
            e.span(),
            &MULTIPLE_CANDIDATES_FOR_METHOD,
            args!(type_name, method_name),
        );
        ck.body.set_ty(expr_ast_id, ty_error());
        ty_error()
    } else {
        let candidate = &candidates[0];
        let fct_id = candidate.fct_id;
        let fct = ck.sa.fct(fct_id);

        let full_type_params = candidate.container_type_params.connect(&fct_type_params);

        let ty = if check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            fct,
            &full_type_params,
            ck.file_id,
            e.span(),
            |ty| specialize_type(ck.sa, ty, &full_type_params),
        ) {
            check_args_compatible_fct(ck, fct, arguments, &full_type_params, None, |ty| ty);
            specialize_type(ck.sa, fct.return_type(), &full_type_params)
        } else {
            ty_error()
        };

        let call_type = Arc::new(CallType::Fct(fct_id, full_type_params));
        ck.body.insert_call_type(expr_ast_id, call_type.clone());

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            ck.report(e.span(), &NOT_ACCESSIBLE, args!());
        }

        ck.body.set_ty(expr_ast_id, ty.clone());
        ty
    }
}

pub(super) fn check_expr_call_method(
    ck: &mut TypeCheck,
    call_expr: ast::AstExpr,
    callee_expr: ast::AstExpr,
    callee_span: Span,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let call_ast_id = call_expr.id();
    let call_span = call_expr.span();
    if let SourceType::TypeParam(id) = object_type {
        return check_expr_call_generic_type_param(
            ck,
            call_expr.clone(),
            SourceType::TypeParam(id),
            id,
            method_name,
            fct_type_params,
            arguments,
        );
    } else if object_type.is_assoc() {
        assert_eq!(fct_type_params.len(), 0);
        return check_expr_call_assoc(ck, call_expr.clone(), method_name, object_type, arguments);
    } else if object_type.is_self() {
        assert_eq!(fct_type_params.len(), 0);
        return check_expr_call_self(ck, call_expr.clone(), method_name, arguments);
    }

    if object_type.is_error() {
        ck.body.set_ty(call_ast_id, ty_error());

        return ty_error();
    }

    let interned_method_name = ck.sa.interner.intern(&method_name);

    let candidates = find_method_call_candidates(
        ck.sa,
        ck.element,
        &ck.symtable,
        object_type.clone(),
        &ck.type_param_definition,
        interned_method_name,
        false,
    );

    if candidates.is_empty() {
        // No method with this name found, so this might actually be a field
        check_expr_call_field(
            ck,
            call_expr.clone(),
            callee_expr.clone(),
            callee_span,
            object_type,
            method_name,
            fct_type_params,
            arguments,
        )
    } else if candidates.len() > 1 {
        let type_name = ck.ty_name(&object_type);
        ck.report(
            call_span,
            &MULTIPLE_CANDIDATES_FOR_METHOD,
            args!(type_name, method_name),
        );
        ck.body.set_ty(call_ast_id, ty_error());
        ty_error()
    } else {
        let candidate = &candidates[0];
        let fct_id = candidate.fct_id;
        let fct = ck.sa.fct(fct_id);

        let full_type_params = candidate.container_type_params.connect(&fct_type_params);

        let ty = if check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            fct,
            &full_type_params,
            ck.file_id,
            call_span,
            |ty| specialize_type(ck.sa, ty, &full_type_params),
        ) {
            let call_data = CallSpecializationData {
                object_ty: candidate.object_type.clone(),
                type_params: full_type_params.clone(),
            };

            check_args_compatible_fct2(ck, fct, arguments, |ty| {
                specialize_ty_for_call(ck.sa, ty, ck.element, &call_data)
            });

            specialize_ty_for_call(ck.sa, fct.return_type(), ck.element, &call_data)
        } else {
            ty_error()
        };

        let call_type = if object_type.is_self() {
            CallType::TraitObjectMethod(object_type, fct_id)
        } else if object_type.is_trait_object() && fct.parent.is_trait() {
            CallType::TraitObjectMethod(object_type, fct_id)
        } else {
            CallType::Method(object_type, fct_id, full_type_params)
        };

        ck.body.insert_call_type(call_ast_id, Arc::new(call_type));

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            ck.report(call_span, &NOT_ACCESSIBLE, args!());
        }

        ck.body.set_ty(call_ast_id, ty.clone());
        ty
    }
}

fn check_expr_call_field(
    ck: &mut TypeCheck,
    call_expr: ast::AstExpr,
    _callee_expr: ast::AstExpr,
    callee_span: Span,
    object_type: SourceType,
    method_name: String,
    _type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let call_ast_id = call_expr.id();
    let interned_method_name = ck.sa.interner.intern(&method_name);
    if let SourceType::Class(cls_id, ..) = object_type.clone() {
        if let Some((field_id, field_type)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_method_name)
        {
            ck.body.set_ty(call_ast_id, field_type.clone());
            ck.body.insert_or_replace_ident(
                call_ast_id,
                IdentType::Field(object_type.clone(), field_id),
            );

            if !class_field_accessible_from(ck.sa, cls_id, field_id, ck.module_id) {
                ck.report(callee_span, &NOT_ACCESSIBLE, args!());
            }

            return check_expr_call_expr(ck, call_expr.clone(), field_type, arguments);
        }
    }

    if let SourceType::Struct(struct_id, struct_type_params) = object_type.clone() {
        let struct_ = ck.sa.struct_(struct_id);
        if let Some(&field_index) = struct_.field_names().get(&interned_method_name) {
            let ident_type = IdentType::StructField(object_type.clone(), field_index);
            ck.body.insert_or_replace_ident(call_ast_id, ident_type);

            let field_id = struct_.field_id(field_index);
            let field = ck.sa.field(field_id);
            let field_type = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

            if !struct_field_accessible_from(ck.sa, struct_id, field_index, ck.module_id) {
                ck.report(callee_span, &NOT_ACCESSIBLE, args!());
            }

            ck.body.set_ty(call_ast_id, field_type.clone());
            return check_expr_call_expr(ck, call_expr, field_type, arguments);
        }
    }

    let ty = ck.ty_name(&object_type);
    ck.report(call_expr.span(), &UNKNOWN_METHOD, args!(ty, method_name));

    ck.body.set_ty(call_ast_id, ty_error());

    ty_error()
}

fn check_expr_call_struct(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let is_struct_accessible = struct_accessible_from(ck.sa, struct_id, ck.module_id);

    if !is_struct_accessible {
        ck.report(e.span(), &NOT_ACCESSIBLE, args!());
    }

    let struct_ = ck.sa.struct_(struct_id);

    if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public(ck.sa)
        && is_struct_accessible
    {
        ck.report(
            e.span(),
            &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(struct_.name(ck.sa)),
        );
    }

    let ty = SourceType::Struct(struct_id, type_params.clone());
    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        struct_,
        &type_params,
        ck.file_id,
        e.span(),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    if !type_params_ok {
        ck.body.set_ty(expr_ast_id, ty_error());
        return ty_error();
    }

    if struct_.field_name_style.is_named() {
        check_expr_call_ctor_with_named_fields(ck, struct_, type_params.clone(), &arguments);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, struct_, type_params.clone(), &arguments);
    }

    ck.body.insert_call_type(
        expr_ast_id,
        Arc::new(CallType::NewStruct(struct_id, type_params)),
    );

    ck.body.set_ty(expr_ast_id, ty.clone());
    ty
}

fn check_expr_call_ctor_with_named_fields(
    ck: &mut TypeCheck,
    element_with_fields: &dyn ElementWithFields,
    type_params: SourceTypeArray,
    arguments: &CallArguments,
) {
    let mut args_by_name: HashMap<Name, ast::AstArgument> = HashMap::new();

    let mut add_named_argument = |arg: ast::AstArgument, name: Name| {
        if args_by_name.contains_key(&name) {
            ck.sa
                .report(ck.file_id, arg.span(), &DUPLICATE_NAMED_ARGUMENT, args!());
        } else {
            assert!(args_by_name.insert(name, arg).is_none());
        }
    };

    let single_named_element = compute_single_named_element(ck.sa, element_with_fields);

    for arg in &arguments.arguments {
        if let Some(name_ident) = arg.name() {
            let name = ck.sa.interner.intern(name_ident.text());
            add_named_argument(arg.clone(), name);
        } else if arguments.arguments.len() == 1 && single_named_element.is_some() {
            add_named_argument(arg.clone(), single_named_element.expect("missing name"));
        } else if let Some(ident) = arg.expr().and_then(|e| e.to_path_expr()) {
            // Only infer names from single-segment identifiers without type params
            let segments: Vec<_> = ident.segments().collect();
            if segments.len() == 1 && !segments[0].has_type_params() {
                if let Some(name_token) = segments[0].name() {
                    let name = ck.sa.interner.intern(name_token.text());
                    add_named_argument(arg.clone(), name);
                } else {
                    ck.report(arg.span(), &UNEXPECTED_POSITIONAL_ARGUMENT, args!());
                }
            } else {
                ck.report(arg.span(), &UNEXPECTED_POSITIONAL_ARGUMENT, args!());
            }
        } else {
            ck.report(arg.span(), &UNEXPECTED_POSITIONAL_ARGUMENT, args!());
        }
    }

    let call_data = CallSpecializationData {
        object_ty: SourceType::Error,
        type_params,
    };

    for &field_id in element_with_fields.field_ids() {
        let field = ck.sa.field(field_id);
        if let Some(name) = field.name {
            if let Some(arg) = args_by_name.remove(&name) {
                let arg_id = arg.id();
                let def_ty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);
                let arg_ty = ck.ty(arg_id);

                if !def_ty.allows(ck.sa, arg_ty.clone()) && !arg_ty.is_error() {
                    let exp = ck.ty_name(&def_ty);
                    let got = ck.ty_name(&arg_ty);

                    ck.report(arg.span(), &WRONG_TYPE_FOR_ARGUMENT, args!(exp, got));
                }

                ck.body.insert_argument(arg_id, field.index.to_usize());
            } else {
                let name = ck.sa.interner.str(name).to_string();
                ck.report(arguments.span, &MISSING_NAMED_ARGUMENT, args!(name));
            }
        }
    }

    for (_name, arg) in args_by_name {
        ck.sa
            .report(ck.file_id, arg.span(), &USE_OF_UNKNOWN_ARGUMENT, args!());
    }
}

fn compute_single_named_element(sa: &Sema, el: &dyn ElementWithFields) -> Option<Name> {
    if el.field_ids().len() != 1 {
        return None;
    }

    let field_id = el.field_ids().first().cloned().expect("first missing");
    sa.field(field_id).name
}

fn check_expr_call_ctor_with_unnamed_fields(
    ck: &mut TypeCheck,
    element_with_fields: &dyn ElementWithFields,
    type_params: SourceTypeArray,
    arguments: &CallArguments,
) -> bool {
    let call_data = CallSpecializationData {
        object_ty: SourceType::Error,
        type_params,
    };

    for (&field_id, arg) in element_with_fields
        .field_ids()
        .iter()
        .zip(&arguments.arguments)
    {
        let field = ck.sa.field(field_id);
        let def_ty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);
        let arg_ty = ck.ty(arg.id());

        if arg.name().is_some() {
            ck.report(arg.span(), &UNEXPECTED_NAMED_ARGUMENT, args!());
        }

        if !def_ty.allows(ck.sa, arg_ty.clone()) && !arg_ty.is_error() {
            let exp = ck.ty_name(&def_ty);
            let got = ck.ty_name(&arg_ty);

            ck.report(
                arg.expr().unwrap().span(),
                &WRONG_TYPE_FOR_ARGUMENT,
                args!(exp, got),
            );
        }

        ck.body.insert_argument(arg.id(), field.index.to_usize());
    }

    let fields = element_with_fields.field_ids().len();

    if arguments.arguments.len() < fields {
        ck.report(
            arguments.span,
            &MISSING_ARGUMENTS,
            args!(fields, arguments.arguments.len()),
        );
    } else {
        for arg in &arguments.arguments[fields..] {
            ck.report(arg.span(), &SUPERFLUOUS_ARGUMENT, args!());
        }
    }

    true
}

fn check_expr_call_class(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    expected_ty: SourceType,
    cls_id: ClassDefinitionId,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let is_class_accessible = class_accessible_from(ck.sa, cls_id, ck.module_id);

    if !is_class_accessible {
        ck.report(e.span(), &NOT_ACCESSIBLE, args!());
    }

    let type_params = if expected_ty.cls_id() == Some(cls_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let cls = ck.sa.class(cls_id);

    if !check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        cls,
        &type_params,
        ck.file_id,
        e.span(),
        |ty| specialize_type(ck.sa, ty, &type_params),
    ) {
        return ty_error();
    };

    let cls_ty = SourceType::Class(cls_id, type_params.clone());

    if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public(ck.sa)
        && is_class_accessible
    {
        ck.report(
            e.span(),
            &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(cls.name(ck.sa)),
        );
    }

    if cls.field_name_style.is_named() {
        check_expr_call_ctor_with_named_fields(ck, cls, type_params.clone(), &arguments);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, cls, type_params.clone(), &arguments);
    }

    ck.body.insert_call_type(
        expr_ast_id,
        Arc::new(CallType::NewClass(cls.id(), type_params)),
    );

    ck.body.set_ty(expr_ast_id, cls_ty.clone());
    cls_ty
}

pub(super) fn check_expr_call_enum_variant(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let enum_ = ck.sa.enum_(enum_id);
    let variant_id = enum_.variant_id_at(variant_idx as usize);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(e.span(), &NOT_ACCESSIBLE, args!());
    }

    let type_params = if expected_ty.enum_id() == Some(enum_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        enum_,
        &type_params,
        ck.file_id,
        e.span(),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    if !type_params_ok {
        ck.body.set_ty(expr_ast_id, ty_error());
        return ty_error();
    }

    let variant = ck.sa.variant(variant_id);

    if variant.field_ids().is_empty() {
        ck.report(e.span(), &UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT, args!());
    } else {
        if variant.field_name_style.is_named() {
            check_expr_call_ctor_with_named_fields(ck, variant, type_params.clone(), &arguments);
        } else {
            check_expr_call_ctor_with_unnamed_fields(ck, variant, type_params.clone(), &arguments);
        }
    }

    let ty = SourceType::Enum(enum_id, type_params);

    ck.body.insert_call_type(
        expr_ast_id,
        Arc::new(CallType::NewEnum(ty.clone(), variant_idx)),
    );

    ck.body.set_ty(expr_ast_id, ty.clone());
    ty
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
    expr: ast::AstExpr,
    name: String,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = expr.id();
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

        ck.body.set_ty(expr_ast_id, return_type.clone());

        ck.body.insert_call_type(
            expr_ast_id,
            Arc::new(CallType::GenericMethodSelf(
                trait_method.trait_id(),
                trait_method_id,
                trait_type_params.clone(),
                SourceTypeArray::empty(),
            )),
        );

        check_args_compatible_fct(
            ck,
            trait_method,
            arguments,
            &trait_type_params,
            Some(SourceType::This),
            |ty| ty,
        );

        return_type
    } else {
        if matched_methods.is_empty() {
            ck.report(
                expr.span(),
                &UNKNOWN_METHOD,
                args!("Self".to_string(), name),
            );
        } else {
            ck.report(
                expr.span(),
                &MULTIPLE_CANDIDATES_FOR_METHOD,
                args!("Self".to_string(), name),
            );
        }
        ck.body.set_ty(expr_ast_id, ty_error());

        ty_error()
    }
}

fn check_expr_call_assoc(
    ck: &mut TypeCheck,
    expr: ast::AstExpr,
    name: String,
    object_type: SourceType,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = expr.id();
    let mut matched_methods = Vec::new();
    let interned_name = ck.sa.interner.intern(&name);

    assert!(object_type.is_assoc());

    for bound in ck.type_param_definition.bounds() {
        if object_type != bound.ty() {
            continue;
        }

        if let Some(trait_ty) = bound.trait_ty() {
            let trait_ = ck.sa.trait_(trait_ty.trait_id);

            if let Some(trait_method_id) = trait_.get_method(interned_name, false) {
                matched_methods.push((trait_method_id, trait_ty));
            }
        }
    }

    if matched_methods.len() == 1 {
        let (trait_method_id, trait_ty) = matched_methods.pop().expect("missing element");
        let trait_type_params = empty_sta();

        let trait_method = ck.sa.fct(trait_method_id);
        let return_type = trait_method.return_type();

        ck.body.set_ty(expr_ast_id, return_type.clone());

        ck.body.insert_call_type(
            expr_ast_id,
            Arc::new(CallType::GenericMethodNew {
                object_type,
                trait_ty,
                fct_id: trait_method_id,
                fct_type_params: SourceTypeArray::empty(),
            }),
        );

        check_args_compatible_fct(
            ck,
            trait_method,
            arguments,
            &trait_type_params,
            Some(SourceType::This),
            |ty| ty,
        );

        return_type
    } else {
        let object_type = object_type.name(ck.sa);
        if matched_methods.is_empty() {
            ck.report(expr.span(), &UNKNOWN_METHOD, args!(object_type, name));
        } else {
            ck.report(
                expr.span(),
                &MULTIPLE_CANDIDATES_FOR_METHOD,
                args!(object_type, name),
            );
        }
        ck.body.set_ty(expr_ast_id, ty_error());

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
    expr: ast::AstExpr,
    object_type: SourceType,
    id: TypeParamId,
    name: String,
    pure_fct_type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = expr.id();
    let expr_span = expr.span();
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
        let combined_fct_type_params = trait_ty.type_params.connect(&pure_fct_type_params);

        if check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            trait_method,
            &combined_fct_type_params,
            ck.file_id,
            expr_span,
            |ty| {
                specialize_ty_for_generic(
                    ck.sa,
                    ty,
                    ck.element,
                    id,
                    &trait_ty,
                    &combined_fct_type_params,
                    &object_type,
                )
            },
        ) {
            let return_type = specialize_ty_for_generic(
                ck.sa,
                trait_method.return_type(),
                ck.element,
                id,
                &trait_ty,
                &combined_fct_type_params,
                &object_type,
            );

            ck.body.set_ty(expr_ast_id, return_type.clone());

            let call_type = CallType::GenericMethod(
                id,
                trait_method.trait_id(),
                trait_method_id,
                trait_ty.type_params.clone(),
                pure_fct_type_params,
            );
            ck.body.insert_call_type(expr_ast_id, Arc::new(call_type));

            check_args_compatible_fct2(ck, trait_method, arguments, |ty| {
                specialize_ty_for_generic(
                    ck.sa,
                    ty,
                    ck.element,
                    id,
                    &trait_ty,
                    &combined_fct_type_params,
                    &object_type,
                )
            });

            return_type
        } else {
            SourceType::Error
        }
    } else {
        if matched_methods.is_empty() {
            ck.report(expr_span, &UNKNOWN_METHOD_FOR_TYPE_PARAM, args!());
        } else {
            ck.report(expr_span, &MULTIPLE_CANDIDATES_FOR_TYPE_PARAM, args!());
        }
        ck.body.set_ty(expr_ast_id, ty_error());

        ty_error()
    }
}

pub(super) fn check_expr_call_sym(
    ck: &mut TypeCheck,
    call_expr: ast::AstExpr,
    e: ast::AstCallExpr,
    expected_ty: SourceType,
    callee: ast::AstExpr,
    sym: Option<SymbolKind>,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    match sym {
        Some(SymbolKind::Fct(fct_id)) => check_expr_call_fct(ck, e, fct_id, type_params, arguments),

        Some(SymbolKind::Class(cls_id)) => {
            check_expr_call_class(ck, e, expected_ty, cls_id, type_params, arguments)
        }

        Some(SymbolKind::Struct(struct_id)) => {
            check_expr_call_struct(ck, e, struct_id, type_params, arguments)
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_expr_call_enum_variant(
            ck,
            e,
            expected_ty,
            enum_id,
            type_params,
            variant_idx,
            arguments,
        ),

        _ => {
            if !type_params.is_empty() {
                ck.report(e.callee().span(), &NO_TYPE_PARAMS_EXPECTED, args!());
            }

            let expr_type = check_expr(ck, callee, SourceType::Any);
            check_expr_call_expr(ck, call_expr, expr_type, arguments)
        }
    }
}

pub(super) fn check_expr_call_path_name(
    ck: &mut TypeCheck,
    e: ast::AstCallExpr,
    expected_ty: SourceType,
    callee_expr: ast::AstPathExpr,
    type_params: SourceTypeArray,
    arguments: CallArguments,
) -> SourceType {
    let expr_ast_id = e.id();
    let segments: Vec<_> = callee_expr.segments().collect();

    // Extract names from segments
    let segment_names: Vec<_> = segments.iter().filter_map(|s| s.name()).collect();
    if segment_names.len() != segments.len() {
        ck.body.set_ty(expr_ast_id, ty_error());
        return ty_error();
    }

    // Resolve through modules to get the container symbol
    let first_name = ck.sa.interner.intern(segment_names[0].text());
    let mut sym = ck.symtable.get(first_name);

    // Resolve intermediate segments (all but the last one)
    for segment_name in &segment_names[1..segment_names.len() - 1] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                let name = ck.sa.interner.intern(segment_name.text());
                sym = symtable.get(name);
            }
            _ => {
                ck.report(callee_expr.span(), &EXPECTED_MODULE, args![]);
                ck.body.set_ty(expr_ast_id, ty_error());
                return ty_error();
            }
        }
    }

    // The last segment is the method/constructor/variant name
    let last_segment_name = &segment_names[segment_names.len() - 1];
    let method_name = last_segment_name.text().to_string();
    let interned_method_name = ck.sa.interner.intern(&method_name);

    // Extract type params from the second-to-last segment (the class/struct/enum segment)
    let container_type_params = if segments.len() >= 2 {
        let container_segment = &segments[segments.len() - 2];
        let params: Vec<SourceType> = container_segment
            .type_params()
            .filter_map(|arg| arg.ty())
            .map(|ty| ck.read_type(ty))
            .collect();
        SourceTypeArray::with(params)
    } else {
        SourceTypeArray::empty()
    };

    match sym {
        Some(SymbolKind::Class(cls_id)) => {
            let cls = ck.sa.class(cls_id);
            if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                cls,
                &container_type_params,
                ck.file_id,
                e.span(),
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                check_expr_call_static_method(
                    ck,
                    e,
                    SourceType::Class(cls_id, container_type_params),
                    method_name,
                    type_params,
                    arguments,
                )
            } else {
                ty_error()
            }
        }

        Some(SymbolKind::Struct(struct_id)) => {
            let struct_ = ck.sa.struct_(struct_id);

            if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                struct_,
                &container_type_params,
                ck.file_id,
                e.span(),
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                let object_ty = if let Some(ref primitive_ty) = struct_.primitive_ty {
                    assert!(container_type_params.is_empty());
                    primitive_ty.clone()
                } else {
                    SourceType::Struct(struct_id, container_type_params)
                };

                check_expr_call_static_method(ck, e, object_ty, method_name, type_params, arguments)
            } else {
                ty_error()
            }
        }

        Some(SymbolKind::Enum(enum_id)) => {
            let enum_ = ck.sa.enum_(enum_id);

            if let Some(&variant_idx) = enum_.name_to_value().get(&interned_method_name) {
                // Check if both enum and variant have type params - that's an error
                if !container_type_params.is_empty() && !type_params.is_empty() {
                    let variant_segment = &segments[segments.len() - 1];
                    ck.report(variant_segment.span(), &NO_TYPE_PARAMS_EXPECTED, args![]);
                }

                // Use container_type_params for enum variant calls like Option[Int]::Some(x)
                let used_type_params = if container_type_params.is_empty() {
                    type_params.clone()
                } else {
                    container_type_params.clone()
                };
                check_expr_call_enum_variant(
                    ck,
                    e,
                    expected_ty,
                    enum_id,
                    used_type_params,
                    variant_idx,
                    arguments,
                )
            } else {
                if check_type_params(
                    ck.sa,
                    ck.element,
                    ck.type_param_definition,
                    enum_,
                    &container_type_params,
                    ck.file_id,
                    e.span(),
                    |ty| specialize_type(ck.sa, ty, &container_type_params),
                ) {
                    let object_ty = SourceType::Enum(enum_id, container_type_params);

                    check_expr_call_static_method(
                        ck,
                        e,
                        object_ty,
                        method_name,
                        type_params,
                        arguments,
                    )
                } else {
                    ty_error()
                }
            }
        }

        Some(SymbolKind::TypeParam(id)) => {
            check_expr_call_generic_static_method(ck, e, id, method_name, type_params, arguments)
        }

        Some(SymbolKind::Module(module_id)) => {
            let sym = {
                let module = ck.sa.module(module_id);
                let table = module.table();
                table.get(interned_method_name)
            };

            check_expr_call_sym(
                ck,
                e.clone().into(),
                e,
                expected_ty,
                callee_expr.clone().into(),
                sym,
                type_params,
                arguments,
            )
        }

        Some(SymbolKind::Alias(alias_id)) => {
            let alias_ty = ck.sa.alias(alias_id).ty();
            check_expr_call_static_method(ck, e, alias_ty, method_name, type_params, arguments)
        }

        _ => {
            ck.report(callee_expr.span(), &INVALID_LEFT_SIDE_OF_SEPARATOR, args![]);
            ck.body.set_ty(expr_ast_id, ty_error());
            ty_error()
        }
    }
}
