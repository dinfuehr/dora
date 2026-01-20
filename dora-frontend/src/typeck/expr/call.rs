use std::collections::HashMap;
use std::rc::Rc;

use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::access::{
    class_accessible_from, enum_accessible_from, fct_accessible_from, is_default_accessible,
    method_accessible_from, struct_accessible_from,
};
use crate::args;
use crate::error::diagnostics::{
    CLASS_CONSTRUCTOR_NOT_ACCESSIBLE, DUPLICATE_NAMED_ARGUMENT, INDEX_GET_NOT_IMPLEMENTED,
    INVALID_LEFT_SIDE_OF_SEPARATOR, MISSING_ARGUMENTS, MISSING_NAMED_ARGUMENT,
    MULTIPLE_CANDIDATES_FOR_METHOD, MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM,
    NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE, STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
    SUPERFLUOUS_ARGUMENT, UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT, UNEXPECTED_NAMED_ARGUMENT,
    UNEXPECTED_POSITIONAL_ARGUMENT, UNKNOWN_IDENTIFIER_IN_MODULE, UNKNOWN_STATIC_METHOD,
    UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM, USE_OF_UNKNOWN_ARGUMENT, WRONG_TYPE_FOR_ARGUMENT,
};
use crate::interner::Name;
use crate::sema::{
    CallExpr, CallType, ClassDefinitionId, ElementWithFields, EnumDefinitionId, Expr, ExprId,
    FctDefinitionId, Param, Sema, StructDefinitionId, TypeParamId, find_impl,
};
use crate::specialize_ty_for_call;
use crate::sym::SymbolKind;
use crate::typeck::{
    TypeCheck, call_arg_name_span, call_arg_span, check_expr, check_type_params,
    expr::resolve_path, find_method_call_candidates,
};
use crate::{
    CallSpecializationData, SourceType, SourceTypeArray, TraitType, replace_type,
    specialize_ty_for_generic, specialize_type, ty::error as ty_error,
};

pub(crate) fn check_expr_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &CallExpr,
    expected_ty: SourceType,
) -> SourceType {
    let call_expr_id = expr_id;

    let callee_expr = ck.expr(sema_expr.callee);

    match callee_expr {
        Expr::Path(name_expr) => {
            // Get type params from the last segment
            let type_params = if let Some(last_segment) = name_expr.path.last() {
                let params: Vec<SourceType> = last_segment
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type(ty))
                    .collect();
                SourceTypeArray::with(params)
            } else {
                SourceTypeArray::empty()
            };

            if name_expr.path.len() == 1 {
                // Single segment: simple identifier lookup
                let sym = match resolve_path(ck, sema_expr.callee, &name_expr.path) {
                    Ok(sym) => sym,
                    Err(()) => {
                        check_call_arguments_any(ck, call_expr_id);
                        ck.body.set_ty(expr_id, ty_error());
                        return ty_error();
                    }
                };

                check_expr_call_sym(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    sym,
                    type_params,
                    call_expr_id,
                )
            } else {
                // Multi-segment path
                check_expr_call_path_name(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    type_params,
                    call_expr_id,
                )
            }
        }

        _ => {
            let expr_type = check_expr(ck, sema_expr.callee, SourceType::Any);
            check_expr_call_expr(ck, expr_id, expr_type, call_expr_id)
        }
    }
}

pub(crate) fn check_call_arguments(ck: &mut TypeCheck, sema_expr: &CallExpr) {
    for sema_arg in &sema_expr.args {
        let ty = check_expr(ck, sema_arg.expr, SourceType::Any);
        ck.body.set_ty(sema_arg.expr, ty);
    }
}

pub(crate) fn check_call_arguments_any(ck: &mut TypeCheck, call_expr_id: ExprId) {
    let arg_ids = ck
        .call_args(call_expr_id)
        .iter()
        .map(|arg| arg.expr)
        .collect::<Vec<_>>();

    for arg_id in arg_ids {
        let ty = check_expr(ck, arg_id, SourceType::Any);
        ck.body.set_ty(arg_id, ty);
    }
}

fn build_expected_call_args<S>(
    ck: &TypeCheck,
    regular_params: &[Param],
    variadic_param: Option<&Param>,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
    mut specialize: S,
) -> ExpectedCallArgs
where
    S: FnMut(SourceType) -> SourceType,
{
    let regular_types = regular_params
        .iter()
        .map(|param| {
            let param_ty = specialize(param.ty().clone());
            replace_type(ck.sa, param_ty, type_params, self_ty.clone())
        })
        .collect::<Vec<_>>();
    let variadic_type = variadic_param.map(|param| {
        let param_ty = specialize(param.ty());
        replace_type(ck.sa, param_ty, type_params, self_ty.clone())
    });

    ExpectedCallArgs {
        regular_types,
        variadic_type,
    }
}

pub(super) fn check_call_arguments_with_expected(
    ck: &mut TypeCheck,
    call_expr_id: ExprId,
    expected: Option<&ExpectedCallArgs>,
) {
    let Some(expected) = expected else {
        check_call_arguments_any(ck, call_expr_id);
        return;
    };

    // First pass: type check all arguments
    let arg_ids = ck
        .call_args(call_expr_id)
        .iter()
        .map(|arg| arg.expr)
        .collect::<Vec<_>>();

    for (idx, arg_id) in arg_ids.iter().enumerate() {
        let expected_ty = if idx < expected.regular_types.len() {
            expected.regular_types[idx].clone()
        } else if let Some(ref variadic_type) = expected.variadic_type {
            variadic_type.clone()
        } else {
            SourceType::Any
        };

        let ty = check_expr(ck, *arg_id, expected_ty);
        ck.body.set_ty(*arg_id, ty);
    }

    // Second pass: check compatibility (need to re-borrow call_args)
    let call_args = ck.call_args(call_expr_id);

    // Check for unexpected named arguments
    for (idx, arg) in call_args.iter().enumerate() {
        if arg.name.is_some() {
            let span = call_arg_name_span(ck, call_expr_id, idx)
                .unwrap_or_else(|| call_arg_span(ck, call_expr_id, idx));
            ck.report(span, &UNEXPECTED_NAMED_ARGUMENT, args!());
        }
    }

    // Check type compatibility for regular parameters
    let no_regular_params = expected.regular_types.len();
    for (idx, param_ty) in expected
        .regular_types
        .iter()
        .take(call_args.len().min(no_regular_params))
        .enumerate()
    {
        let arg_id = call_args[idx].expr;
        let arg_ty = ck.body.ty(arg_id);

        if !arg_allows(ck.sa, param_ty.clone(), arg_ty.clone(), None) && !arg_ty.is_error() {
            let exp = ck.ty_name(&param_ty);
            let got = ck.ty_name(&arg_ty);

            ck.report(
                call_arg_span(ck, call_expr_id, idx),
                &WRONG_TYPE_FOR_ARGUMENT,
                args!(exp, got),
            );
        }
    }

    // Check argument count
    if call_args.len() < no_regular_params {
        ck.report(
            ck.expr_span(call_expr_id),
            &MISSING_ARGUMENTS,
            args!(no_regular_params, call_args.len()),
        );
    } else if let Some(ref variadic_ty) = expected.variadic_type {
        // Check variadic arguments
        for idx in no_regular_params..call_args.len() {
            let arg_id = call_args[idx].expr;
            let arg_ty = ck.body.ty(arg_id);

            if !arg_allows(ck.sa, variadic_ty.clone(), arg_ty.clone(), None) && !arg_ty.is_error() {
                let exp = ck.ty_name(&variadic_ty);
                let got = ck.ty_name(&arg_ty);

                ck.report(
                    call_arg_span(ck, call_expr_id, idx),
                    &WRONG_TYPE_FOR_ARGUMENT,
                    args!(exp, got),
                );
            }
        }
    } else {
        // Report superfluous arguments
        for idx in no_regular_params..call_args.len() {
            ck.sa.report(
                ck.file_id,
                call_arg_span(ck, call_expr_id, idx),
                &SUPERFLUOUS_ARGUMENT,
                args!(),
            );
        }
    }
}

fn check_expr_call_generic_static_method(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    tp_id: TypeParamId,
    name: String,
    pure_fct_type_params: SourceTypeArray,
    call_expr_id: ExprId,
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
        let desc = if matched_methods.len() > 1 {
            &MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM
        } else {
            &UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM
        };

        ck.report(ck.expr_span(expr_id), desc, args!());

        ck.body.set_ty(expr_id, ty_error());
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
        ck.expr_span(expr_id),
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
        let expected = build_expected_call_args(
            ck,
            trait_method.params.regular_params(),
            trait_method.params.variadic_param(),
            None,
            None,
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
        );
        check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

        let call_type = CallType::GenericStaticMethod(
            tp_id,
            trait_ty.trait_id,
            trait_method_id,
            trait_ty.type_params.clone(),
            pure_fct_type_params,
        );
        ck.body.insert_call_type(expr_id, Rc::new(call_type));

        let return_type = specialize_ty_for_generic(
            ck.sa,
            trait_method.return_type(),
            ck.element,
            tp_id,
            &trait_ty,
            &combined_fct_type_params,
            &tp,
        );

        ck.body.set_ty(expr_id, return_type.clone());

        return_type
    } else {
        check_call_arguments_with_expected(ck, call_expr_id, None);
        SourceType::Error
    }
}

pub(crate) fn check_expr_call_expr(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expr_type: SourceType,
    call_expr_id: ExprId,
) -> SourceType {
    if expr_type.is_error() {
        check_call_arguments_any(ck, call_expr_id);
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    }

    if expr_type.is_lambda() {
        return check_expr_call_expr_lambda(ck, expr_id, expr_type, call_expr_id);
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
            .insert_or_replace_call_type(expr_id, Rc::new(call_type));

        let method = ck.sa.fct(method_id);

        let expected = build_expected_call_args(
            ck,
            method.params.regular_params(),
            method.params.variadic_param(),
            Some(&impl_match.bindings),
            None,
            |ty| ty,
        );
        check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

        let return_type = specialize_type(ck.sa, method.return_type(), &impl_match.bindings);
        ck.body.set_ty(expr_id, return_type.clone());

        return_type
    } else {
        let ty = ck.ty_name(&expr_type);
        ck.report(ck.expr_span(expr_id), &INDEX_GET_NOT_IMPLEMENTED, args!(ty));

        check_call_arguments_any(ck, call_expr_id);
        ck.body.set_ty(expr_id, ty_error());

        ty_error()
    }
}

fn check_expr_call_expr_lambda(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expr_type: SourceType,
    call_expr_id: ExprId,
) -> SourceType {
    let node_id = expr_id;
    let (params, return_type) = expr_type.to_lambda().expect("lambda expected");

    let expected = ExpectedCallArgs {
        regular_types: params.iter().collect(),
        variadic_type: None,
    };
    check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

    let call_type = CallType::Lambda(params, return_type.clone());

    ck.body
        .insert_or_replace_call_type(node_id, Rc::new(call_type));

    ck.body.set_ty(node_id, return_type.clone());
    return_type
}

fn check_expr_call_fct(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    fct_id: FctDefinitionId,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
    let fct = ck.sa.fct(fct_id);

    if !fct_accessible_from(ck.sa, fct_id, ck.module_id) {
        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args!());
    }

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        &ck.type_param_definition,
        fct,
        &type_params,
        ck.file_id,
        ck.expr_span(expr_id),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let expected = type_params_ok.then(|| {
        build_expected_call_args(
            ck,
            fct.params.regular_params(),
            fct.params.variadic_param(),
            Some(&type_params),
            None,
            |ty| ty,
        )
    });
    check_call_arguments_with_expected(ck, call_expr_id, expected.as_ref());

    let ty = if type_params_ok {
        specialize_type(ck.sa, fct.return_type(), &type_params)
    } else {
        ty_error()
    };

    ck.body.set_ty(expr_id, ty.clone());

    let call_type = CallType::Fct(fct_id, type_params.clone());
    ck.body.insert_call_type(expr_id, Rc::new(call_type));

    ty
}

fn check_expr_call_static_method(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
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
            ck.expr_span(expr_id),
            &UNKNOWN_STATIC_METHOD,
            args!(type_name, method_name),
        );
        ck.body.set_ty(expr_id, ty_error());
        ty_error()
    } else if candidates.len() > 1 {
        let type_name = ck.ty_name(&object_type);
        ck.report(
            ck.expr_span(expr_id),
            &MULTIPLE_CANDIDATES_FOR_METHOD,
            args!(type_name, method_name),
        );
        ck.body.set_ty(expr_id, ty_error());
        ty_error()
    } else {
        let candidate = &candidates[0];
        let fct_id = candidate.fct_id;
        let fct = ck.sa.fct(fct_id);

        let full_type_params = candidate.container_type_params.connect(&fct_type_params);

        let type_params_ok = check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            fct,
            &full_type_params,
            ck.file_id,
            ck.expr_span(expr_id),
            |ty| specialize_type(ck.sa, ty, &full_type_params),
        );

        let expected = type_params_ok.then(|| {
            build_expected_call_args(
                ck,
                fct.params.regular_params(),
                fct.params.variadic_param(),
                Some(&full_type_params),
                None,
                |ty| ty,
            )
        });
        check_call_arguments_with_expected(ck, call_expr_id, expected.as_ref());

        let ty = if type_params_ok {
            specialize_type(ck.sa, fct.return_type(), &full_type_params)
        } else {
            ty_error()
        };

        let call_type = Rc::new(CallType::Fct(fct_id, full_type_params));
        ck.body.insert_call_type(expr_id, call_type.clone());

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args!());
        }

        ck.body.set_ty(expr_id, ty.clone());
        ty
    }
}

fn check_expr_call_struct(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    struct_id: StructDefinitionId,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
    let is_struct_accessible = struct_accessible_from(ck.sa, struct_id, ck.module_id);

    if !is_struct_accessible {
        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args!());
    }

    let struct_ = ck.sa.struct_(struct_id);

    if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public(ck.sa)
        && is_struct_accessible
    {
        ck.report(
            ck.expr_span(expr_id),
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
        ck.expr_span(expr_id),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    if !type_params_ok {
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    }

    if struct_.field_name_style.is_named() {
        check_expr_call_ctor_with_named_fields(ck, struct_, type_params.clone(), call_expr_id);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, struct_, type_params.clone(), call_expr_id);
    }

    ck.body.insert_call_type(
        expr_id,
        Rc::new(CallType::NewStruct(struct_id, type_params)),
    );

    ck.body.set_ty(expr_id, ty.clone());
    ty
}

fn check_expr_call_ctor_with_named_fields(
    ck: &mut TypeCheck,
    element_with_fields: &dyn ElementWithFields,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) {
    let mut args_by_name: HashMap<Name, usize> = HashMap::new();

    let single_named_element = compute_single_named_element(ck.sa, element_with_fields);

    let call_args = ck
        .call_args(call_expr_id)
        .iter()
        .enumerate()
        .map(|(idx, arg)| (idx, arg.expr, arg.name))
        .collect::<Vec<_>>();

    for (idx, arg_id, name) in call_args.iter().copied() {
        if let Some(name) = name {
            if args_by_name.contains_key(&name) {
                ck.sa.report(
                    ck.file_id,
                    call_arg_span(ck, call_expr_id, idx),
                    &DUPLICATE_NAMED_ARGUMENT,
                    args!(),
                );
            } else {
                assert!(args_by_name.insert(name, idx).is_none());
            }
        } else if call_args.len() == 1 && single_named_element.is_some() {
            let name = single_named_element.expect("missing name");
            if args_by_name.contains_key(&name) {
                ck.sa.report(
                    ck.file_id,
                    call_arg_span(ck, call_expr_id, idx),
                    &DUPLICATE_NAMED_ARGUMENT,
                    args!(),
                );
            } else {
                assert!(args_by_name.insert(name, idx).is_none());
            }
        } else if let Some(name) = infer_named_argument(ck, arg_id) {
            if args_by_name.contains_key(&name) {
                ck.sa.report(
                    ck.file_id,
                    call_arg_span(ck, call_expr_id, idx),
                    &DUPLICATE_NAMED_ARGUMENT,
                    args!(),
                );
            } else {
                assert!(args_by_name.insert(name, idx).is_none());
            }
        } else {
            ck.report(
                call_arg_span(ck, call_expr_id, idx),
                &UNEXPECTED_POSITIONAL_ARGUMENT,
                args!(),
            );
            let ty = check_expr(ck, arg_id, SourceType::Any);
            ck.body.set_ty(arg_id, ty);
        }
    }

    let call_data = CallSpecializationData {
        object_ty: SourceType::Error,
        type_params,
    };

    for &field_id in element_with_fields.field_ids() {
        let field = ck.sa.field(field_id);
        if let Some(name) = field.name {
            if let Some(arg_index) = args_by_name.remove(&name) {
                let arg_id = call_args[arg_index].1;
                let def_ty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);
                let arg_ty = check_expr(ck, arg_id, def_ty.clone());
                ck.body.set_ty(arg_id, arg_ty.clone());

                if !def_ty.allows(ck.sa, arg_ty.clone()) && !arg_ty.is_error() {
                    let exp = ck.ty_name(&def_ty);
                    let got = ck.ty_name(&arg_ty);

                    ck.report(
                        call_arg_span(ck, call_expr_id, arg_index),
                        &WRONG_TYPE_FOR_ARGUMENT,
                        args!(exp, got),
                    );
                }

                ck.body.insert_argument(arg_id, field.index.to_usize());
            } else {
                let name = ck.sa.interner.str(name).to_string();
                ck.report(
                    ck.expr_span(call_expr_id),
                    &MISSING_NAMED_ARGUMENT,
                    args!(name),
                );
            }
        }
    }

    for (_name, arg_index) in args_by_name {
        ck.sa.report(
            ck.file_id,
            call_arg_span(ck, call_expr_id, arg_index),
            &USE_OF_UNKNOWN_ARGUMENT,
            args!(),
        );
        let arg_id = call_args[arg_index].1;
        let ty = check_expr(ck, arg_id, SourceType::Any);
        ck.body.set_ty(arg_id, ty);
    }
}

fn infer_named_argument(ck: &TypeCheck, arg_id: ExprId) -> Option<Name> {
    match ck.expr(arg_id) {
        Expr::Path(path_expr) => {
            if path_expr.path.len() == 1 && path_expr.path[0].type_params.is_empty() {
                Some(path_expr.path[0].name)
            } else {
                None
            }
        }
        _ => None,
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
    call_expr_id: ExprId,
) -> bool {
    let call_data = CallSpecializationData {
        object_ty: SourceType::Error,
        type_params,
    };

    let fields = element_with_fields.field_ids().len();
    let call_args = ck
        .call_args(call_expr_id)
        .iter()
        .enumerate()
        .map(|(idx, arg)| (idx, arg.expr, arg.name))
        .collect::<Vec<_>>();
    let provided = call_args.len().min(fields);

    for (idx, arg_id, name) in call_args.iter().copied().take(provided) {
        let field_id = element_with_fields.field_ids()[idx];
        let field = ck.sa.field(field_id);
        let def_ty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);
        let arg_ty = check_expr(ck, arg_id, def_ty.clone());
        ck.body.set_ty(arg_id, arg_ty.clone());

        if name.is_some() {
            let span = call_arg_name_span(ck, call_expr_id, idx)
                .unwrap_or_else(|| call_arg_span(ck, call_expr_id, idx));
            ck.report(span, &UNEXPECTED_NAMED_ARGUMENT, args!());
        }

        if !def_ty.allows(ck.sa, arg_ty.clone()) && !arg_ty.is_error() {
            let exp = ck.ty_name(&def_ty);
            let got = ck.ty_name(&arg_ty);

            ck.report(
                call_arg_span(ck, call_expr_id, idx),
                &WRONG_TYPE_FOR_ARGUMENT,
                args!(exp, got),
            );
        }

        ck.body.insert_argument(arg_id, field.index.to_usize());
    }

    if call_args.len() < fields {
        ck.report(
            ck.expr_span(call_expr_id),
            &MISSING_ARGUMENTS,
            args!(fields, call_args.len()),
        );
    } else {
        for idx in fields..call_args.len() {
            ck.report(
                call_arg_span(ck, call_expr_id, idx),
                &SUPERFLUOUS_ARGUMENT,
                args!(),
            );
            let arg_id = call_args[idx].1;
            let ty = check_expr(ck, arg_id, SourceType::Any);
            ck.body.set_ty(arg_id, ty);
        }
    }

    true
}

fn check_expr_call_class(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
    cls_id: ClassDefinitionId,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
    let is_class_accessible = class_accessible_from(ck.sa, cls_id, ck.module_id);

    if !is_class_accessible {
        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args!());
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
        ck.expr_span(expr_id),
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
            ck.expr_span(expr_id),
            &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(cls.name(ck.sa)),
        );
    }

    if cls.field_name_style.is_named() {
        check_expr_call_ctor_with_named_fields(ck, cls, type_params.clone(), call_expr_id);
    } else {
        check_expr_call_ctor_with_unnamed_fields(ck, cls, type_params.clone(), call_expr_id);
    }

    ck.body
        .insert_call_type(expr_id, Rc::new(CallType::NewClass(cls.id(), type_params)));

    ck.body.set_ty(expr_id, cls_ty.clone());
    cls_ty
}

fn check_expr_call_enum_variant(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
    call_expr_id: ExprId,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);
    let variant_id = enum_.variant_id_at(variant_idx as usize);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args!());
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
        ck.expr_span(expr_id),
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    if !type_params_ok {
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    }

    let variant = ck.sa.variant(variant_id);

    if variant.field_ids().is_empty() {
        ck.report(
            ck.expr_span(expr_id),
            &UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT,
            args!(),
        );
    } else {
        if variant.field_name_style.is_named() {
            check_expr_call_ctor_with_named_fields(ck, variant, type_params.clone(), call_expr_id);
        } else {
            check_expr_call_ctor_with_unnamed_fields(
                ck,
                variant,
                type_params.clone(),
                call_expr_id,
            );
        }
    }

    let ty = SourceType::Enum(enum_id, type_params);

    ck.body
        .insert_call_type(expr_id, Rc::new(CallType::NewEnum(ty.clone(), variant_idx)));

    ck.body.set_ty(expr_id, ty.clone());
    ty
}

fn check_expr_call_sym(
    ck: &mut TypeCheck,
    expected_ty: SourceType,
    expr_id: ExprId,
    callee_id: ExprId,
    sym: SymbolKind,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
    match sym {
        SymbolKind::Fct(fct_id) => {
            check_expr_call_fct(ck, expr_id, fct_id, type_params, call_expr_id)
        }

        SymbolKind::Class(cls_id) => {
            check_expr_call_class(ck, expr_id, expected_ty, cls_id, type_params, call_expr_id)
        }

        SymbolKind::Struct(struct_id) => {
            check_expr_call_struct(ck, expr_id, struct_id, type_params, call_expr_id)
        }

        SymbolKind::EnumVariant(enum_id, variant_idx) => check_expr_call_enum_variant(
            ck,
            expr_id,
            expected_ty,
            enum_id,
            type_params,
            variant_idx,
            call_expr_id,
        ),

        _ => {
            let expr_type = check_expr(ck, callee_id, SourceType::Any);
            check_expr_call_expr(ck, expr_id, expr_type, call_expr_id)
        }
    }
}

fn path_expr_last_segment_span(ck: &TypeCheck, callee_id: ExprId) -> Span {
    let expr = ck.syntax::<ast::AstPathExpr>(callee_id);
    expr.segments()
        .last()
        .map(|segment| segment.span())
        .unwrap_or_else(|| ck.expr_span(callee_id))
}

fn check_expr_call_path_name(
    ck: &mut TypeCheck,
    expected_ty: SourceType,
    expr_id: ExprId,
    callee_id: ExprId,
    type_params: SourceTypeArray,
    call_expr_id: ExprId,
) -> SourceType {
    let name_expr = ck
        .body
        .expr(callee_id)
        .to_path()
        .expect("path expr expected");
    let segments = &name_expr.path;

    // Resolve through modules to get the container symbol (all but the last segment)
    let sym = match resolve_path(ck, callee_id, &segments[..segments.len() - 1]) {
        Ok(sym) => sym,
        Err(()) => {
            ck.body.set_ty(expr_id, ty_error());
            return ty_error();
        }
    };

    // The last segment is the method/constructor/variant name
    let last_segment_name = &segments[segments.len() - 1];
    let method_name = ck.sa.interner.str(last_segment_name.name).to_string();
    let interned_method_name = last_segment_name.name;

    // Extract type params from the second-to-last segment (the class/struct/enum segment)
    let container_type_params = if segments.len() >= 2 {
        let container_segment = &segments[segments.len() - 2];
        let params: Vec<SourceType> = container_segment
            .type_params
            .iter()
            .map(|&ty| ck.read_type(ty))
            .collect();
        SourceTypeArray::with(params)
    } else {
        SourceTypeArray::empty()
    };

    match sym {
        SymbolKind::Class(cls_id) => {
            let cls = ck.sa.class(cls_id);
            if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                cls,
                &container_type_params,
                ck.file_id,
                ck.expr_span(expr_id),
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                check_expr_call_static_method(
                    ck,
                    expr_id,
                    SourceType::Class(cls_id, container_type_params),
                    method_name,
                    type_params,
                    call_expr_id,
                )
            } else {
                ty_error()
            }
        }

        SymbolKind::Struct(struct_id) => {
            let struct_ = ck.sa.struct_(struct_id);

            if check_type_params(
                ck.sa,
                ck.element,
                ck.type_param_definition,
                struct_,
                &container_type_params,
                ck.file_id,
                ck.expr_span(expr_id),
                |ty| specialize_type(ck.sa, ty, &container_type_params),
            ) {
                let object_ty = if let Some(ref primitive_ty) = struct_.primitive_ty {
                    assert!(container_type_params.is_empty());
                    primitive_ty.clone()
                } else {
                    SourceType::Struct(struct_id, container_type_params)
                };

                check_expr_call_static_method(
                    ck,
                    expr_id,
                    object_ty,
                    method_name,
                    type_params,
                    call_expr_id,
                )
            } else {
                ty_error()
            }
        }

        SymbolKind::Enum(enum_id) => {
            let enum_ = ck.sa.enum_(enum_id);

            if let Some(&variant_idx) = enum_.name_to_value().get(&interned_method_name) {
                // Check if both enum and variant have type params - that's an error
                if !container_type_params.is_empty() && !type_params.is_empty() {
                    ck.report(
                        path_expr_last_segment_span(ck, callee_id),
                        &NO_TYPE_PARAMS_EXPECTED,
                        args![],
                    );
                }

                // Use container_type_params for enum variant calls like Option[Int]::Some(x)
                let used_type_params = if container_type_params.is_empty() {
                    type_params.clone()
                } else {
                    container_type_params.clone()
                };
                check_expr_call_enum_variant(
                    ck,
                    expr_id,
                    expected_ty,
                    enum_id,
                    used_type_params,
                    variant_idx,
                    call_expr_id,
                )
            } else {
                if check_type_params(
                    ck.sa,
                    ck.element,
                    ck.type_param_definition,
                    enum_,
                    &container_type_params,
                    ck.file_id,
                    ck.expr_span(expr_id),
                    |ty| specialize_type(ck.sa, ty, &container_type_params),
                ) {
                    let object_ty = SourceType::Enum(enum_id, container_type_params);

                    check_expr_call_static_method(
                        ck,
                        expr_id,
                        object_ty,
                        method_name,
                        type_params,
                        call_expr_id,
                    )
                } else {
                    ty_error()
                }
            }
        }

        SymbolKind::TypeParam(id) => check_expr_call_generic_static_method(
            ck,
            expr_id,
            id,
            method_name,
            type_params,
            call_expr_id,
        ),

        SymbolKind::Module(module_id) => {
            let table = ck.sa.module_table(module_id);
            let Some(sym) = table.get(interned_method_name) else {
                let module = ck.sa.module(module_id).name(ck.sa);
                ck.report(
                    ck.expr_span(callee_id),
                    &UNKNOWN_IDENTIFIER_IN_MODULE,
                    args![module, method_name.clone()],
                );
                ck.body.set_ty(expr_id, ty_error());
                return ty_error();
            };

            check_expr_call_sym(
                ck,
                expected_ty,
                expr_id,
                callee_id,
                sym,
                type_params,
                call_expr_id,
            )
        }

        SymbolKind::Alias(alias_id) => {
            let alias_ty = ck.sa.alias(alias_id).ty();
            check_expr_call_static_method(
                ck,
                expr_id,
                alias_ty,
                method_name,
                type_params,
                call_expr_id,
            )
        }

        _ => {
            ck.report(
                ck.expr_span(callee_id),
                &INVALID_LEFT_SIDE_OF_SEPARATOR,
                args![],
            );
            ck.body.set_ty(expr_id, ty_error());
            ty_error()
        }
    }
}

pub(crate) struct ExpectedCallArgs {
    pub regular_types: Vec<SourceType>,
    pub variadic_type: Option<SourceType>,
}

fn arg_allows(sa: &Sema, def: SourceType, arg: SourceType, self_ty: Option<SourceType>) -> bool {
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

#[cfg(test)]
mod tests {
    use crate::error::diagnostics::UNKNOWN_IDENTIFIER;
    use crate::{args, tests::*};

    #[test]
    fn infer_enum_constructor_arg_from_expected() {
        ok("
            enum Maybe[T] { Some(T) }
            fn take(value: Maybe[Int32]) {}
            fn f() { take(Maybe::Some(1)); }
        ");
    }

    #[test]
    fn infer_struct_constructor_arg_from_expected() {
        ok("
            struct Box[T](T)
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](1)); }
        ");
    }

    #[test]
    fn infer_class_constructor_arg_from_expected() {
        ok("
            class Box[T](T)
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](1)); }
        ");
    }

    #[test]
    fn infer_enum_none_in_call_arg() {
        ok("
            fn take(value: Option[Int32]) {}
            fn f() { take(None); }
        ");
    }

    #[test]
    fn infer_struct_constructor_none_arg() {
        ok("
            struct Box[T](Option[T])
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](None)); }
        ");
    }

    #[test]
    fn infer_class_constructor_none_arg() {
        ok("
            class Box[T](Option[T])
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](None)); }
        ");
    }

    #[test]
    fn infer_lambda_call_arg_from_expected() {
        ok("
            fn f(foo: (Int32): Int32): Int32 {
                foo(1)
            }
        ");
    }

    #[test]
    fn infer_lambda_call_none_arg_from_expected() {
        ok("
            fn f(foo: (Option[Int32]): Int32): Int32 {
                foo(None)
            }
        ");
    }

    #[test]
    fn infer_array_index_arg_from_expected() {
        ok("
            fn f(arr: Array[String]): String {
                arr(0)
            }
        ");
    }

    #[test]
    fn call_unknown_function_with_unknown_argument() {
        errors(
            "fn f() { foo(unknown); }",
            vec![
                (
                    (1, 10),
                    3,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_IDENTIFIER,
                    args!("foo"),
                ),
                (
                    (1, 14),
                    7,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_IDENTIFIER,
                    args!("unknown"),
                ),
            ],
        );
    }
}
