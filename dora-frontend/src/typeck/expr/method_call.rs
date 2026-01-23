use std::rc::Rc;

use dora_parser::ast;

use super::call::check_expr_call_expr;
use crate::access::{
    class_field_accessible_from, method_accessible_from, struct_field_accessible_from,
};
use crate::args;
use crate::error::diagnostics::{
    MULTIPLE_CANDIDATES_FOR_METHOD, MULTIPLE_CANDIDATES_FOR_TYPE_PARAM, NOT_ACCESSIBLE,
    UNKNOWN_METHOD, UNKNOWN_METHOD_FOR_TYPE_PARAM,
};
use crate::interner::Name;
use crate::sema::{
    CallType, Element, ExprId, FctDefinitionId, IdentType, MethodCallExpr, Param, Sema,
    TraitDefinition, TypeParamId, find_field_in_class,
};
use crate::specialize::replace_type;
use crate::typeck::{TypeCheck, check_expr, check_type_params, find_method_call_candidates};

use super::call::{ExpectedCallArgs, check_call_arguments_with_expected};
use crate::{
    CallSpecializationData, SourceType, SourceTypeArray, TraitType, empty_sta,
    specialize_ty_for_call, specialize_ty_for_generic, specialize_type, ty::error as ty_error,
};

pub(crate) fn check_expr_method_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &MethodCallExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, sema_expr.object, SourceType::Any);
    let method_name = ck.sa.interner.str(sema_expr.name).to_string();

    let type_params: SourceTypeArray = SourceTypeArray::with(
        sema_expr
            .type_params
            .iter()
            .map(|&type_ref_id| ck.read_type(type_ref_id))
            .collect(),
    );

    check_expr_call_method(
        ck,
        // Call node.
        expr_id,
        object_type,
        method_name,
        type_params,
    )
}

#[cfg(test)]
mod tests {
    use crate::error::diagnostics::{UNKNOWN_IDENTIFIER, UNKNOWN_METHOD};
    use crate::{args, tests::*};

    #[test]
    fn infer_method_call_arg_from_expected() {
        ok("class Foo impl Foo { fn take(value: Int32) {} } fn f(foo: Foo) { foo.take(1); }");
    }

    #[test]
    fn infer_method_call_none_arg_from_expected() {
        ok(
            "class Foo impl Foo { fn take(value: Option[Int32]) {} } fn f(foo: Foo) { foo.take(None); }",
        );
    }

    #[test]
    fn infer_trait_object_method_call_arg_from_expected() {
        ok("trait Foo { fn take(value: Int32); } fn f(foo: Foo) { foo.take(1); }");
    }

    #[test]
    fn infer_trait_object_method_call_none_arg_from_expected() {
        ok("trait Foo { fn take(value: Option[Int32]); } fn f(foo: Foo) { foo.take(None); }");
    }

    #[test]
    fn infer_type_param_method_call_arg_from_expected() {
        ok("trait Foo { fn take(value: Int32); } fn f[T: Foo](foo: T) { foo.take(1); }");
    }

    #[test]
    fn infer_type_param_method_call_none_arg_from_expected() {
        ok("trait Foo { fn take(value: Option[Int32]); } fn f[T: Foo](foo: T) { foo.take(None); }");
    }

    #[test]
    fn call_unknown_method_with_unknown_argument() {
        errors(
            "class Foo fn f(foo: Foo) { foo.bar(unknown); }",
            vec![
                (
                    (1, 28),
                    16,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_METHOD,
                    args!("Foo", "bar"),
                ),
                (
                    (1, 36),
                    7,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_IDENTIFIER,
                    args!("unknown"),
                ),
            ],
        );
    }
}

pub(crate) fn check_method_call_arguments(ck: &mut TypeCheck, sema_expr: &MethodCallExpr) {
    for sema_arg in &sema_expr.args {
        let ty = check_expr(ck, sema_arg.expr, SourceType::Any);
        ck.body.set_ty(sema_arg.expr, ty);
    }
}

fn build_expected_method_call_args<S>(
    regular_params: &[Param],
    variadic_param: Option<&Param>,
    mut specialize: S,
) -> ExpectedCallArgs
where
    S: FnMut(SourceType) -> SourceType,
{
    let regular_types = regular_params
        .iter()
        .map(|param| specialize(param.ty().clone()))
        .collect::<Vec<_>>();
    let variadic_type = variadic_param.map(|param| specialize(param.ty()));

    ExpectedCallArgs {
        regular_types,
        variadic_type,
    }
}

fn check_expr_call_method(
    ck: &mut TypeCheck,
    call_expr_id: ExprId,
    object_type: SourceType,
    method_name: String,
    fct_type_params: SourceTypeArray,
) -> SourceType {
    if let SourceType::TypeParam(id) = object_type {
        return check_method_call_on_type_param(
            ck,
            call_expr_id,
            SourceType::TypeParam(id),
            id,
            method_name,
            fct_type_params,
            call_expr_id,
        );
    } else if object_type.is_assoc() {
        assert_eq!(fct_type_params.len(), 0);
        return check_method_call_on_assoc(
            ck,
            call_expr_id,
            method_name,
            object_type,
            call_expr_id,
        );
    } else if object_type.is_self() {
        assert_eq!(fct_type_params.len(), 0);
        return check_method_call_on_self(ck, call_expr_id, method_name, call_expr_id);
    }

    if object_type.is_error() {
        ck.body.set_ty(call_expr_id, ty_error());

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
        check_method_call_is_array_field_access(
            ck,
            call_expr_id,
            object_type,
            method_name,
            fct_type_params,
            call_expr_id,
        )
    } else if candidates.len() > 1 {
        let type_name = ck.ty_name(&object_type);
        ck.report(
            ck.expr_span(call_expr_id),
            &MULTIPLE_CANDIDATES_FOR_METHOD,
            args!(type_name, method_name),
        );
        check_call_arguments_with_expected(ck, call_expr_id, None);
        ck.body.set_ty(call_expr_id, ty_error());
        ty_error()
    } else {
        let candidate = &candidates[0];
        let fct_id = candidate.fct_id;
        let fct = ck.sa.fct(fct_id);

        let full_type_params = candidate.container_type_params.connect(&fct_type_params);

        let call_data = CallSpecializationData {
            object_ty: candidate.object_type.clone(),
            type_params: full_type_params.clone(),
        };

        let type_params_ok = check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            fct,
            &full_type_params,
            ck.file_id,
            || ck.expr_span(call_expr_id),
            |ty| specialize_type(ck.sa, ty, &full_type_params),
        );

        let expected = type_params_ok.then(|| {
            build_expected_method_call_args(
                fct.params.regular_params(),
                fct.params.variadic_param(),
                |ty| specialize_ty_for_call(ck.sa, ty, ck.element, &call_data),
            )
        });
        check_call_arguments_with_expected(ck, call_expr_id, expected.as_ref());

        let ty = if type_params_ok {
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

        ck.body.insert_call_type(call_expr_id, Rc::new(call_type));

        if !method_accessible_from(ck.sa, fct_id, ck.module_id) {
            ck.report(ck.expr_span(call_expr_id), &NOT_ACCESSIBLE, args!());
        }

        ck.body.set_ty(call_expr_id, ty.clone());
        ty
    }
}

fn check_method_call_is_array_field_access(
    ck: &mut TypeCheck,
    call_expr_id: ExprId,
    object_type: SourceType,
    method_name: String,
    _type_params: SourceTypeArray,
    call_expr_id_for_args: ExprId,
) -> SourceType {
    let interned_method_name = ck.sa.interner.intern(&method_name);
    if let SourceType::Class(cls_id, ..) = object_type.clone() {
        if let Some((field_id, field_type)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_method_name)
        {
            ck.body.set_ty(call_expr_id, field_type.clone());
            ck.body.insert_or_replace_ident(
                call_expr_id,
                IdentType::Field(object_type.clone(), field_id),
            );

            if !class_field_accessible_from(ck.sa, cls_id, field_id, ck.module_id) {
                let syntax = ck.syntax::<ast::AstMethodCallExpr>(call_expr_id);
                ck.report(syntax.field_span(), &NOT_ACCESSIBLE, args!());
            }

            return check_expr_call_expr(ck, call_expr_id, field_type, call_expr_id_for_args);
        }
    }

    if let SourceType::Struct(struct_id, struct_type_params) = object_type.clone() {
        let struct_ = ck.sa.struct_(struct_id);
        if let Some(&field_index) = struct_.field_names().get(&interned_method_name) {
            let ident_type = IdentType::StructField(object_type.clone(), field_index);
            ck.body.insert_or_replace_ident(call_expr_id, ident_type);

            let field_id = struct_.field_id(field_index);
            let field = ck.sa.field(field_id);
            let field_type = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

            if !struct_field_accessible_from(ck.sa, struct_id, field_index, ck.module_id) {
                let syntax = ck.syntax::<ast::AstMethodCallExpr>(call_expr_id);
                ck.report(syntax.field_span(), &NOT_ACCESSIBLE, args!());
            }

            ck.body.set_ty(call_expr_id, field_type.clone());
            return check_expr_call_expr(ck, call_expr_id, field_type, call_expr_id_for_args);
        }
    }

    let ty = ck.ty_name(&object_type);
    ck.report(
        ck.expr_span(call_expr_id),
        &UNKNOWN_METHOD,
        args!(ty, method_name),
    );

    check_call_arguments_with_expected(ck, call_expr_id, None);
    ck.body.set_ty(call_expr_id, ty_error());

    ty_error()
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

fn check_method_call_on_self(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    name: String,
    call_expr_id: ExprId,
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

        ck.body.set_ty(expr_id, return_type.clone());

        ck.body.insert_call_type(
            expr_id,
            Rc::new(CallType::GenericMethodSelf(
                trait_method.trait_id(),
                trait_method_id,
                trait_type_params.clone(),
                SourceTypeArray::empty(),
            )),
        );

        let expected = build_expected_method_call_args(
            trait_method.params.regular_params(),
            trait_method.params.variadic_param(),
            |ty| replace_type(ck.sa, ty, Some(&trait_type_params), Some(SourceType::This)),
        );
        check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

        return_type
    } else {
        if matched_methods.is_empty() {
            ck.report(
                ck.expr_span(expr_id),
                &UNKNOWN_METHOD,
                args!("Self".to_string(), name),
            );
        } else {
            ck.report(
                ck.expr_span(expr_id),
                &MULTIPLE_CANDIDATES_FOR_METHOD,
                args!("Self".to_string(), name),
            );
        }
        ck.body.set_ty(expr_id, ty_error());

        ty_error()
    }
}

fn check_method_call_on_assoc(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    name: String,
    object_type: SourceType,
    call_expr_id: ExprId,
) -> SourceType {
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

    // Also check bounds directly on the associated type definition.
    if let SourceType::Assoc { assoc_id, .. } = &object_type {
        let alias = ck.sa.alias(*assoc_id);
        for bound in alias.bounds() {
            if let Some(trait_ty) = bound.ty() {
                let trait_ = ck.sa.trait_(trait_ty.trait_id);
                if let Some(trait_method_id) = trait_.get_method(interned_name, false) {
                    matched_methods.push((trait_method_id, trait_ty));
                }
            }
        }
    }

    if matched_methods.len() == 1 {
        let (trait_method_id, trait_ty) = matched_methods.pop().expect("missing element");
        let trait_type_params = empty_sta();

        let trait_method = ck.sa.fct(trait_method_id);
        let return_type = trait_method.return_type();

        ck.body.set_ty(expr_id, return_type.clone());

        ck.body.insert_call_type(
            expr_id,
            Rc::new(CallType::GenericMethodNew {
                object_type,
                trait_ty,
                fct_id: trait_method_id,
                fct_type_params: SourceTypeArray::empty(),
            }),
        );

        let expected = build_expected_method_call_args(
            trait_method.params.regular_params(),
            trait_method.params.variadic_param(),
            |ty| replace_type(ck.sa, ty, Some(&trait_type_params), Some(SourceType::This)),
        );
        check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

        return_type
    } else {
        let object_type = object_type.name(ck.sa);
        if matched_methods.is_empty() {
            ck.report(
                ck.expr_span(expr_id),
                &UNKNOWN_METHOD,
                args!(object_type, name),
            );
        } else {
            ck.report(
                ck.expr_span(expr_id),
                &MULTIPLE_CANDIDATES_FOR_METHOD,
                args!(object_type, name),
            );
        }
        ck.body.set_ty(expr_id, ty_error());

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

fn check_method_call_on_type_param(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    object_type: SourceType,
    id: TypeParamId,
    name: String,
    pure_fct_type_params: SourceTypeArray,
    call_expr_id: ExprId,
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
        let combined_fct_type_params = trait_ty.type_params.connect(&pure_fct_type_params);

        let type_params_ok = check_type_params(
            ck.sa,
            ck.element,
            &ck.type_param_definition,
            trait_method,
            &combined_fct_type_params,
            ck.file_id,
            || ck.expr_span(expr_id),
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
        );

        if type_params_ok {
            let return_type = specialize_ty_for_generic(
                ck.sa,
                trait_method.return_type(),
                ck.element,
                id,
                &trait_ty,
                &combined_fct_type_params,
                &object_type,
            );

            ck.body.set_ty(expr_id, return_type.clone());

            let call_type = CallType::GenericMethod(
                id,
                trait_method.trait_id(),
                trait_method_id,
                trait_ty.type_params.clone(),
                pure_fct_type_params,
            );
            ck.body.insert_call_type(expr_id, Rc::new(call_type));

            let expected = build_expected_method_call_args(
                trait_method.params.regular_params(),
                trait_method.params.variadic_param(),
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
            );
            check_call_arguments_with_expected(ck, call_expr_id, Some(&expected));

            return_type
        } else {
            check_call_arguments_with_expected(ck, call_expr_id, None);
            SourceType::Error
        }
    } else {
        if matched_methods.is_empty() {
            ck.report(
                ck.expr_span(expr_id),
                &UNKNOWN_METHOD_FOR_TYPE_PARAM,
                args!(),
            );
        } else {
            ck.report(
                ck.expr_span(expr_id),
                &MULTIPLE_CANDIDATES_FOR_TYPE_PARAM,
                args!(),
            );
        }
        ck.body.set_ty(expr_id, ty_error());

        ty_error()
    }
}
