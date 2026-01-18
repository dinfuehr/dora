use crate::access::{class_field_accessible_from, struct_field_accessible_from};
use crate::args;
use crate::error::diagnostics::{ILLEGAL_TUPLE_INDEX, NOT_ACCESSIBLE, UNKNOWN_FIELD};
use crate::interner::Name;
use crate::sema::{ConstValue, ExprId, FieldExpr, FieldIndex, IdentType, find_field_in_class};
use crate::ty::error as ty_error;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::{CallSpecializationData, SourceType, specialize_ty_for_call};
use dora_parser::Span;
use dora_parser::ast;

pub(super) fn starts_with_digit(name: &str) -> bool {
    name.chars().next().is_some_and(|ch| ch.is_ascii_digit())
}

pub(super) fn parse_field_index(name: &str) -> Option<usize> {
    name.parse().ok()
}

pub(super) fn check_expr_field(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &FieldExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, sema_expr.lhs, SourceType::Any);

    let Some(ref name) = sema_expr.name else {
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    };

    if starts_with_digit(name.as_str()) {
        return check_expr_field_unnamed(ck, expr_id, sema_expr, object_type);
    }

    let interned_name = ck.sa.interner.intern(name.as_str());
    check_expr_field_named(ck, expr_id, object_type, interned_name, move |ck| {
        ck.expr_span(expr_id)
    })
}

pub(super) fn check_expr_field_named(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    object_type: SourceType,
    name: Name,
    error_span: impl FnOnce(&TypeCheck) -> Span,
) -> SourceType {
    let mut error_span = Some(error_span);
    let mut span = || error_span.take().expect("field span already computed")(ck);
    match object_type.clone() {
        SourceType::Error
        | SourceType::Any
        | SourceType::Unit
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Bool
        | SourceType::Ptr
        | SourceType::This
        | SourceType::TraitObject(..)
        | SourceType::Enum(..)
        | SourceType::TypeParam(..)
        | SourceType::Lambda(..)
        | SourceType::Tuple(..)
        | SourceType::Alias(..)
        | SourceType::Assoc { .. }
        | SourceType::GenericAssoc { .. } => {}
        SourceType::Class(cls_id, class_type_params) => {
            if let Some((field_index, _)) = find_field_in_class(ck.sa, object_type.clone(), name) {
                let ident_type = IdentType::Field(object_type.clone(), field_index);
                ck.body.insert_or_replace_ident(expr_id, ident_type);

                let cls = ck.sa.class(cls_id);
                let field_id = cls.field_id(field_index);
                let field = ck.sa.field(field_id);
                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: class_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !class_field_accessible_from(ck.sa, cls_id, field_index, ck.module_id) {
                    ck.report(span(), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                return fty;
            }
        }
        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if let Some(&field_index) = struct_.field_names().get(&name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_index);
                ck.body.insert_or_replace_ident(expr_id, ident_type);

                let field_id = struct_.field_id(field_index);
                let field = &ck.sa.field(field_id);
                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: struct_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !struct_field_accessible_from(ck.sa, struct_id, field_index, ck.module_id) {
                    ck.report(span(), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                return fty;
            }
        }
    }

    // field not found, report error
    if !object_type.is_error() {
        let expr_name = ck.ty_name(&object_type);
        let name = ck.sa.interner.str(name).to_string();
        ck.report(span(), &UNKNOWN_FIELD, args![name, expr_name]);
    }

    ck.body.set_ty(expr_id, ty_error());

    ty_error()
}

fn check_expr_field_unnamed(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &FieldExpr,
    object_type: SourceType,
) -> SourceType {
    let name = match sema_expr.name.as_ref() {
        Some(name) => name.as_str(),
        None => {
            ck.body.set_ty(expr_id, ty_error());
            return ty_error();
        }
    };
    let Some(index) = parse_field_index(name) else {
        let expr_name = ck.ty_name(&object_type);
        ck.report(
            ck.expr_span(expr_id),
            &UNKNOWN_FIELD,
            args![name, expr_name],
        );
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    };
    ck.body
        .set_const_value(expr_id, ConstValue::Int(index as i64));

    match object_type.clone() {
        SourceType::Error
        | SourceType::Any
        | SourceType::Unit
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Bool
        | SourceType::Ptr
        | SourceType::This
        | SourceType::TraitObject(..)
        | SourceType::Enum(..)
        | SourceType::TypeParam(..)
        | SourceType::Lambda(..)
        | SourceType::Alias(..)
        | SourceType::Assoc { .. }
        | SourceType::GenericAssoc { .. } => {
            let name = index.to_string();
            let expr_name = ck.ty_name(&object_type);
            ck.report(
                field_name_span(ck, expr_id),
                &UNKNOWN_FIELD,
                args![name, expr_name],
            );
            SourceType::Error
        }

        SourceType::Class(class_id, class_type_params) => {
            let cls = ck.sa.class(class_id);
            if !cls.field_name_style.is_named() && index < cls.field_ids().len() {
                let field_id = cls.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::Field(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(expr_id, ident_type);

                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: class_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !class_field_accessible_from(ck.sa, class_id, field.index, ck.module_id) {
                    ck.report(field_name_span(ck, expr_id), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(
                    field_name_span(ck, expr_id),
                    &UNKNOWN_FIELD,
                    args![name, expr_name],
                );
                SourceType::Error
            }
        }

        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if !struct_.field_name_style.is_named() && index < struct_.field_ids().len() {
                let field_id = struct_.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::StructField(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(expr_id, ident_type);

                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: struct_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !struct_field_accessible_from(ck.sa, struct_id, field.index, ck.module_id) {
                    ck.report(field_name_span(ck, expr_id), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(
                    field_name_span(ck, expr_id),
                    &UNKNOWN_FIELD,
                    args![name, expr_name],
                );
                SourceType::Error
            }
        }

        SourceType::Tuple(subtypes) => {
            if index >= subtypes.len() {
                ck.report(
                    field_name_span(ck, expr_id),
                    &ILLEGAL_TUPLE_INDEX,
                    args![index, ck.ty_name(&object_type)],
                );

                ck.body.set_ty(expr_id, ty_error());
                return ty_error();
            }

            let ty = subtypes[usize::try_from(index).unwrap()].clone();
            ck.body.set_ty(expr_id, ty.clone());
            ty
        }
    }
}

fn field_name_span(ck: &TypeCheck, expr_id: ExprId) -> Span {
    let expr = ck.syntax::<ast::AstFieldExpr>(expr_id);
    expr.name().expect("missing name").span()
}

#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::UNKNOWN_FIELD;
    use crate::tests::*;

    #[test]
    fn invalid_field_index() {
        err(
            "
            class Foo(Int, Bool)
            fn f(x: Foo): Int {
                x.0usize
            }
        ",
            (4, 17),
            8,
            crate::ErrorLevel::Error,
            &UNKNOWN_FIELD,
            args!("0usize", "Foo"),
        );
    }
}
