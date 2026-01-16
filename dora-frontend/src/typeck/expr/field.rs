use dora_parser::Span;
use dora_parser::TokenKind;
use dora_parser::ast;

use crate::access::{class_field_accessible_from, struct_field_accessible_from};
use crate::args;
use crate::error::diagnostics::{ILLEGAL_TUPLE_INDEX, NOT_ACCESSIBLE, UNKNOWN_FIELD};
use crate::interner::Name;
use crate::sema::{ConstValue, ExprId, FieldExpr, FieldIndex, IdentType, find_field_in_class};
use crate::ty::error as ty_error;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr_id;
use crate::{CallSpecializationData, SourceType, specialize_ty_for_call};

pub(super) fn check_expr_field(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &FieldExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr_id(ck, sema_expr.lhs, SourceType::Any);

    let Some(ref name) = sema_expr.name else {
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    };

    // Load AST to check if field name is an integer literal (for tuple access)
    let node = ck.syntax_by_id::<ast::AstFieldExpr>(expr_id);
    let name_token = node.name().unwrap();

    if name_token.syntax_kind() == TokenKind::INT_LITERAL {
        return check_expr_field_unnamed(ck, expr_id, object_type);
    }

    let interned_name = ck.sa.interner.intern(name.as_str());
    check_expr_field_named(
        ck,
        expr_id,
        ck.expr_span(expr_id),
        object_type,
        interned_name,
    )
}

pub(super) fn check_expr_field_named(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    error_span: Span,
    object_type: SourceType,
    name: Name,
) -> SourceType {
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
                    ck.report(error_span, &NOT_ACCESSIBLE, args![]);
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
                    ck.report(error_span, &NOT_ACCESSIBLE, args![]);
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
        ck.report(error_span, &UNKNOWN_FIELD, args![name, expr_name]);
    }

    ck.body.set_ty(expr_id, ty_error());

    ty_error()
}

fn check_expr_field_unnamed(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    object_type: SourceType,
) -> SourceType {
    let node = ck.syntax_by_id::<ast::AstFieldExpr>(expr_id);
    let field_token = node.name().unwrap();

    let index: usize = field_token.text().parse().unwrap_or(0);
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
            ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);
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
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);
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
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                ck.body.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);
                SourceType::Error
            }
        }

        SourceType::Tuple(subtypes) => {
            if index >= subtypes.len() {
                let op_span = node.dot_token().span();
                ck.report(
                    op_span,
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
