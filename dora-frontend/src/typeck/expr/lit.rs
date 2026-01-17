use crate::sema::{ConstValue, ExprId};
use crate::typeck::{
    TypeCheck, check_lit_char_from_text, check_lit_float_from_text, check_lit_int_from_text,
    check_lit_str_from_text,
};
use crate::{SourceType, SourceTypeArray};

pub(super) fn check_expr_lit_int(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    text: &str,
    negate: bool,
    expected_ty: SourceType,
) -> SourceType {
    let span = ck.expr_span(expr_id);
    let (ty, value) = check_lit_int_from_text(ck.sa, ck.file_id, text, span, negate, expected_ty);

    ck.body.set_ty(expr_id, ty.clone());
    ck.body.set_const_value(expr_id, value);

    ty
}

pub(super) fn check_expr_lit_float(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    text: &str,
    negate: bool,
    _expected_ty: SourceType,
) -> SourceType {
    let span = ck.expr_span(expr_id);
    let (ty, value) = check_lit_float_from_text(ck.sa, ck.file_id, text, span, negate);

    ck.body.set_ty(expr_id, ty.clone());
    ck.body.set_const_value(expr_id, ConstValue::Float(value));

    ty
}

pub(super) fn check_expr_lit_bool(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    _expected_ty: SourceType,
) -> SourceType {
    ck.body.set_ty(expr_id, SourceType::Bool);

    SourceType::Bool
}

pub(super) fn check_expr_lit_char(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    text: &str,
    _expected_ty: SourceType,
) -> SourceType {
    let span = ck.expr_span(expr_id);
    let value = check_lit_char_from_text(ck.sa, ck.file_id, text, span);

    ck.body.set_ty(expr_id, SourceType::Char);
    ck.body.set_const_value(expr_id, ConstValue::Char(value));

    SourceType::Char
}

pub(super) fn check_expr_lit_str(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    text: &str,
    _expected_ty: SourceType,
) -> SourceType {
    let span = ck.expr_span(expr_id);
    let value = check_lit_str_from_text(ck.sa, ck.file_id, text, span);

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.body.set_ty(expr_id, str_ty.clone());
    ck.body.set_const_value(expr_id, ConstValue::String(value));

    str_ty
}
