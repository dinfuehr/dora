use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

use crate::sema::{ConstValue, ExprId, Sema, SourceFileId};
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

pub fn compute_lit_int(
    sa: &Sema,
    file_id: SourceFileId,
    expr: ast::AstExpr,
    expected_ty: SourceType,
) -> (SourceType, ConstValue) {
    if expr.is_un_expr() && expr.clone().as_un_expr().op() == ast::UnOp::Neg {
        let lit_expr = expr.as_un_expr().opnd().as_lit_int_expr();
        check_lit_int_from_text(
            sa,
            file_id,
            lit_expr.token().text(),
            lit_expr.span(),
            true,
            expected_ty,
        )
    } else {
        let lit_expr = expr.as_lit_int_expr();
        check_lit_int_from_text(
            sa,
            file_id,
            lit_expr.token().text(),
            lit_expr.span(),
            false,
            expected_ty,
        )
    }
}

pub fn compute_lit_float(sa: &Sema, file_id: SourceFileId, expr: AstExpr) -> (SourceType, f64) {
    if expr.is_un_expr() {
        let expr = expr.as_un_expr();
        assert_eq!(expr.op(), ast::UnOp::Neg);
        let lit_expr = expr.opnd().as_lit_float_expr();
        check_lit_float_from_text(sa, file_id, lit_expr.token().text(), lit_expr.span(), true)
    } else {
        let lit_expr = expr.as_lit_float_expr();
        check_lit_float_from_text(sa, file_id, lit_expr.token().text(), lit_expr.span(), false)
    }
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
