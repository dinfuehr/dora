use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

use crate::sema::{ConstValue, ExprId, Sema, SourceFileId};
use crate::typeck::{TypeCheck, check_lit_char, check_lit_float, check_lit_int, check_lit_str};
use crate::{SourceType, SourceTypeArray};

pub(super) fn check_expr_lit_int(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    expr: ast::AstLitIntExpr,
    _sema_expr: &String,
    negate: bool,
    expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_int(ck.sa, ck.file_id, expr.clone(), negate, expected_ty);

    ck.body.set_ty(expr.id(), ty.clone());
    ck.body.set_const_value(expr.id(), value);

    ty
}

pub fn compute_lit_int(
    sa: &Sema,
    file_id: SourceFileId,
    expr: ast::AstExpr,
    expected_ty: SourceType,
) -> (SourceType, ConstValue) {
    if expr.is_un_expr() && expr.clone().as_un_expr().op() == ast::UnOp::Neg {
        check_lit_int(
            sa,
            file_id,
            expr.as_un_expr().opnd().as_lit_int_expr(),
            true,
            expected_ty,
        )
    } else {
        check_lit_int(sa, file_id, expr.as_lit_int_expr(), false, expected_ty)
    }
}

pub fn compute_lit_float(sa: &Sema, file_id: SourceFileId, expr: AstExpr) -> (SourceType, f64) {
    if expr.is_un_expr() {
        let expr = expr.as_un_expr();
        assert_eq!(expr.op(), ast::UnOp::Neg);
        check_lit_float(sa, file_id, expr.opnd().as_lit_float_expr(), true)
    } else {
        check_lit_float(sa, file_id, expr.as_lit_float_expr(), false)
    }
}

pub(super) fn check_expr_lit_float(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstLitFloatExpr,
    _sema_expr: &String,
    negate: bool,
    _expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_float(ck.sa, ck.file_id, node.clone(), negate);

    ck.body.set_ty(node.id(), ty.clone());
    ck.body.set_const_value(node.id(), ConstValue::Float(value));

    ty
}

pub(super) fn check_expr_lit_bool(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstLitBoolExpr,
    _sema_expr: &bool,
    _expected_ty: SourceType,
) -> SourceType {
    ck.body.set_ty(node.id(), SourceType::Bool);

    SourceType::Bool
}

pub fn check_expr_lit_char(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstLitCharExpr,
    _sema_expr: &String,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_char(ck.sa, ck.file_id, node.clone());

    ck.body.set_ty(node.id(), SourceType::Char);
    ck.body.set_const_value(node.id(), ConstValue::Char(value));

    SourceType::Char
}

pub(super) fn check_expr_lit_str(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstLitStrExpr,
    _sema_expr: &String,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_str(ck.sa, ck.file_id, node.clone());

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.body.set_ty(node.id(), str_ty.clone());
    ck.body
        .set_const_value(node.id(), ConstValue::String(value));

    str_ty
}
