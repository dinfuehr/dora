use crate::args;
use crate::error::diagnostics::{ASSIGN_TYPE, CONST_VALUE_EXPECTED};
use crate::sema::{ConstDefinition, ConstValue, Sema};
use crate::ty::{self, SourceType};
use crate::typeck::compute_lit_int;
use crate::typeck::function::{
    check_lit_char_from_text, check_lit_float_from_text, check_lit_int_from_text,
};

use dora_parser::ast::{self, AstExpr, SyntaxNodeBase};

pub struct ConstCheck<'a> {
    pub sa: &'a Sema,
    pub const_: &'a ConstDefinition,
}

impl<'a> ConstCheck<'a> {
    pub fn check_expr(&mut self, expr: AstExpr) -> (SourceType, ConstValue) {
        let expected_type = self.const_.ty();

        let (ty, lit) = if self.is_lit_int_maybe_minus(expr.clone()) {
            compute_lit_int(self.sa, self.const_.file_id, expr.clone(), expected_type)
        } else {
            match expr.clone() {
                AstExpr::LitCharExpr(ref expr) => {
                    let value = check_lit_char_from_text(
                        self.sa,
                        self.const_.file_id,
                        expr.token().text(),
                        expr.span(),
                    );
                    (SourceType::Char, ConstValue::Char(value))
                }
                AstExpr::LitIntExpr(ref expr) => {
                    let (ty, value) = check_lit_int_from_text(
                        self.sa,
                        self.const_.file_id,
                        expr.token().text(),
                        expr.span(),
                        false,
                        expected_type,
                    );

                    (ty, value)
                }
                AstExpr::LitFloatExpr(ref expr) => {
                    let (ty, val) = check_lit_float_from_text(
                        self.sa,
                        self.const_.file_id,
                        expr.token().text(),
                        expr.span(),
                        false,
                    );
                    (ty, ConstValue::Float(val))
                }
                AstExpr::LitBoolExpr(expr) => (SourceType::Bool, ConstValue::Bool(expr.value())),

                AstExpr::UnExpr(ref expr)
                    if expr.op() == ast::UnOp::Neg && expr.opnd().is_lit_int_expr() =>
                {
                    let lit_expr = expr.opnd().as_lit_int_expr();
                    let (ty, value) = check_lit_int_from_text(
                        self.sa,
                        self.const_.file_id,
                        lit_expr.token().text(),
                        lit_expr.span(),
                        true,
                        expected_type,
                    );

                    (ty, value)
                }

                AstExpr::UnExpr(ref expr)
                    if expr.op() == ast::UnOp::Neg && expr.opnd().is_lit_float_expr() =>
                {
                    let lit_expr = expr.opnd().as_lit_float_expr();
                    let (ty, val) = check_lit_float_from_text(
                        self.sa,
                        self.const_.file_id,
                        lit_expr.token().text(),
                        lit_expr.span(),
                        true,
                    );
                    (ty, ConstValue::Float(val))
                }

                _ => {
                    self.sa.report(
                        self.const_.file_id,
                        expr.span(),
                        &CONST_VALUE_EXPECTED,
                        args!(),
                    );
                    return (ty::error(), ConstValue::None);
                }
            }
        };

        if !self.const_.ty().allows(self.sa, ty.clone()) {
            let const_ty = self.const_.ty().name(self.sa);
            let ty = ty.name(self.sa);
            self.sa.report(
                self.const_.file_id,
                expr.span(),
                &ASSIGN_TYPE,
                args!(const_ty, ty),
            );
        }

        (ty, lit)
    }

    fn is_lit_int_maybe_minus(&self, e: AstExpr) -> bool {
        if matches!(e, AstExpr::UnExpr(ref e) if e.op() == ast::UnOp::Neg) {
            e.as_un_expr().opnd().is_lit_int_expr()
        } else {
            e.is_lit_int_expr()
        }
    }
}
