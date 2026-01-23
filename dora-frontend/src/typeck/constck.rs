use super::{check_lit_char_from_text, check_lit_float_from_text, check_lit_int_from_text};
use crate::args;
use crate::error::diagnostics::{ASSIGN_TYPE, CONST_VALUE_EXPECTED};
use crate::sema::{Body, ConstDefinition, ConstValue, Expr, ExprId, Sema};
use crate::ty::{self, SourceType};

use dora_parser::Span;
use dora_parser::ast;
use dora_parser::ast::SyntaxNode;

pub struct ConstCheck<'a> {
    pub sa: &'a Sema,
    pub const_: &'a ConstDefinition,
    pub body: &'a Body,
}

impl<'a> ConstCheck<'a> {
    fn expr_span(&self, id: ExprId) -> Span {
        let ptr = self.body.exprs().syntax_node_ptr(id);
        let node = self.sa.syntax::<SyntaxNode>(self.const_.file_id, ptr);
        node.span()
    }

    pub fn check_expr(&mut self, expr_id: ExprId) -> (SourceType, ConstValue) {
        let expected_type = self.const_.ty();
        let expr = self.body.expr(expr_id);

        let (ty, lit) = match expr {
            Expr::LitChar(text) => {
                let span = self.expr_span(expr_id);
                let value = check_lit_char_from_text(self.sa, self.const_.file_id, text, span);
                (SourceType::Char, ConstValue::Char(value))
            }
            Expr::LitInt(text) => {
                let span = self.expr_span(expr_id);
                check_lit_int_from_text(
                    self.sa,
                    self.const_.file_id,
                    text,
                    span,
                    false,
                    expected_type,
                )
            }
            Expr::LitFloat(text) => {
                let span = self.expr_span(expr_id);
                let (ty, val) =
                    check_lit_float_from_text(self.sa, self.const_.file_id, text, span, false);
                (ty, ConstValue::Float(val))
            }
            Expr::LitBool(value) => (SourceType::Bool, ConstValue::Bool(*value)),

            Expr::Un(un_expr) if un_expr.op == ast::UnOp::Neg => {
                let inner_expr = self.body.expr(un_expr.expr);
                match inner_expr {
                    Expr::LitInt(text) => {
                        let span = self.expr_span(un_expr.expr);
                        check_lit_int_from_text(
                            self.sa,
                            self.const_.file_id,
                            text,
                            span,
                            true,
                            expected_type,
                        )
                    }
                    Expr::LitFloat(text) => {
                        let span = self.expr_span(un_expr.expr);
                        let (ty, val) = check_lit_float_from_text(
                            self.sa,
                            self.const_.file_id,
                            text,
                            span,
                            true,
                        );
                        (ty, ConstValue::Float(val))
                    }
                    _ => {
                        let span = self.expr_span(expr_id);
                        self.sa
                            .report(self.const_.file_id, span, &CONST_VALUE_EXPECTED, args!());
                        return (ty::error(), ConstValue::None);
                    }
                }
            }

            _ => {
                let span = self.expr_span(expr_id);
                self.sa
                    .report(self.const_.file_id, span, &CONST_VALUE_EXPECTED, args!());
                return (ty::error(), ConstValue::None);
            }
        };

        if !self.const_.ty().allows(self.sa, ty.clone()) {
            let const_ty = self.const_.ty().name(self.sa);
            let ty = ty.name(self.sa);
            let span = self.expr_span(expr_id);
            self.sa
                .report(self.const_.file_id, span, &ASSIGN_TYPE, args!(const_ty, ty));
        }

        (ty, lit)
    }
}
