use crate::error::msg::ErrorMessage;
use crate::sema::{ConstDefinition, ConstValue, Sema};
use crate::ty::{self, SourceType};
use crate::typeck::compute_lit_int;
use crate::typeck::function::{check_lit_char, check_lit_float, check_lit_int};

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
                AstExpr::LitChar(expr) => {
                    let value = check_lit_char(self.sa, self.const_.file_id, expr.clone());
                    (SourceType::Char, ConstValue::Char(value))
                }
                AstExpr::LitInt(expr) => {
                    let (ty, value) = check_lit_int(
                        self.sa,
                        self.const_.file_id,
                        expr.id(),
                        false,
                        expected_type,
                    );

                    (ty, value)
                }
                AstExpr::LitFloat(expr) => {
                    let (ty, val) =
                        check_lit_float(self.sa, self.const_.file_id, expr.raw_node(), false);
                    (ty, ConstValue::Float(val))
                }
                AstExpr::LitBool(expr) => (SourceType::Bool, ConstValue::Bool(expr.value())),

                AstExpr::Un(expr) if expr.op() == ast::UnOp::Neg && expr.opnd().is_lit_int() => {
                    let (ty, value) = check_lit_int(
                        self.sa,
                        self.const_.file_id,
                        expr.opnd().id(),
                        true,
                        expected_type,
                    );

                    (ty, value)
                }

                AstExpr::Un(expr) if expr.op() == ast::UnOp::Neg && expr.opnd().is_lit_float() => {
                    let (ty, val) = check_lit_float(
                        self.sa,
                        self.const_.file_id,
                        expr.opnd().as_lit_float().raw_node(),
                        true,
                    );
                    (ty, ConstValue::Float(val))
                }

                _ => {
                    let msg = ErrorMessage::ConstValueExpected;
                    self.sa.report(self.const_.file_id, expr.span(), msg);
                    return (ty::error(), ConstValue::None);
                }
            }
        };

        if !self.const_.ty().allows(self.sa, ty.clone()) {
            let const_ty = self.const_.ty().name(self.sa);
            let ty = ty.name(self.sa);
            let msg = ErrorMessage::AssignType(const_ty, ty);
            self.sa.report(self.const_.file_id, expr.span(), msg);
        }

        (ty, lit)
    }

    fn is_lit_int_maybe_minus(&self, e: AstExpr) -> bool {
        if matches!(e, AstExpr::Un(ref e) if e.op() == ast::UnOp::Neg) {
            e.as_un().opnd().is_lit_int()
        } else {
            e.is_lit_int()
        }
    }
}
