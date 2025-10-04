use crate::error::msg::ErrorMessage;
use crate::sema::{ConstDefinition, ConstValue, Sema};
use crate::ty::{self, SourceType};
use crate::typeck::compute_lit_int;
use crate::typeck::function::{check_lit_char, check_lit_float, check_lit_int};

use dora_parser::ast::*;

pub struct ConstCheck<'a> {
    pub sa: &'a Sema,
    pub const_: &'a ConstDefinition,
}

impl<'a> ConstCheck<'a> {
    fn node(&self, id: AstId) -> &'a Ast {
        self.sa.node(self.const_.file_id, id)
    }

    pub fn check_expr(&mut self, expr_id: AstId) -> (SourceType, ConstValue) {
        let expr = self.node(expr_id);
        let expected_type = self.const_.ty();

        let (ty, lit) = if self.is_lit_int_maybe_minus(expr) {
            compute_lit_int(self.sa, self.const_.file_id, expr_id, expected_type)
        } else {
            match expr {
                &Ast::LitChar(ref e) => {
                    let value = check_lit_char(self.sa, self.const_.file_id, e);
                    (SourceType::Char, ConstValue::Char(value))
                }
                &Ast::LitInt(..) => {
                    let (ty, value) =
                        check_lit_int(self.sa, self.const_.file_id, expr_id, false, expected_type);

                    (ty, value)
                }
                &Ast::LitFloat(ref expr) => {
                    let (ty, val) = check_lit_float(self.sa, self.const_.file_id, expr, false);
                    (ty, ConstValue::Float(val))
                }
                &Ast::LitBool(ref expr) => (SourceType::Bool, ConstValue::Bool(expr.value)),

                &Ast::Un(ref expr) if expr.op == UnOp::Neg && self.node(expr.opnd).is_lit_int() => {
                    let (ty, value) =
                        check_lit_int(self.sa, self.const_.file_id, expr.opnd, true, expected_type);

                    (ty, value)
                }

                &Ast::Un(ref expr)
                    if expr.op == UnOp::Neg && self.node(expr.opnd).is_lit_float() =>
                {
                    let (ty, val) = check_lit_float(
                        self.sa,
                        self.const_.file_id,
                        self.node(expr.opnd).as_lit_float(),
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

    fn is_lit_int_maybe_minus(&self, e: &Ast) -> bool {
        if e.is_un_op(UnOp::Neg) {
            let e = e.as_un();
            self.node(e.opnd).is_lit_int()
        } else {
            e.is_lit_int()
        }
    }
}
