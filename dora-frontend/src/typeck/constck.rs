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
    pub fn check_expr(&mut self, expr: &ExprData) -> (SourceType, ConstValue) {
        let expected_type = self.const_.ty();

        let (ty, lit) = if is_lit_int_maybe_minus(expr) {
            compute_lit_int(self.sa, self.const_.file_id, expr, expected_type)
        } else {
            match expr {
                &ExprData::LitChar(ref e) => {
                    let value = check_lit_char(self.sa, self.const_.file_id, e);
                    (SourceType::Char, ConstValue::Char(value))
                }
                &ExprData::LitInt(ref expr) => {
                    let (ty, value) =
                        check_lit_int(self.sa, self.const_.file_id, expr, false, expected_type);

                    (ty, value)
                }
                &ExprData::LitFloat(ref expr) => {
                    let (ty, val) = check_lit_float(self.sa, self.const_.file_id, expr, false);
                    (ty, ConstValue::Float(val))
                }
                &ExprData::LitBool(ref expr) => (SourceType::Bool, ConstValue::Bool(expr.value)),

                &ExprData::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_int() => {
                    let (ty, value) = check_lit_int(
                        self.sa,
                        self.const_.file_id,
                        expr.opnd.to_lit_int().unwrap(),
                        true,
                        expected_type,
                    );

                    (ty, value)
                }

                &ExprData::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_float() => {
                    let (ty, val) = check_lit_float(
                        self.sa,
                        self.const_.file_id,
                        expr.opnd.to_lit_float().unwrap(),
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
}

fn is_lit_int_maybe_minus(e: &ExprData) -> bool {
    if e.is_un_op(UnOp::Neg) {
        let e = e.to_un().expect("unary expected");
        e.opnd.is_lit_int()
    } else {
        e.is_lit_int()
    }
}
