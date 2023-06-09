use crate::error::msg::ErrorMessage;
use crate::fctbodyck::body::{check_lit_char, check_lit_float, check_lit_int};
use crate::sema::{ConstDefinition, ConstValue, Sema};
use crate::ty::SourceType;

use dora_parser::ast::*;

pub struct ConstCheck<'a> {
    pub sa: &'a Sema,
    pub const_: &'a ConstDefinition,
}

impl<'a> ConstCheck<'a> {
    pub fn check_expr(&mut self, expr: &ExprData) -> (SourceType, ConstValue) {
        let expected_type = self.const_.ty();

        let (ty, lit) = match expr {
            &ExprData::LitChar(ref e) => {
                let value = check_lit_char(self.sa, self.const_.file_id, e);
                (SourceType::Char, ConstValue::Char(value))
            }
            &ExprData::LitInt(ref expr) => {
                let (ty, value_i64, value_f64) =
                    check_lit_int(self.sa, self.const_.file_id, expr, false, expected_type);

                let value = if ty.is_float() {
                    ConstValue::Float(value_f64)
                } else {
                    ConstValue::Int(value_i64)
                };

                (ty, value)
            }
            &ExprData::LitFloat(ref expr) => {
                let (ty, val) = check_lit_float(self.sa, self.const_.file_id, expr, false);
                (ty, ConstValue::Float(val))
            }
            &ExprData::LitBool(ref expr) => (SourceType::Bool, ConstValue::Bool(expr.value)),

            &ExprData::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_int() => {
                let (ty, value_i64, value_f64) = check_lit_int(
                    self.sa,
                    self.const_.file_id,
                    expr.opnd.to_lit_int().unwrap(),
                    true,
                    expected_type,
                );

                let value = if ty.is_float() {
                    ConstValue::Float(value_f64)
                } else {
                    ConstValue::Int(value_i64)
                };

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
                self.sa
                    .diag
                    .lock()
                    .report(self.const_.file_id, expr.span(), msg);
                return (SourceType::Error, ConstValue::None);
            }
        };

        if !self.const_.ty().allows(self.sa, ty.clone()) {
            let name = self.sa.interner.str(self.const_.name).to_string();
            let const_ty = self.const_.ty().name(self.sa);
            let ty = ty.name(self.sa);
            let msg = ErrorMessage::AssignType(name, const_ty, ty);
            self.sa
                .diag
                .lock()
                .report(self.const_.file_id, expr.span(), msg);
        }

        (ty, lit)
    }
}
