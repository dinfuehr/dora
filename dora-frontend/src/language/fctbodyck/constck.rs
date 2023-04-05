use crate::language::error::msg::ErrorMessage;
use crate::language::fctbodyck::body::{
    check_lit_float, check_lit_int, determine_type_literal_int,
};
use crate::language::sem_analysis::{ConstDefinition, ConstValue, SemAnalysis};
use crate::language::ty::SourceType;

use dora_parser::ast::*;

pub struct ConstCheck<'a> {
    pub sa: &'a SemAnalysis,
    pub const_: &'a ConstDefinition,
}

impl<'a> ConstCheck<'a> {
    pub fn check_expr(&mut self, expr: &Expr) -> (SourceType, ConstValue) {
        let expected_type = self.const_.ty.clone();

        let (ty, lit) = match expr {
            &Expr::LitChar(ref expr) => (SourceType::Char, ConstValue::Char(expr.value)),
            &Expr::LitInt(ref expr) => {
                let (ty, value) =
                    check_lit_int(self.sa, self.const_.file_id, expr, false, expected_type);

                (ty, ConstValue::Int(value))
            }
            &Expr::LitFloat(ref expr) => {
                let (ty, val) = check_lit_float(self.sa, self.const_.file_id, expr, false);
                (ty, ConstValue::Float(val))
            }
            &Expr::LitBool(ref expr) => (SourceType::Bool, ConstValue::Bool(expr.value)),

            &Expr::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_int() => {
                let lit_int = expr.opnd.to_lit_int().unwrap();
                let ty = determine_type_literal_int(lit_int, expected_type.clone());

                if ty == SourceType::UInt8 {
                    let ty = SourceType::UInt8.name(self.sa);
                    let msg = ErrorMessage::UnOpType(expr.op.as_str().into(), ty);
                    self.sa
                        .diag
                        .lock()
                        .report(self.const_.file_id, expr.pos, msg);
                }

                let (ty, value) = check_lit_int(
                    self.sa,
                    self.const_.file_id,
                    expr.opnd.to_lit_int().unwrap(),
                    true,
                    expected_type,
                );

                (ty, ConstValue::Int(value))
            }

            &Expr::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_float() => {
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
                    .report_span(self.const_.file_id, expr.span(), msg);
                return (SourceType::Error, ConstValue::None);
            }
        };

        if !self.const_.ty.allows(self.sa, ty.clone()) {
            let name = self.sa.interner.str(self.const_.name).to_string();
            let const_ty = self.const_.ty.name(self.sa);
            let ty = ty.name(self.sa);
            let msg = ErrorMessage::AssignType(name, const_ty, ty);
            self.sa
                .diag
                .lock()
                .report_span(self.const_.file_id, expr.span(), msg);
        }

        (ty, lit)
    }
}
