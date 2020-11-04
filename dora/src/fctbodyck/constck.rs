use crate::error::msg::SemError;
use crate::fctbodyck::expr::{check_lit_float, check_lit_int};
use crate::ty::SourceType;
use crate::vm::{ConstData, ConstValue, VM};

use dora_parser::ast::*;
use dora_parser::lexer::token::IntSuffix;

pub struct ConstCheck<'a> {
    pub vm: &'a VM,
    pub xconst: &'a ConstData,
    pub negative_expr_id: NodeId,
}

impl<'a> ConstCheck<'a> {
    pub fn check_expr(&mut self, expr: &Expr) -> (SourceType, ConstValue) {
        let (ty, lit) = match expr {
            &Expr::LitChar(ref expr) => (SourceType::Char, ConstValue::Char(expr.value)),
            &Expr::LitInt(ref expr) => {
                let (ty, val) =
                    check_lit_int(self.vm, self.xconst.file_id, expr, false, SourceType::Any);
                (ty, ConstValue::Int(val))
            }
            &Expr::LitFloat(ref expr) => {
                let (ty, val) = check_lit_float(self.vm, self.xconst.file_id, expr, false);
                (ty, ConstValue::Float(val))
            }
            &Expr::LitBool(ref expr) => (SourceType::Bool, ConstValue::Bool(expr.value)),

            &Expr::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_int() => {
                let lit_int = expr.opnd.to_lit_int().unwrap();

                if lit_int.suffix == IntSuffix::UInt8 {
                    let ty = SourceType::UInt8.name(self.vm);
                    let msg = SemError::UnOpType(expr.op.as_str().into(), ty);
                    self.vm
                        .diag
                        .lock()
                        .report(self.xconst.file_id, expr.pos, msg);
                }

                let (ty, val) = check_lit_int(
                    self.vm,
                    self.xconst.file_id,
                    expr.opnd.to_lit_int().unwrap(),
                    true,
                    SourceType::Any,
                );
                (ty, ConstValue::Int(val))
            }

            &Expr::Un(ref expr) if expr.op == UnOp::Neg && expr.opnd.is_lit_float() => {
                let (ty, val) = check_lit_float(
                    self.vm,
                    self.xconst.file_id,
                    expr.opnd.to_lit_float().unwrap(),
                    true,
                );
                (ty, ConstValue::Float(val))
            }

            _ => {
                let msg = SemError::ConstValueExpected;
                self.vm
                    .diag
                    .lock()
                    .report(self.xconst.file_id, expr.pos(), msg);
                return (SourceType::Error, ConstValue::None);
            }
        };

        if !self.xconst.ty.allows(self.vm, ty.clone()) {
            let name = self.vm.interner.str(self.xconst.name).to_string();
            let const_ty = self.xconst.ty.name(self.vm);
            let ty = ty.name(self.vm);
            let msg = SemError::AssignType(name, const_ty, ty);
            self.vm
                .diag
                .lock()
                .report(self.xconst.file_id, expr.pos(), msg);
        }

        (ty, lit)
    }
}
