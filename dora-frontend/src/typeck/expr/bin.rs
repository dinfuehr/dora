use std::sync::Arc;

use dora_parser::ast::{self, SyntaxNodeBase};

use super::is::check_expr_is_raw;
use crate::args;
use crate::error::diagnostics::{
    BIN_OP_TYPE, EXPECTED_IDENTITY_TYPE, TYPES_INCOMPATIBLE, WRONG_TYPE,
};
use crate::flatten_and;
use crate::replace_type;
use crate::sema::{
    BinExpr, CallType, ExprId, Intrinsic, TraitDefinitionId, find_impl, implements_trait,
};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::typeck::function::is_simple_enum;
use crate::{SourceType, SourceTypeArray, ty::error as ty_error};

pub(super) fn check_expr_bin(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &BinExpr,
    _expected_ty: SourceType,
) -> SourceType {
    if sema_expr.op == ast::BinOp::And {
        ck.symtable.push_level();
        check_expr_bin_and(ck, expr_id);
        ck.symtable.pop_level();
        return SourceType::Bool;
    }

    let lhs_type = check_expr(ck, sema_expr.lhs, SourceType::Any);
    let rhs_type = check_expr(ck, sema_expr.rhs, SourceType::Any);

    if lhs_type.is_error() || rhs_type.is_error() {
        ck.body.set_ty(expr_id, ty_error());
        return ty_error();
    }

    match sema_expr.op {
        ast::BinOp::Or | ast::BinOp::And => {
            check_expr_bin_bool(ck, expr_id, sema_expr.op, lhs_type, rhs_type)
        }
        ast::BinOp::Cmp(cmp) => check_expr_bin_cmp(ck, expr_id, cmp, lhs_type, rhs_type),
        ast::BinOp::Add => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.add(),
                "add",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Sub => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.sub(),
                "sub",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Mul => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.mul(),
                "mul",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Div => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.div(),
                "div",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Mod => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.mod_(),
                "modulo",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitOr => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.bit_or(),
                "bitor",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitAnd => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.bit_and(),
                "bitand",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitXor => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.bit_xor(),
                "bitxor",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::ShiftL => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.shl(),
                "shl",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::ArithShiftR => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.sar(),
                "sar",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::LogicalShiftR => {
            check_expr_bin_trait(
                ck,
                expr_id,
                sema_expr.op,
                ck.sa.known.traits.shr(),
                "shr",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
    }
}

pub(super) fn check_expr_bin_and(ck: &mut TypeCheck, expr_id: ExprId) -> SourceType {
    // Load AST for flatten_and which still uses AST
    let node = ck.syntax_by_id::<ast::AstBinExpr>(expr_id);
    let conditions = flatten_and(node);

    for cond in conditions.into_iter() {
        let cond_expr_id = ck.expr_id(cond.id());
        if cond.is_is_expr() {
            let cond_sema = ck.expr(cond_expr_id).as_is();
            check_expr_is_raw(ck, cond_sema, SourceType::Bool);
        } else {
            let cond_ty = check_expr(ck, cond_expr_id, SourceType::Bool);
            if !cond_ty.is_bool() && !cond_ty.is_error() {
                let cond_ty = cond_ty.name(ck.sa);
                ck.report(
                    ck.expr_span(cond_expr_id),
                    &WRONG_TYPE,
                    args!["Bool".to_string(), cond_ty],
                );
            }
        }
    }

    ck.body.set_ty(expr_id, SourceType::Bool);
    SourceType::Bool
}

fn check_expr_bin_bool(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    check_type(ck, expr_id, op, lhs_type, rhs_type, SourceType::Bool);
    ck.body.set_ty(expr_id, SourceType::Bool);

    SourceType::Bool
}

pub(super) struct OpTraitInfo {
    pub(super) rhs_type: SourceType,
    pub(super) return_type: SourceType,
}

fn check_expr_bin_trait(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::BinOp,
    trait_id: TraitDefinitionId,
    trait_method_name: &str,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> OpTraitInfo {
    let trait_ty = TraitType::from_trait_id(trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        lhs_type.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );
    let trait_method_name = ck.sa.interner.intern(trait_method_name);

    if let Some(impl_match) = impl_match {
        let type_params = impl_match.bindings;

        let trait_ = &ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id);

        if let Some(method_id) = method_id {
            let call_type = CallType::Method(lhs_type.clone(), method_id, type_params.clone());
            ck.body
                .insert_or_replace_call_type(expr_id, Arc::new(call_type));

            let method = ck.sa.fct(method_id);
            let params = method.params_without_self();

            assert_eq!(params.len(), 1);

            let param = params[0].ty();
            let param = replace_type(ck.sa, param, Some(&type_params), None);

            if !param.allows(ck.sa, rhs_type.clone())
                && !lhs_type.is_error()
                && !rhs_type.is_error()
            {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.report(
                    ck.expr_span(expr_id),
                    &BIN_OP_TYPE,
                    args![op.as_str().to_string(), lhs_type, rhs_type],
                );
            }

            let return_type = method.return_type();
            ck.body.set_ty(expr_id, return_type.clone());

            OpTraitInfo {
                rhs_type,
                return_type,
            }
        } else {
            ck.body.set_ty(expr_id, ty_error());
            OpTraitInfo {
                rhs_type: ty_error(),
                return_type: ty_error(),
            }
        }
    } else if lhs_type.is_type_param()
        && implements_trait(ck.sa, lhs_type.clone(), ck.element, trait_ty)
    {
        let trait_ = ck.sa.trait_(trait_id);

        let method_id = trait_
            .get_method(trait_method_name, false)
            .expect("method not found");

        let method = ck.sa.fct(method_id);
        let params = method.params_without_self();

        let call_type = CallType::GenericMethod(
            lhs_type.type_param_id().expect("type param expected"),
            trait_id,
            method_id,
            SourceTypeArray::empty(),
            SourceTypeArray::empty(),
        );
        ck.body
            .insert_or_replace_call_type(expr_id, Arc::new(call_type));

        let param = params[0].ty();
        let param = replace_type(
            ck.sa,
            param,
            Some(&SourceTypeArray::empty()),
            Some(lhs_type.clone()),
        );

        if !param.allows(ck.sa, rhs_type.clone()) {
            let lhs_type = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            ck.report(
                ck.expr_span(expr_id),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type, rhs_type],
            );
        }

        let return_type = method.return_type();
        let return_type = replace_type(
            ck.sa,
            return_type,
            Some(&SourceTypeArray::empty()),
            Some(lhs_type.clone()),
        );

        ck.body.set_ty(expr_id, return_type.clone());

        OpTraitInfo {
            rhs_type,
            return_type,
        }
    } else {
        if !lhs_type.is_error() && !rhs_type.is_error() {
            let lhs_type = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            ck.report(
                ck.expr_span(expr_id),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type, rhs_type],
            );
        }

        ck.body.set_ty(expr_id, ty_error());

        OpTraitInfo {
            rhs_type: ty_error(),
            return_type: ty_error(),
        }
    }
}

fn check_expr_bin_cmp(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    cmp: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    match cmp {
        ast::CmpOp::Is | ast::CmpOp::IsNot => {
            if lhs_type != rhs_type {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.report(
                    ck.expr_span(expr_id),
                    &TYPES_INCOMPATIBLE,
                    args![lhs_type, rhs_type],
                );
            } else if !lhs_type.is_class() && !lhs_type.is_lambda() && !lhs_type.is_trait_object() {
                let lhs_type = ck.ty_name(&lhs_type);
                ck.report(
                    ck.expr_span(expr_id),
                    &EXPECTED_IDENTITY_TYPE,
                    args![lhs_type],
                );
            }

            ck.body.set_ty(expr_id, SourceType::Bool);
            return SourceType::Bool;
        }

        ast::CmpOp::Eq | ast::CmpOp::Ne => {
            if is_simple_enum(ck.sa, lhs_type.clone()) {
                check_expr_cmp_enum(ck, expr_id, cmp, lhs_type, rhs_type)
            } else {
                check_expr_bin_trait(
                    ck,
                    expr_id,
                    ast::BinOp::Cmp(cmp),
                    ck.sa.known.traits.equals(),
                    "equals",
                    lhs_type,
                    rhs_type,
                );
            }
        }

        ast::CmpOp::Ge | ast::CmpOp::Gt | ast::CmpOp::Le | ast::CmpOp::Lt => {
            check_expr_bin_trait(
                ck,
                expr_id,
                ast::BinOp::Cmp(cmp),
                ck.sa.known.traits.comparable(),
                "cmp",
                lhs_type,
                rhs_type,
            );
        }
    }

    ck.body.set_ty(expr_id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_cmp_enum(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) {
    if lhs_type.allows(ck.sa, rhs_type.clone()) {
        let intrinsic = match op {
            ast::CmpOp::Eq => Intrinsic::EnumEq,
            ast::CmpOp::Ne => Intrinsic::EnumNe,
            _ => unreachable!(),
        };
        let call_type = CallType::Intrinsic(intrinsic);
        ck.body
            .insert_or_replace_call_type(expr_id, Arc::new(call_type));

        ck.body.set_ty(expr_id, SourceType::Bool);
    } else {
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        ck.report(
            ck.expr_span(expr_id),
            &BIN_OP_TYPE,
            args!["equals".to_string(), lhs_type, rhs_type],
        );

        ck.body.set_ty(expr_id, ty_error());
    }
}

fn check_type(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
    expected_type: SourceType,
) {
    if !expected_type.allows(ck.sa, lhs_type.clone())
        || !expected_type.allows(ck.sa, rhs_type.clone())
    {
        let op = op.as_str().to_string();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        ck.report(
            ck.expr_span(expr_id),
            &BIN_OP_TYPE,
            args![op, lhs_type, rhs_type],
        );
    }
}
