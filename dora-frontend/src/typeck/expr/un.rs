use std::sync::Arc;

use dora_parser::ast;

use super::lit::check_expr_lit_int;
use crate::args;
use crate::error::diagnostics::UN_OP_TYPE;
use crate::replace_type;
use crate::sema::{CallType, Expr, ExprId, TraitDefinitionId, UnExpr, find_impl, implements_trait};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::{SourceType, SourceTypeArray, ty::error as ty_error};

pub(super) fn check_expr_un(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &UnExpr,
    expected_ty: SourceType,
) -> SourceType {
    // Check for negated integer literal
    if sema_expr.op == ast::UnOp::Neg {
        if let Expr::LitInt(text) = ck.expr(sema_expr.expr) {
            let expr_type = check_expr_lit_int(ck, sema_expr.expr, text, true, expected_ty);
            ck.body.set_ty(expr_id, expr_type.clone());
            return expr_type;
        }
    }

    let opnd_ty = check_expr(ck, sema_expr.expr, SourceType::Any);

    match sema_expr.op {
        ast::UnOp::Neg => check_expr_un_trait(
            ck,
            expr_id,
            sema_expr.op,
            ck.sa.known.traits.neg(),
            "neg",
            opnd_ty,
        ),
        ast::UnOp::Not => check_expr_un_trait(
            ck,
            expr_id,
            sema_expr.op,
            ck.sa.known.traits.not(),
            "not",
            opnd_ty,
        ),
    }
}

fn check_expr_un_trait(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::UnOp,
    trait_id: TraitDefinitionId,
    trait_method_name: &str,
    ty: SourceType,
) -> SourceType {
    let trait_ty = TraitType::from_trait_id(trait_id);
    let trait_method_name = ck.sa.interner.intern(trait_method_name);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        ty.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let trait_ = ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id)
            .expect("method not found");

        let call_type = CallType::Method(ty.clone(), method_id, SourceTypeArray::empty());
        ck.body
            .insert_or_replace_call_type(expr_id, Arc::new(call_type));

        let method = ck.sa.fct(method_id);

        let return_type = method.return_type();
        ck.body.set_ty(expr_id, return_type.clone());

        return_type
    } else if ty.is_type_param() && implements_trait(ck.sa, ty.clone(), ck.element, trait_ty) {
        let trait_ = &ck.sa.trait_(trait_id);

        let method_id = trait_
            .get_method(trait_method_name, false)
            .expect("method not found");

        let method = ck.sa.fct(method_id);

        let call_type = CallType::GenericMethod(
            ty.type_param_id().expect("type param expected"),
            trait_id,
            method_id,
            SourceTypeArray::empty(),
            SourceTypeArray::empty(),
        );
        ck.body
            .insert_or_replace_call_type(expr_id, Arc::new(call_type));

        let return_type = method.return_type();
        let return_type = replace_type(
            ck.sa,
            return_type,
            Some(&SourceTypeArray::empty()),
            Some(ty.clone()),
        );

        ck.body.set_ty(expr_id, return_type.clone());

        return_type
    } else {
        let ty = ck.ty_name(&ty);
        ck.report(
            ck.expr_span(expr_id),
            &UN_OP_TYPE,
            args![op.as_str().to_string(), ty],
        );

        ck.body.set_ty(expr_id, ty_error());
        ty_error()
    }
}
