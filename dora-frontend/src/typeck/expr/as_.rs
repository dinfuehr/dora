use dora_parser::ast::{self, SyntaxNodeBase};

use crate::args;
use crate::error::diagnostics::{TRAIT_EXPECTED, TYPE_NOT_IMPLEMENTING_TRAIT};
use crate::sema::{AsExpr, ExprId, implements_trait};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::{SourceType, ty::error as ty_error};

pub(super) fn check_expr_as(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &AsExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = if let Some(object_id) = sema_expr.object {
        let ty = check_expr(ck, object_id, SourceType::Any);
        ck.body.set_ty(object_id, ty.clone());
        ty
    } else {
        SourceType::Error
    };

    let check_type = ck.read_type(sema_expr.ty);
    let type_syntax_id = ck.body.type_refs().syntax_node_id(sema_expr.ty);
    let type_ast = ck
        .sa
        .file(ck.file_id)
        .ast()
        .syntax_by_id::<ast::AstType>(type_syntax_id);
    ck.body.set_ty(type_ast.id(), check_type.clone());

    if check_type.is_trait_object() {
        let implements = implements_trait(
            ck.sa,
            object_type.clone(),
            ck.element,
            TraitType::new_ty(ck.sa, check_type.clone()),
        );

        if !implements {
            let object_type = ck.ty_name(&object_type);
            let check_type_name = ck.ty_name(&check_type);

            ck.report(
                ck.expr_span(expr_id),
                &TYPE_NOT_IMPLEMENTING_TRAIT,
                args![object_type, check_type_name],
            );
        }

        ck.body.set_ty(expr_id, check_type.clone());
        check_type
    } else if !check_type.is_error() {
        let name = ck.ty_name(&check_type);
        ck.report(ck.expr_span(expr_id), &TRAIT_EXPECTED, args!(name));
        let ty = ty_error();
        ck.body.set_ty(expr_id, ty.clone());
        ty
    } else {
        ty_error()
    }
}
