use dora_parser::ast::{self, SyntaxNodeBase};

use crate::args;
use crate::error::diagnostics::{TRAIT_EXPECTED, TYPE_NOT_IMPLEMENTING_TRAIT};
use crate::sema::{AsExpr, ExprId, implements_trait};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr_opt;
use crate::{SourceType, ty::error as ty_error};

pub(super) fn check_expr_as(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstAsExpr,
    _sema_expr: &AsExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr_opt(ck, node.object(), SourceType::Any);

    if let Some(object) = node.object() {
        ck.body.set_ty(object.id(), object_type.clone());
    }

    let check_type = ck.read_type_opt(node.data_type());
    if let Some(ref ast) = node.data_type() {
        ck.body.set_ty(ast.id(), check_type.clone());
    }

    if check_type.is_trait_object() {
        let implements = implements_trait(
            ck.sa,
            object_type.clone(),
            ck.element,
            TraitType::new_ty(ck.sa, check_type.clone()),
        );

        if !implements {
            let object_type = ck.ty_name(&object_type);
            let check_type = ck.ty_name(&check_type);

            ck.report(
                node.span(),
                &TYPE_NOT_IMPLEMENTING_TRAIT,
                args![object_type, check_type],
            );
        }

        ck.body.set_ty(node.id(), check_type.clone());
        check_type
    } else if !check_type.is_error() {
        let name = ck.ty_name(&check_type);
        ck.sa
            .report(ck.file_id, node.span(), &TRAIT_EXPECTED, args!(name));
        let ty = ty_error();
        ck.body.set_ty(node.id(), ty.clone());
        ty
    } else {
        ty_error()
    }
}
