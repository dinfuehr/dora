use dora_parser::ast::{self, SyntaxNodeBase};

use crate::SourceType;
use crate::sema::{ExprId, TupleExpr, create_tuple};
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;

pub(super) fn check_expr_tuple(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstTupleExpr,
    _sema_expr: &TupleExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let mut subtypes = Vec::new();

    if node.values().count() == 0 {
        ck.body.set_ty(node.id(), SourceType::Unit);
        return SourceType::Unit;
    }

    for value in node.values() {
        let subtype = check_expr(ck, value, SourceType::Any);
        subtypes.push(subtype);
    }

    let ty = create_tuple(ck.sa, subtypes);
    ck.body.set_ty(node.id(), ty.clone());

    ty
}
