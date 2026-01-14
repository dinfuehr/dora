use dora_parser::ast::{self, SyntaxNodeBase};

use crate::SourceType;
use crate::args;
use crate::error::diagnostics::INVALID_RETURN;
use crate::sema::{ExprId, ReturnExpr};
use crate::typeck::{TypeCheck, check_expr};

pub(crate) fn check_expr_return(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstReturnExpr,
    _sema_expr: &ReturnExpr,
    _expected_ty: SourceType,
) -> SourceType {
    if let Some(ref return_type) = ck.return_type {
        let expected_ty = return_type.clone();

        let expr_type = node
            .expr()
            .map(|expr| check_expr(ck, expr, expected_ty.clone()))
            .unwrap_or(SourceType::Unit);

        ck.check_fct_return_type(expected_ty, node.span(), expr_type);
    } else {
        ck.sa
            .report(ck.file_id, node.span(), &INVALID_RETURN, args!());

        if let Some(expr) = node.expr() {
            check_expr(ck, expr, SourceType::Any);
        }
    }

    SourceType::Unit
}
