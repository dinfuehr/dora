use crate::SourceType;
use crate::sema::{BlockExpr, ExprId};
use crate::typeck::{TypeCheck, check_expr_id, check_stmt_id};

pub(super) fn check_expr_block(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    expr: &BlockExpr,
    _expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    for &stmt_id in &expr.stmts {
        check_stmt_id(ck, stmt_id);
    }

    let ty = if let Some(expr_id) = expr.expr {
        check_expr_id(ck, expr_id, SourceType::Any)
    } else {
        SourceType::Unit
    };

    ck.body.set_ty(_expr_id, ty.clone());
    ck.symtable.pop_level();

    ty
}
