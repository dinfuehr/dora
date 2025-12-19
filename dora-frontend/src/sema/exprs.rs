use id_arena::Id;

use dora_parser::ast;

use crate::sema::Sema;

pub type ExprId = Id<Expr>;

pub enum Expr {
    Bin {
        op: ast::UnOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    Un {
        op: ast::UnOp,
        expr: ExprId,
    },
    Error,
}

#[allow(unused)]
pub(crate) fn lower_expr(sa: &mut Sema, file_id: SourceFileId, node: ast::AstExpr) -> ExprId {
    let expr = match node {
        ast::AstExpr::Bin(node) => Expr::Bin {
            op: node.op(),
            lhs: lower_expr(sa, file_id, node.lhs()),
            rhs: lower_expr(sa, file_id, node.rhs()),
        },
        ast::AstExpr::Un(node) => Expr::Un {
            op: node.op(),
            expr: lower_expr(sa, file_id, node.opnd()),
        },
        ast::AstExpr::Error => Expr::Error,
        _ => unimplemented!(),
    };

    sa.exprs.alloc(expr)
}
