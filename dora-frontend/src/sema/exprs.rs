use id_arena::Id;

use dora_parser::ast;

use crate::sema::{Sema, SourceFileId};

pub type ExprId = Id<Expr>;

pub enum Expr {
    Bin(BinExpr),
    Un(UnExpr),
    Error,
}

pub struct BinExpr {
    pub op: ast::BinOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

pub struct UnExpr {
    pub op: ast::UnOp,
    pub expr: ExprId,
}

#[allow(unused)]
pub(crate) fn lower_expr(sa: &mut Sema, file_id: SourceFileId, node: ast::AstExpr) -> ExprId {
    let expr = match node {
        ast::AstExpr::Bin(node) => Expr::Bin(BinExpr {
            op: node.op(),
            lhs: lower_expr(sa, file_id, node.lhs()),
            rhs: lower_expr(sa, file_id, node.rhs()),
        }),
        ast::AstExpr::Un(node) => Expr::Un(UnExpr {
            op: node.op(),
            expr: lower_expr(sa, file_id, node.opnd()),
        }),
        ast::AstExpr::Error(..) => Expr::Error,
        ast::AstExpr::Block(..) => unimplemented!(),
        ast::AstExpr::Break(..) => unimplemented!(),
        ast::AstExpr::Call(..) => unimplemented!(),
        ast::AstExpr::Continue(..) => unimplemented!(),
        ast::AstExpr::Conv(..) => unimplemented!(),
        ast::AstExpr::DotExpr(..) => unimplemented!(),
        ast::AstExpr::For(..) => unimplemented!(),
        ast::AstExpr::NameExpr(..) => unimplemented!(),
        ast::AstExpr::If(..) => unimplemented!(),
        ast::AstExpr::Is(..) => unimplemented!(),
        ast::AstExpr::Lambda(..) => unimplemented!(),
        ast::AstExpr::LitBool(..) => unimplemented!(),
        ast::AstExpr::LitChar(..) => unimplemented!(),
        ast::AstExpr::LitFloat(..) => unimplemented!(),
        ast::AstExpr::LitInt(..) => unimplemented!(),
        ast::AstExpr::LitStr(..) => unimplemented!(),
        ast::AstExpr::Match(..) => unimplemented!(),
        ast::AstExpr::MethodCallExpr(..) => unimplemented!(),
        ast::AstExpr::Paren(..) => unimplemented!(),
        ast::AstExpr::Path(..) => unimplemented!(),
        ast::AstExpr::Return(..) => unimplemented!(),
        ast::AstExpr::Template(..) => unimplemented!(),
        ast::AstExpr::This(..) => unimplemented!(),
        ast::AstExpr::Tuple(..) => unimplemented!(),
        ast::AstExpr::TypedExpr(..) => unimplemented!(),
        ast::AstExpr::While(..) => unimplemented!(),
    };

    sa.exprs.alloc(expr)
}
