use id_arena::Id;

use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::type_refs::lower_type;
use crate::sema::{
    ExprArenaBuilder, ExprId, PatternArenaBuilder, PatternId, Sema, SourceFileId, StmtArenaBuilder,
    TypeRefId,
};
use crate::sema::{lower_expr, lower_pattern};

pub type StmtId = Id<Stmt>;

pub enum Stmt {
    Let(LetStmt),
    Expr(ExprId),
    Error,
}

impl Stmt {
    pub fn as_let(&self) -> &LetStmt {
        match self {
            Stmt::Let(stmt) => stmt,
            _ => unreachable!(),
        }
    }

    pub fn to_let(&self) -> Option<&LetStmt> {
        match self {
            Stmt::Let(stmt) => Some(stmt),
            _ => None,
        }
    }

    pub fn as_expr(&self) -> ExprId {
        match self {
            Stmt::Expr(expr_id) => *expr_id,
            _ => unreachable!(),
        }
    }

    pub fn to_expr(&self) -> Option<ExprId> {
        match self {
            Stmt::Expr(expr_id) => Some(*expr_id),
            _ => None,
        }
    }
}

pub struct LetStmt {
    pub pattern: PatternId,
    pub data_type: Option<TypeRefId>,
    pub expr: Option<ExprId>,
}

pub(crate) fn lower_stmt(
    sa: &mut Sema,
    expr_arena: &mut ExprArenaBuilder,
    stmt_arena: &mut StmtArenaBuilder,
    pattern_arena: &mut PatternArenaBuilder,
    file_id: SourceFileId,
    stmt: ast::AstStmt,
) -> StmtId {
    let syntax_node_ptr = stmt.as_ptr();
    let syntax_node_id = stmt.as_syntax_node_id();
    let green_id = Some(stmt.id());

    let stmt = match stmt {
        ast::AstStmt::ExprStmt(stmt) => Stmt::Expr(lower_expr(
            sa,
            expr_arena,
            stmt_arena,
            pattern_arena,
            file_id,
            stmt.expr(),
        )),
        ast::AstStmt::Let(stmt) => Stmt::Let(LetStmt {
            pattern: lower_pattern(sa, pattern_arena, file_id, stmt.pattern()),
            data_type: stmt.data_type().map(|ty| lower_type(sa, file_id, ty)),
            expr: stmt
                .expr()
                .map(|expr| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, expr)),
        }),
        ast::AstStmt::Error(..) => Stmt::Error,
    };

    stmt_arena.alloc_stmt(stmt, Some(syntax_node_id), Some(syntax_node_ptr), green_id)
}
