use id_arena::Id;

use dora_parser::ast::{self, SyntaxNodeBase, SyntaxNodePtr};

use crate::sema::type_refs::lower_type;
use crate::sema::{Body, Name, Sema, SourceFileId, TypeRef, TypeRefId};

pub type ExprId = Id<Expr>;

pub enum Expr {
    Bin(BinExpr),
    Block(BlockExpr),
    Break,
    Call(CallExpr),
    Continue,
    Conv(ConvExpr),
    Dot(DotExpr),
    For(ForExpr),
    If(IfExpr),
    Is(IsExpr),
    Lambda(LambdaExpr),
    LitBool(bool),
    LitChar(String),
    LitFloat(String),
    LitInt(String),
    LitStr(String),
    Match(MatchExpr),
    Paren(ExprId),
    Path(PathExpr),
    Name(NameExpr),
    MethodCall(MethodCallExpr),
    Return(ReturnExpr),
    Template(TemplateExpr),
    This,
    Tuple(TupleExpr),
    Typed(TypedExpr),
    Un(UnExpr),
    While(WhileExpr),
    Error,
}

pub struct BinExpr {
    pub op: ast::BinOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

pub struct BlockExpr {
    pub stmts: Vec<ExprId>,
    pub expr: Option<ExprId>,
}

pub struct CallExpr {
    pub callee: ExprId,
    pub args: Vec<CallArg>,
}

pub struct ConvExpr {
    pub object: Option<ExprId>,
    pub ty: TypeRefId,
}

pub struct DotExpr {
    pub lhs: ExprId,
    pub rhs: ExprId,
}

pub struct ForExpr {
    pub pattern: SyntaxNodePtr,
    pub expr: ExprId,
    pub block: ExprId,
}

pub struct LambdaParam {
    pub pattern: Option<SyntaxNodePtr>,
    pub ty: Option<TypeRefId>,
    pub variadic: bool,
}

pub struct LambdaExpr {
    pub params: Vec<LambdaParam>,
    pub return_ty: Option<TypeRefId>,
    pub block: Option<ExprId>,
}

pub struct NameExpr {
    pub name: Name,
}

pub struct MethodCallExpr {
    pub object: ExprId,
    pub name: Name,
    pub type_params: Vec<TypeRefId>,
    pub args: Vec<CallArg>,
}

pub struct PathExpr {
    pub lhs: ExprId,
    pub rhs: ExprId,
}

pub struct ReturnExpr {
    pub expr: Option<ExprId>,
}

pub struct TupleExpr {
    pub values: Vec<ExprId>,
}

pub struct TypedExpr {
    pub callee: ExprId,
    pub args: Vec<TypeRefId>,
}

pub struct MatchExpr {
    pub expr: Option<ExprId>,
    pub arms: Vec<MatchArmExpr>,
}

pub struct MatchArmExpr {
    pub pattern: SyntaxNodePtr,
    pub cond: Option<ExprId>,
    pub value: ExprId,
}

pub struct CallArg {
    pub name: Option<Name>,
    pub expr: ExprId,
}

pub struct TemplateExpr {
    pub parts: Vec<ExprId>,
}

pub struct UnExpr {
    pub op: ast::UnOp,
    pub expr: ExprId,
}

pub struct IfExpr {
    pub cond: ExprId,
    pub then_expr: ExprId,
    pub else_expr: Option<ExprId>,
}

pub struct IsExpr {
    pub value: ExprId,
    pub pattern: SyntaxNodePtr,
}

pub struct WhileExpr {
    pub cond: ExprId,
    pub block: ExprId,
}

fn lower_stmt(
    sa: &mut Sema,
    body: &mut Body,
    file_id: SourceFileId,
    stmt: ast::AstStmt,
) -> Option<ExprId> {
    match stmt {
        ast::AstStmt::ExprStmt(stmt) => Some(lower_expr(sa, body, file_id, stmt.expr())),
        ast::AstStmt::Let(stmt) => stmt.expr().map(|expr| lower_expr(sa, body, file_id, expr)),
        ast::AstStmt::Error(..) => None,
    }
}

fn lower_expr_opt(
    sa: &mut Sema,
    body: &mut Body,
    file_id: SourceFileId,
    node: Option<ast::AstExpr>,
) -> ExprId {
    node.map(|node| lower_expr(sa, body, file_id, node))
        .unwrap_or_else(|| body.alloc_expr(Expr::Error, None, None))
}

#[allow(unused)]
pub(crate) fn lower_expr(
    sa: &mut Sema,
    body: &mut Body,
    file_id: SourceFileId,
    node: ast::AstExpr,
) -> ExprId {
    let syntax_node_ptr = node.as_ptr();
    let green_id = Some(node.id());
    let expr = match node {
        ast::AstExpr::Bin(node) => Expr::Bin(BinExpr {
            op: node.op(),
            lhs: lower_expr(sa, body, file_id, node.lhs()),
            rhs: lower_expr(sa, body, file_id, node.rhs()),
        }),
        ast::AstExpr::Un(node) => Expr::Un(UnExpr {
            op: node.op(),
            expr: lower_expr(sa, body, file_id, node.opnd()),
        }),
        ast::AstExpr::Break(..) => Expr::Break,
        ast::AstExpr::Continue(..) => Expr::Continue,
        ast::AstExpr::If(node) => Expr::If(IfExpr {
            cond: lower_expr(sa, body, file_id, node.cond()),
            then_expr: lower_expr(sa, body, file_id, node.then_block()),
            else_expr: node
                .else_block()
                .map(|else_block| lower_expr(sa, body, file_id, else_block)),
        }),
        ast::AstExpr::Is(node) => Expr::Is(IsExpr {
            value: lower_expr(sa, body, file_id, node.value()),
            pattern: node.pattern().as_ptr(),
        }),
        ast::AstExpr::While(node) => Expr::While(WhileExpr {
            cond: lower_expr(sa, body, file_id, node.cond()),
            block: lower_expr(sa, body, file_id, ast::AstExpr::Block(node.block())),
        }),
        ast::AstExpr::Error(..) => Expr::Error,
        ast::AstExpr::Block(node) => {
            let mut stmts = Vec::new();

            for stmt in node.stmts_without_tail() {
                if let Some(expr) = lower_stmt(sa, body, file_id, stmt) {
                    stmts.push(expr);
                }
            }

            let expr = node
                .tail()
                .map(|stmt| lower_expr(sa, body, file_id, stmt.as_expr_stmt().expr()));
            Expr::Block(BlockExpr { stmts, expr })
        }
        ast::AstExpr::Call(node) => {
            let callee = lower_expr(sa, body, file_id, node.callee());
            let args = node
                .arg_list()
                .items()
                .map(|arg| {
                    let name = arg.name().map(|name| sa.interner.intern(name.text()));
                    let expr = lower_expr_opt(sa, body, file_id, arg.expr());
                    CallArg { name, expr }
                })
                .collect();
            Expr::Call(CallExpr { callee, args })
        }
        ast::AstExpr::Conv(node) => {
            let object = node
                .object()
                .map(|expr| lower_expr(sa, body, file_id, expr));
            let ty = node
                .data_type()
                .map(|ty| lower_type(sa, file_id, ty))
                .unwrap_or_else(|| sa.alloc_type_ref(TypeRef::Error, None));
            Expr::Conv(ConvExpr { object, ty })
        }
        ast::AstExpr::DotExpr(node) => Expr::Dot(DotExpr {
            lhs: lower_expr(sa, body, file_id, node.lhs()),
            rhs: lower_expr(sa, body, file_id, node.rhs()),
        }),
        ast::AstExpr::For(node) => Expr::For(ForExpr {
            pattern: node.pattern().as_ptr(),
            expr: lower_expr(sa, body, file_id, node.expr()),
            block: lower_expr(sa, body, file_id, ast::AstExpr::Block(node.block())),
        }),
        ast::AstExpr::NameExpr(node) => {
            let name = sa.interner.intern(node.token().text());
            Expr::Name(NameExpr { name })
        }
        ast::AstExpr::LitBool(node) => Expr::LitBool(node.value()),
        ast::AstExpr::LitChar(node) => Expr::LitChar(node.token_as_string()),
        ast::AstExpr::LitFloat(node) => Expr::LitFloat(node.token_as_string()),
        ast::AstExpr::LitInt(node) => Expr::LitInt(node.token_as_string()),
        ast::AstExpr::LitStr(node) => Expr::LitStr(node.token_as_string()),
        ast::AstExpr::Match(node) => {
            let expr = Some(lower_expr_opt(sa, body, file_id, node.expr()));
            let mut arms = Vec::new();

            for arm in node.arms() {
                let pattern = arm.pattern().as_ptr();
                let cond = arm.cond().map(|cond| lower_expr(sa, body, file_id, cond));
                let value = lower_expr(sa, body, file_id, arm.value());
                arms.push(MatchArmExpr {
                    pattern,
                    cond,
                    value,
                });
            }

            Expr::Match(MatchExpr { expr, arms })
        }
        ast::AstExpr::Paren(node) => Expr::Paren(lower_expr_opt(sa, body, file_id, node.expr())),
        ast::AstExpr::Lambda(node) => {
            let mut params = Vec::new();

            for param in node.params() {
                let pattern = param.pattern().map(|pattern| pattern.as_ptr());
                let ty = param.data_type().map(|ty| lower_type(sa, file_id, ty));
                let variadic = param.variadic();
                params.push(LambdaParam {
                    pattern,
                    ty,
                    variadic,
                });
            }

            let return_ty = node.return_type().map(|ty| lower_type(sa, file_id, ty));
            let block = node
                .block()
                .map(|block| lower_expr(sa, body, file_id, ast::AstExpr::Block(block)));
            Expr::Lambda(LambdaExpr {
                params,
                return_ty,
                block,
            })
        }
        ast::AstExpr::Return(node) => Expr::Return(ReturnExpr {
            expr: node.expr().map(|expr| lower_expr(sa, body, file_id, expr)),
        }),
        ast::AstExpr::This(..) => Expr::This,
        ast::AstExpr::TypedExpr(node) => {
            let callee = lower_expr(sa, body, file_id, node.callee());
            let args = node.args().map(|ty| lower_type(sa, file_id, ty)).collect();
            Expr::Typed(TypedExpr { callee, args })
        }
        ast::AstExpr::MethodCallExpr(node) => {
            let object = lower_expr(sa, body, file_id, node.object());
            let name = sa.interner.intern(node.name().text());
            let type_params = node
                .type_argument_list()
                .map(|list| {
                    list.items()
                        .map(|arg| {
                            arg.ty()
                                .map(|ty| lower_type(sa, file_id, ty))
                                .unwrap_or_else(|| sa.alloc_type_ref(TypeRef::Error, None))
                        })
                        .collect()
                })
                .unwrap_or_default();
            let args = node
                .arg_list()
                .map(|list| {
                    list.items()
                        .map(|arg| {
                            let name = arg.name().map(|name| sa.interner.intern(name.text()));
                            let expr = lower_expr_opt(sa, body, file_id, arg.expr());
                            CallArg { name, expr }
                        })
                        .collect()
                })
                .unwrap_or_default();
            Expr::MethodCall(MethodCallExpr {
                object,
                name,
                type_params,
                args,
            })
        }
        ast::AstExpr::Path(node) => Expr::Path(PathExpr {
            lhs: lower_expr(sa, body, file_id, node.lhs()),
            rhs: lower_expr(sa, body, file_id, node.rhs()),
        }),
        ast::AstExpr::Template(node) => {
            let parts = node
                .parts()
                .map(|part| lower_expr(sa, body, file_id, part))
                .collect();
            Expr::Template(TemplateExpr { parts })
        }
        ast::AstExpr::Tuple(node) => {
            let values = node
                .values()
                .map(|value| lower_expr(sa, body, file_id, value))
                .collect();
            Expr::Tuple(TupleExpr { values })
        }
    };

    body.alloc_expr(expr, Some(syntax_node_ptr), green_id)
}
