use id_arena::Id;
use smol_str::SmolStr;

use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::type_refs::lower_type;
use crate::sema::{
    ExprArenaBuilder, Name, PatternArenaBuilder, PatternId, Sema, SourceFileId, StmtArenaBuilder,
    StmtId, TypeRef, TypeRefId, lower_pattern, lower_stmt,
};

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

impl Expr {
    pub fn as_bin(&self) -> &BinExpr {
        match self {
            Expr::Bin(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_bin(&self) -> Option<&BinExpr> {
        match self {
            Expr::Bin(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_block(&self) -> &BlockExpr {
        match self {
            Expr::Block(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_block(&self) -> Option<&BlockExpr> {
        match self {
            Expr::Block(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_break(&self) {
        match self {
            Expr::Break => (),
            _ => unreachable!(),
        }
    }

    pub fn to_break(&self) -> Option<()> {
        match self {
            Expr::Break => Some(()),
            _ => None,
        }
    }

    pub fn as_call(&self) -> &CallExpr {
        match self {
            Expr::Call(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_call(&self) -> Option<&CallExpr> {
        match self {
            Expr::Call(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_continue(&self) {
        match self {
            Expr::Continue => (),
            _ => unreachable!(),
        }
    }

    pub fn to_continue(&self) -> Option<()> {
        match self {
            Expr::Continue => Some(()),
            _ => None,
        }
    }

    pub fn as_conv(&self) -> &ConvExpr {
        match self {
            Expr::Conv(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_conv(&self) -> Option<&ConvExpr> {
        match self {
            Expr::Conv(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_dot(&self) -> &DotExpr {
        match self {
            Expr::Dot(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_dot(&self) -> Option<&DotExpr> {
        match self {
            Expr::Dot(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_for(&self) -> &ForExpr {
        match self {
            Expr::For(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_for(&self) -> Option<&ForExpr> {
        match self {
            Expr::For(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_if(&self) -> &IfExpr {
        match self {
            Expr::If(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_if(&self) -> Option<&IfExpr> {
        match self {
            Expr::If(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_is(&self) -> &IsExpr {
        match self {
            Expr::Is(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_is(&self) -> Option<&IsExpr> {
        match self {
            Expr::Is(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> &LambdaExpr {
        match self {
            Expr::Lambda(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_lambda(&self) -> Option<&LambdaExpr> {
        match self {
            Expr::Lambda(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_lit_bool(&self) -> bool {
        match self {
            Expr::LitBool(value) => *value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_bool(&self) -> Option<bool> {
        match self {
            Expr::LitBool(value) => Some(*value),
            _ => None,
        }
    }

    pub fn as_lit_char(&self) -> &String {
        match self {
            Expr::LitChar(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_char(&self) -> Option<&String> {
        match self {
            Expr::LitChar(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_float(&self) -> &String {
        match self {
            Expr::LitFloat(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_float(&self) -> Option<&String> {
        match self {
            Expr::LitFloat(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_int(&self) -> &String {
        match self {
            Expr::LitInt(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_int(&self) -> Option<&String> {
        match self {
            Expr::LitInt(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_lit_str(&self) -> &String {
        match self {
            Expr::LitStr(value) => value,
            _ => unreachable!(),
        }
    }

    pub fn to_lit_str(&self) -> Option<&String> {
        match self {
            Expr::LitStr(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_match(&self) -> &MatchExpr {
        match self {
            Expr::Match(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_match(&self) -> Option<&MatchExpr> {
        match self {
            Expr::Match(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_paren(&self) -> ExprId {
        match self {
            Expr::Paren(expr_id) => *expr_id,
            _ => unreachable!(),
        }
    }

    pub fn to_paren(&self) -> Option<ExprId> {
        match self {
            Expr::Paren(expr_id) => Some(*expr_id),
            _ => None,
        }
    }

    pub fn as_path(&self) -> &PathExpr {
        match self {
            Expr::Path(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_path(&self) -> Option<&PathExpr> {
        match self {
            Expr::Path(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_name(&self) -> &NameExpr {
        match self {
            Expr::Name(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_name(&self) -> Option<&NameExpr> {
        match self {
            Expr::Name(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_method_call(&self) -> &MethodCallExpr {
        match self {
            Expr::MethodCall(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_method_call(&self) -> Option<&MethodCallExpr> {
        match self {
            Expr::MethodCall(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_return(&self) -> &ReturnExpr {
        match self {
            Expr::Return(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_return(&self) -> Option<&ReturnExpr> {
        match self {
            Expr::Return(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_template(&self) -> &TemplateExpr {
        match self {
            Expr::Template(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_template(&self) -> Option<&TemplateExpr> {
        match self {
            Expr::Template(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_this(&self) {
        match self {
            Expr::This => (),
            _ => unreachable!(),
        }
    }

    pub fn to_this(&self) -> Option<()> {
        match self {
            Expr::This => Some(()),
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> &TupleExpr {
        match self {
            Expr::Tuple(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_tuple(&self) -> Option<&TupleExpr> {
        match self {
            Expr::Tuple(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_typed(&self) -> &TypedExpr {
        match self {
            Expr::Typed(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_typed(&self) -> Option<&TypedExpr> {
        match self {
            Expr::Typed(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_un(&self) -> &UnExpr {
        match self {
            Expr::Un(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_un(&self) -> Option<&UnExpr> {
        match self {
            Expr::Un(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_while(&self) -> &WhileExpr {
        match self {
            Expr::While(expr) => expr,
            _ => unreachable!(),
        }
    }

    pub fn to_while(&self) -> Option<&WhileExpr> {
        match self {
            Expr::While(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_error(&self) {
        match self {
            Expr::Error => (),
            _ => unreachable!(),
        }
    }

    pub fn to_error(&self) -> Option<()> {
        match self {
            Expr::Error => Some(()),
            _ => None,
        }
    }
}

pub struct BinExpr {
    pub op: ast::BinOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

pub struct BlockExpr {
    pub stmts: Vec<StmtId>,
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
    pub name: Option<SmolStr>,
}

pub struct ForExpr {
    pub pattern: PatternId,
    pub expr: ExprId,
    pub block: ExprId,
}

pub struct LambdaParam {
    pub pattern: Option<PatternId>,
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
    pub pattern: PatternId,
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
    pub pattern: PatternId,
}

pub struct WhileExpr {
    pub cond: ExprId,
    pub block: ExprId,
}

fn lower_expr_opt(
    sa: &mut Sema,
    expr_arena: &mut ExprArenaBuilder,
    stmt_arena: &mut StmtArenaBuilder,
    pattern_arena: &mut PatternArenaBuilder,
    file_id: SourceFileId,
    node: Option<ast::AstExpr>,
) -> ExprId {
    node.map(|node| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, node))
        .unwrap_or_else(|| expr_arena.alloc_expr(Expr::Error, None, None, None))
}

pub(crate) fn lower_expr(
    sa: &mut Sema,
    expr_arena: &mut ExprArenaBuilder,
    stmt_arena: &mut StmtArenaBuilder,
    pattern_arena: &mut PatternArenaBuilder,
    file_id: SourceFileId,
    node: ast::AstExpr,
) -> ExprId {
    let syntax_node_ptr = node.as_ptr();
    let syntax_node_id = node.as_syntax_node_id();
    let green_id = Some(node.id());
    let expr = match node {
        ast::AstExpr::Bin(node) => Expr::Bin(BinExpr {
            op: node.op(),
            lhs: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.lhs(),
            ),
            rhs: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.rhs(),
            ),
        }),
        ast::AstExpr::Un(node) => Expr::Un(UnExpr {
            op: node.op(),
            expr: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.opnd(),
            ),
        }),
        ast::AstExpr::Break(..) => Expr::Break,
        ast::AstExpr::Continue(..) => Expr::Continue,
        ast::AstExpr::If(node) => Expr::If(IfExpr {
            cond: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.cond(),
            ),
            then_expr: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.then_block(),
            ),
            else_expr: node.else_block().map(|else_block| {
                lower_expr(
                    sa,
                    expr_arena,
                    stmt_arena,
                    pattern_arena,
                    file_id,
                    else_block,
                )
            }),
        }),
        ast::AstExpr::Is(node) => Expr::Is(IsExpr {
            value: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.value(),
            ),
            pattern: lower_pattern(sa, pattern_arena, file_id, node.pattern()),
        }),
        ast::AstExpr::While(node) => Expr::While(WhileExpr {
            cond: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.cond(),
            ),
            block: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.block().into(),
            ),
        }),
        ast::AstExpr::Error(..) => Expr::Error,
        ast::AstExpr::Block(node) => {
            let mut stmts = Vec::new();

            for stmt in node.stmts_without_tail() {
                let stmt_id = lower_stmt(sa, expr_arena, stmt_arena, pattern_arena, file_id, stmt);
                stmts.push(stmt_id);
            }

            let expr = node.tail().map(|stmt| {
                lower_expr(
                    sa,
                    expr_arena,
                    stmt_arena,
                    pattern_arena,
                    file_id,
                    stmt.as_expr_stmt().expr(),
                )
            });
            Expr::Block(BlockExpr { stmts, expr })
        }
        ast::AstExpr::Call(node) => {
            let callee = lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.callee(),
            );
            let args = node
                .arg_list()
                .items()
                .map(|arg| {
                    let name = arg.name().map(|name| sa.interner.intern(name.text()));
                    let expr = lower_expr_opt(
                        sa,
                        expr_arena,
                        stmt_arena,
                        pattern_arena,
                        file_id,
                        arg.expr(),
                    );
                    CallArg { name, expr }
                })
                .collect();
            Expr::Call(CallExpr { callee, args })
        }
        ast::AstExpr::Conv(node) => {
            let object = node
                .object()
                .map(|expr| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, expr));
            let ty = node
                .data_type()
                .map(|ty| lower_type(sa, file_id, ty))
                .unwrap_or_else(|| sa.alloc_type_ref(TypeRef::Error, None));
            Expr::Conv(ConvExpr { object, ty })
        }
        ast::AstExpr::DotExpr(node) => Expr::Dot(DotExpr {
            lhs: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.lhs(),
            ),
            name: node.name().map(|t| t.text().into()),
        }),
        ast::AstExpr::For(node) => Expr::For(ForExpr {
            pattern: lower_pattern(sa, pattern_arena, file_id, node.pattern()),
            expr: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.expr(),
            ),
            block: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                ast::AstExpr::Block(node.block()),
            ),
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
            let expr = Some(lower_expr_opt(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.expr(),
            ));
            let mut arms = Vec::new();

            for arm in node.arms() {
                let pattern = lower_pattern(sa, pattern_arena, file_id, arm.pattern());
                let cond = arm.cond().map(|cond| {
                    lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, cond)
                });
                let value = lower_expr(
                    sa,
                    expr_arena,
                    stmt_arena,
                    pattern_arena,
                    file_id,
                    arm.value(),
                );
                arms.push(MatchArmExpr {
                    pattern,
                    cond,
                    value,
                });
            }

            Expr::Match(MatchExpr { expr, arms })
        }
        ast::AstExpr::Paren(node) => Expr::Paren(lower_expr_opt(
            sa,
            expr_arena,
            stmt_arena,
            pattern_arena,
            file_id,
            node.expr(),
        )),
        ast::AstExpr::Lambda(node) => {
            let mut params = Vec::new();

            for param in node.params() {
                let pattern = param
                    .pattern()
                    .map(|pattern| lower_pattern(sa, pattern_arena, file_id, pattern));
                let ty = param.data_type().map(|ty| lower_type(sa, file_id, ty));
                params.push(LambdaParam {
                    pattern,
                    ty,
                    variadic: param.variadic(),
                });
            }

            let return_ty = node.return_type().map(|ty| lower_type(sa, file_id, ty));
            let block = node.block().map(|block| {
                lower_expr(
                    sa,
                    expr_arena,
                    stmt_arena,
                    pattern_arena,
                    file_id,
                    block.into(),
                )
            });
            Expr::Lambda(LambdaExpr {
                params,
                return_ty,
                block,
            })
        }
        ast::AstExpr::Return(node) => Expr::Return(ReturnExpr {
            expr: node
                .expr()
                .map(|expr| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, expr)),
        }),
        ast::AstExpr::This(..) => Expr::This,
        ast::AstExpr::TypedExpr(node) => {
            let callee = lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.callee(),
            );
            let args = node.args().map(|ty| lower_type(sa, file_id, ty)).collect();
            Expr::Typed(TypedExpr { callee, args })
        }
        ast::AstExpr::MethodCallExpr(node) => {
            let object = lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.object(),
            );
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
                            let expr = lower_expr_opt(
                                sa,
                                expr_arena,
                                stmt_arena,
                                pattern_arena,
                                file_id,
                                arg.expr(),
                            );
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
            lhs: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.lhs(),
            ),
            rhs: lower_expr(
                sa,
                expr_arena,
                stmt_arena,
                pattern_arena,
                file_id,
                node.rhs(),
            ),
        }),
        ast::AstExpr::Template(node) => {
            let parts = node
                .parts()
                .map(|part| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, part))
                .collect();
            Expr::Template(TemplateExpr { parts })
        }
        ast::AstExpr::Tuple(node) => {
            let values = node
                .values()
                .map(|value| lower_expr(sa, expr_arena, stmt_arena, pattern_arena, file_id, value))
                .collect();
            Expr::Tuple(TupleExpr { values })
        }
    };

    expr_arena.alloc_expr(expr, Some(syntax_node_id), Some(syntax_node_ptr), green_id)
}
