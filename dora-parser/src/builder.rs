use std::sync::Arc;

use crate::ast::*;
use crate::interner::*;
use crate::Span;

pub struct Builder;

impl Builder {
    pub fn new() -> Builder {
        Builder
    }

    pub fn build_block(&self) -> BuilderBlock {
        BuilderBlock::new()
    }

    pub fn build_fct(&self, name: Name) -> BuilderFct {
        BuilderFct::new(name)
    }

    pub fn build_initializer_assign(&self, id: NodeId, span: Span, lhs: Expr, rhs: Expr) -> Expr {
        Arc::new(ExprData::Bin(ExprBinType {
            id,
            span,

            op: BinOp::Assign,
            initializer: true,
            lhs,
            rhs,
        }))
    }

    pub fn build_ident(&self, id: NodeId, span: Span, name: Name) -> Expr {
        Arc::new(ExprData::Ident(ExprIdentType { id, span, name }))
    }
}

pub struct BuilderFct {
    name: Name,
    visibility: Visibility,
    is_constructor: bool,
    return_type: Option<Type>,
    params: Vec<Param>,
    block: Option<Expr>,
}

impl<'a> BuilderFct {
    pub fn new(name: Name) -> BuilderFct {
        BuilderFct {
            name,
            visibility: Visibility::Public,
            is_constructor: false,
            return_type: None,
            params: Vec::new(),
            block: None,
        }
    }

    pub fn block(&mut self, block: Expr) -> &mut BuilderFct {
        self.block = Some(block);
        self
    }

    pub fn build(self, id: NodeId, span: Span) -> Function {
        Function {
            id,
            kind: FunctionKind::Function,
            span,
            name: Some(Arc::new(IdentData {
                name: self.name,
                span,
            })),
            is_optimize_immediately: false,
            visibility: self.visibility,
            is_static: false,
            internal: false,
            is_constructor: self.is_constructor,
            is_test: false,
            params: self.params,
            return_type: self.return_type,
            block: self.block,
            type_params: None,
        }
    }
}

pub struct BuilderBlock {
    stmts: Vec<Stmt>,
}

impl<'a> BuilderBlock {
    pub fn new() -> BuilderBlock {
        BuilderBlock { stmts: Vec::new() }
    }

    pub fn add_expr(&mut self, id: NodeId, expr: Expr) -> &mut BuilderBlock {
        let stmt = Arc::new(StmtData::Expr(StmtExprType {
            id,
            span: expr.span(),
            expr,
        }));

        self.stmts.push(stmt);
        self
    }

    pub fn build(self, id: NodeId, span: Span) -> Expr {
        Arc::new(ExprData::Block(ExprBlockType {
            id,
            span,
            stmts: self.stmts,
            expr: None,
        }))
    }
}
