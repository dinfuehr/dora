use crate::ast::*;

use crate::interner::*;
use crate::lexer::position::{Position, Span};

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

    pub fn build_initializer_assign(
        &self,
        id: NodeId,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> Box<Expr> {
        Box::new(Expr::Bin(ExprBinType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),

            op: BinOp::Assign,
            initializer: true,
            lhs,
            rhs,
        }))
    }

    pub fn build_ident(&self, id: NodeId, name: Name) -> Box<Expr> {
        Box::new(Expr::Ident(ExprIdentType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),

            name,
            type_params: None,
        }))
    }
}

pub struct BuilderFct {
    name: Name,
    is_method: bool,
    visibility: Visibility,
    is_constructor: bool,
    return_type: Option<Type>,
    params: Vec<Param>,
    block: Option<Box<ExprBlockType>>,
}

impl<'a> BuilderFct {
    pub fn new(name: Name) -> BuilderFct {
        BuilderFct {
            name,
            is_method: false,
            visibility: Visibility::Public,
            is_constructor: false,
            return_type: None,
            params: Vec::new(),
            block: None,
        }
    }

    pub fn block(&mut self, block: Box<ExprBlockType>) -> &mut BuilderFct {
        self.block = Some(block);
        self
    }

    pub fn build(self, id: NodeId) -> Function {
        Function {
            id,
            kind: FunctionKind::Function,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            name: self.name,
            method: self.is_method,
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
    stmts: Vec<Box<Stmt>>,
}

impl<'a> BuilderBlock {
    pub fn new() -> BuilderBlock {
        BuilderBlock { stmts: Vec::new() }
    }

    pub fn add_expr(&mut self, id: NodeId, expr: Box<Expr>) -> &mut BuilderBlock {
        let stmt = Box::new(Stmt::Expr(StmtExprType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            expr,
        }));

        self.stmts.push(stmt);
        self
    }

    pub fn build(self, id: NodeId) -> Box<ExprBlockType> {
        Box::new(ExprBlockType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            stmts: self.stmts,
            expr: None,
        })
    }
}
