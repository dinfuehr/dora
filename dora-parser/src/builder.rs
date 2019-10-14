use crate::ast::*;

use crate::interner::*;
use crate::lexer::position::{Position, Span};

use crate::parser::NodeIdGenerator;

pub struct Builder<'a> {
    id_generator: &'a NodeIdGenerator,
}

impl<'a> Builder<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator) -> Builder<'a> {
        Builder { id_generator }
    }

    pub fn build_block(&self) -> BuilderBlock<'a> {
        BuilderBlock::new(self.id_generator)
    }

    pub fn build_fct(&self, name: Name) -> BuilderFct<'a> {
        BuilderFct::new(self.id_generator, name)
    }

    pub fn build_this(&self) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprSelf(ExprSelfType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),
        }))
    }

    pub fn build_assign(&self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprBin(ExprBinType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),

            op: BinOp::Assign,
            lhs,
            rhs,
        }))
    }

    pub fn build_dot(&self, object: Box<Expr>, name: Name) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprDot(ExprDotType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),

            object,
            name,
        }))
    }

    pub fn build_ident(&self, name: Name) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprIdent(ExprIdentType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),

            name,
            type_params: None,
        }))
    }
}

pub struct BuilderFct<'a> {
    id_generator: &'a NodeIdGenerator,
    name: Name,
    is_method: bool,
    is_public: bool,
    is_constructor: bool,
    return_type: Option<Type>,
    params: Vec<Param>,
    block: Option<Box<Stmt>>,
}

impl<'a> BuilderFct<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator, name: Name) -> BuilderFct<'a> {
        BuilderFct {
            id_generator,
            name,
            is_method: false,
            is_public: false,
            is_constructor: false,
            return_type: None,
            params: Vec::new(),
            block: None,
        }
    }

    pub fn add_param(&mut self, name: Name, ty: Type) -> &mut BuilderFct<'a> {
        let id = self.id_generator.next();

        let param = Param {
            id,
            idx: self.params.len() as u32,
            name,
            reassignable: false,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            data_type: ty,
        };

        self.params.push(param);
        self
    }

    pub fn is_method(&mut self, value: bool) -> &mut BuilderFct<'a> {
        self.is_method = value;
        self
    }

    pub fn is_public(&mut self, value: bool) -> &mut BuilderFct<'a> {
        self.is_public = value;
        self
    }

    pub fn constructor(&mut self, constructor: bool) -> &mut BuilderFct<'a> {
        self.is_constructor = constructor;
        self
    }

    pub fn block(&mut self, stmt: Box<Stmt>) -> &mut BuilderFct<'a> {
        self.block = Some(stmt);
        self
    }

    pub fn build(self) -> Function {
        Function {
            id: self.id_generator.next(),
            pos: Position::new(1, 1),
            span: Span::invalid(),
            name: self.name,
            method: self.is_method,
            has_open: false,
            has_override: false,
            has_final: false,
            has_optimize: false,
            is_pub: self.is_public,
            is_static: false,
            is_abstract: false,
            internal: false,
            is_constructor: self.is_constructor,
            params: self.params,
            throws: false,
            return_type: self.return_type,
            block: self.block,
            type_params: None,
        }
    }
}

pub struct BuilderBlock<'a> {
    id_generator: &'a NodeIdGenerator,
    stmts: Vec<Box<Stmt>>,
}

impl<'a> BuilderBlock<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator) -> BuilderBlock<'a> {
        BuilderBlock {
            id_generator,
            stmts: Vec::new(),
        }
    }

    pub fn add_stmts(&mut self, mut stmts: Vec<Box<Stmt>>) -> &mut BuilderBlock<'a> {
        self.stmts.append(&mut stmts);
        self
    }

    pub fn add_expr(&mut self, expr: Box<Expr>) -> &mut BuilderBlock<'a> {
        let id = self.id_generator.next();

        let stmt = Box::new(Stmt::StmtExpr(StmtExprType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            expr,
        }));

        self.stmts.push(stmt);
        self
    }

    pub fn build(self) -> Box<Stmt> {
        let id = self.id_generator.next();

        Box::new(Stmt::StmtBlock(StmtBlockType {
            id,
            pos: Position::new(1, 1),
            span: Span::invalid(),
            stmts: self.stmts,
        }))
    }
}
