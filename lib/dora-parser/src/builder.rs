use ast::*;
use ast::Elem::*;
use interner::*;
use lexer::position::Position;

use parser::NodeIdGenerator;

pub struct Builder<'a> {
    id_generator: &'a NodeIdGenerator,
}

impl<'a> Builder<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator) -> Builder<'a> {
        Builder { id_generator: id_generator }
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
                                    id: id,
                                    pos: Position::new(1, 1),
                                }))
    }

    pub fn build_assign(&self, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprAssign(ExprAssignType {
                                      id: id,
                                      pos: Position::new(1, 1),
                                      lhs: lhs,
                                      rhs: rhs,
                                  }))
    }

    pub fn build_field(&self, object: Box<Expr>, name: Name) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprField(ExprFieldType {
                                     id: id,
                                     pos: Position::new(1, 1),
                                     object: object,
                                     name: name,
                                 }))
    }

    pub fn build_ident(&self, name: Name) -> Box<Expr> {
        let id = self.id_generator.next();

        Box::new(Expr::ExprIdent(ExprIdentType {
                                     id: id,
                                     pos: Position::new(1, 1),
                                     name: name,
                                     type_params: None,
                                 }))
    }

    pub fn build_param(&self, idx: u32, name: Name, ty: Type) -> Param {
        let id = self.id_generator.next();

        Param {
            id: id,
            idx: idx,
            name: name,
            reassignable: false,
            pos: Position::new(1, 1),
            data_type: ty,
        }
    }
}

pub struct BuilderFct<'a> {
    id_generator: &'a NodeIdGenerator,
    name: Name,
    is_method: bool,
    is_public: bool,
    ctor: CtorType,
    return_type: Option<Type>,
    params: Vec<Param>,
    block: Option<Box<Stmt>>,
}

impl<'a> BuilderFct<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator, name: Name) -> BuilderFct<'a> {
        BuilderFct {
            id_generator: id_generator,
            name: name,
            is_method: false,
            is_public: false,
            ctor: CtorType::None,
            return_type: None,
            params: Vec::new(),
            block: None,
        }
    }

    pub fn return_type(&mut self, ty: Type) -> &mut BuilderFct<'a> {
        self.return_type = Some(ty);
        self
    }

    pub fn add_param(&mut self, name: Name, ty: Type) -> &mut BuilderFct<'a> {
        let id = self.id_generator.next();

        let param = Param {
            id: id,
            idx: self.params.len() as u32,
            name: name,
            reassignable: false,
            pos: Position::new(1, 1),
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

    pub fn ctor(&mut self, ctor: CtorType) -> &mut BuilderFct<'a> {
        self.ctor = ctor;
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
            name: self.name,
            method: self.is_method,
            has_open: false,
            has_override: false,
            has_final: false,
            is_pub: self.is_public,
            is_static: false,
            is_abstract: false,
            internal: false,
            ctor: self.ctor,
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
            id_generator: id_generator,
            stmts: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.stmts.len()
    }

    pub fn add_stmts(&mut self, mut stmts: Vec<Box<Stmt>>) -> &mut BuilderBlock<'a> {
        self.stmts.append(&mut stmts);
        self
    }

    pub fn add_stmt(&mut self, stmt: Box<Stmt>) -> &mut BuilderBlock<'a> {
        self.stmts.push(stmt);
        self
    }

    pub fn add_expr(&mut self, expr: Box<Expr>) -> &mut BuilderBlock<'a> {
        let id = self.id_generator.next();

        let stmt = Box::new(Stmt::StmtExpr(StmtExprType {
                                    id: id,
                                    pos: Position::new(1, 1),
                                    expr: expr,
                                }));

        self.stmts.push(stmt);
        self
    }

    pub fn build(self) -> Box<Stmt> {
        let id = self.id_generator.next();

        Box::new(Stmt::StmtBlock(StmtBlockType {
                                     id: id,
                                     pos: Position::new(1, 1),
                                     stmts: self.stmts,
                                 }))
    }
}
