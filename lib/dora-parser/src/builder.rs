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

    pub fn build_block(&self, stmts: Vec<Box<Stmt>>) -> Box<Stmt> {
        let id = self.id_generator.next();

        Box::new(Stmt::StmtBlock(StmtBlockType {
                                     id: id,
                                     pos: Position::new(1, 1),
                                     stmts: stmts,
                                 }))
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

    pub fn build_stmt_expr(&self, expr: Box<Expr>) -> Box<Stmt> {
        let id = self.id_generator.next();

        Box::new(Stmt::StmtExpr(StmtExprType {
                                    id: id,
                                    pos: Position::new(1, 1),
                                    expr: expr,
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
    return_type: Option<Type>,
    params: Vec<Param>,
}

impl<'a> BuilderFct<'a> {
    pub fn new(id_generator: &'a NodeIdGenerator, name: Name) -> BuilderFct<'a> {
        BuilderFct {
            id_generator: id_generator,
            name: name,
            return_type: None,
            params: Vec::new(),
        }
    }

    pub fn return_type(&mut self, ty: Type) {
        self.return_type = Some(ty);
    }

    pub fn add_param(&mut self, name: Name, ty: Type) {
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
    }
}