use std::collections::HashMap;

use data_type::DataType;
use lexer::position::Position;

#[derive(PartialEq,Eq,Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Function {
    pub name: String,
    pub position: Position,
    pub params: Vec<usize>,
    pub vars: Vec<LocalVar>,
    pub block: Box<Statement>,
}

impl Function {
    pub fn new(name: String, pos: Position) -> Function {
        Function {
            name: name,
            position: pos,
            params: vec![],
            vars: vec![],
            block: Statement::new( Position::new(1,1), StatementType::Nop),
        }
    }

    pub fn exists(&self, var: &str) -> bool {
        self.vars.iter().any(|x| x.name.as_slice() == var)
    }

    pub fn add_param(&mut self, var: LocalVar) {
        let ind = self.vars.len();
        self.vars.push(var);
        self.params.push(ind);
    }
}

#[derive(PartialEq,Eq,Debug)]
pub struct LocalVar {
    pub name: String,
    pub position: Position,
    pub data_type: DataType,
}

impl LocalVar {
    pub fn new(name: String, data_type: DataType, position: Position) -> LocalVar {
        LocalVar { name: name, data_type: data_type, position: position }
    }
}

#[derive(PartialEq,Eq,Debug)]
pub struct Statement {
    pub position: Position,
    pub stmt: StatementType
}

impl Statement {
    pub fn new(pos: Position, stmt: StatementType) -> Box<Statement> {
        box Statement { position: pos, stmt: stmt }
    }

    pub fn expr(pos: Position, expr: Box<ExprType>) -> Box<Statement> {
        Statement::new(pos, StatementType::Expr(expr))
    }

    pub fn empty_block(pos: Position) -> Box<Statement> {
        Statement::new(pos, StatementType::Block(vec![]))
    }

    pub fn block(pos: Position, stmt: Box<Statement>) -> Box<Statement> {
        Statement::new(pos, StatementType::Block(vec![stmt]))
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum StatementType {
    Var(String,DataType,Box<ExprType>),
    While(Box<ExprType>,Box<Statement>),
    Loop(Box<Statement>),
    If(Box<ExprType>,Box<Statement>,Option<Box<Statement>>),
    Expr(Box<ExprType>),
    Block(Vec<Box<Statement>>),
    Break,
    Continue,
    Return(Box<ExprType>),
    Nop,
}

#[derive(PartialEq,Eq,Debug)]
pub enum UnOp {
    Plus,
    Neg,
}

#[derive(PartialEq,Eq,Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub struct Expr {
    pub position: Position,
    pub data_type: DataType,
    pub expr: ExprType,
}

impl Expr {
    pub fn new(position: Position, data_type: DataType, expr: ExprType) -> Box<Expr> {
        box Expr { position: position, data_type: data_type, expr: expr }
    }

    pub fn lit_int(position: Position, value: i64) -> Box<ExprType> {
        box ExprType::LitInt(value)
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum ExprType {
    Un(UnOp,Box<ExprType>),
    Bin(BinOp,Box<ExprType>,Box<ExprType>),
    LitInt(i64),
    LitStr(String),
    LitTrue,
    LitFalse,
    Ident(String),
    Assign(Box<ExprType>,Box<ExprType>),
    Call(String,Vec<Box<ExprType>>),
}

