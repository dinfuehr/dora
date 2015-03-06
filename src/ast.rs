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
    pub block: Box<StatementType>,
}

impl Function {
    pub fn new(name: String, pos: Position) -> Function {
        Function {
            name: name,
            position: pos,
            params: vec![],
            vars: vec![],
            block: box StatementType::Nop,
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
pub enum StatementType {
    Var(String,DataType,Box<ExprType>),
    While(Box<ExprType>,Box<StatementType>),
    Loop(Box<StatementType>),
    If(Box<ExprType>,Box<StatementType>,Box<StatementType>),
    Expr(Box<ExprType>),
    Block(Vec<Box<StatementType>>),
    Break,
    Continue,
    Return(Box<ExprType>),
    Nop,
}

impl StatementType {
    pub fn empty_block() -> Box<StatementType> {
        box StatementType::Block(vec![])
    }

    pub fn block(expr: ExprType) -> Box<StatementType> {
        let expr = box StatementType::Expr(box expr);
        box StatementType::Block(vec![expr])
    }
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
    fn new(position: Position, data_type: DataType, expr: ExprType) -> Expr {
        Expr { position: position, data_type: data_type, expr: expr }
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

