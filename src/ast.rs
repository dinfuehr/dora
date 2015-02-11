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
    pub params: Vec<Param>,
    pub block: Box<Statement>,
}

#[derive(PartialEq,Eq,Debug)]
pub enum Statement {
    Var(String,DataType,Box<Expr>),
    While(Box<Expr>,Box<Statement>),
    Loop(Box<Statement>),
    If(Box<Expr>,Box<Statement>,Box<Statement>),
    Expr(Box<Expr>),
    Block(Vec<Box<Statement>>),
    Break,
    Continue,
    Return(Box<Expr>),
}

impl Statement {
    pub fn empty_block() -> Box<Statement> {
        box Statement::Block(vec![])
    }

    pub fn block(expr: Expr) -> Box<Statement> {
        let expr = box Statement::Expr(box expr);
        box Statement::Block(vec![expr])
    }
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: String,
    pub position: Position,
    pub data_type: DataType,
}

#[derive(PartialEq,Eq,Debug)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
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

pub struct ExprInfo {
    pub position: Position,
    pub data_type: DataType,
    pub expr: Expr,
}

#[derive(PartialEq,Eq,Debug)]
pub enum Expr {
    Un(UnOp,Box<Expr>),
    Bin(BinOp,Box<Expr>,Box<Expr>),
    LitInt(i64),
    LitStr(String),
    LitTrue,
    LitFalse,
    Ident(String),
    Assign(Box<Expr>,Box<Expr>),
    Call(String,Vec<Box<Expr>>),
}

