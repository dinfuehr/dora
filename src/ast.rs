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
    pub params: Vec<LocalVar>,
    pub block: Box<Statement>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct LocalVar {
    pub name: String,
    pub position: Position,
    pub data_type: DataType,
}

#[derive(PartialEq,Eq,Debug)]
pub enum Statement {
    Var(String,DataType,Box<ExprType>),
    While(Box<ExprType>,Box<Statement>),
    Loop(Box<Statement>),
    If(Box<ExprType>,Box<Statement>,Box<Statement>),
    Expr(Box<ExprType>),
    Block(Vec<Box<Statement>>),
    Break,
    Continue,
    Return(Box<ExprType>),
}

impl Statement {
    pub fn empty_block() -> Box<Statement> {
        box Statement::Block(vec![])
    }

    pub fn block(expr: ExprType) -> Box<Statement> {
        let expr = box Statement::Expr(box expr);
        box Statement::Block(vec![expr])
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

