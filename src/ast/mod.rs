use ast::ElemType::ElemFunction;
use lexer::position::Position;
use interner::Interner;
use interner::InternStr;

pub mod visit;

pub struct Ast {
    pub elements: Vec<Elem>,
    pub interner: Interner,
}

impl Ast {
    pub fn function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let ElemFunction(ref fct) = e.node {
                if self.interner.str(fct.name) == name { return Some(fct); }
            }
        }

        None
    }
}

#[derive(Debug)]
pub struct Elem {
    pub pos: Position,
    pub node: ElemType,
}

impl Elem {
    pub fn new(pos: Position, node: ElemType) -> Elem {
        Elem { pos: pos, node: node }
    }
}

#[derive(Debug)]
pub enum ElemType {
    ElemFunction(Function),
    ElemUnknown
}

#[derive(PartialEq,Eq,Debug)]
pub enum TypeInfo {
    Basic(InternStr),
    Unit
}

#[derive(Debug)]
pub struct Function {
    pub name: InternStr,
    pub pos: Position,

    pub params: Vec<Param>,

    pub return_type: TypeInfo,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: InternStr,
    pub position: Position,
    pub data_type: TypeInfo,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Stmt {
    pub pos: Position,
    pub node: StmtType,
}

impl Stmt {
    pub fn new(pos: Position, node: StmtType) -> Stmt {
        Stmt { pos: pos, node: node }
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum StmtType {
    StmtVar(InternStr, Option<TypeInfo>, Option<Box<Expr>>),
    StmtWhile(Box<Expr>, Box<Stmt>),
    StmtLoop(Box<Stmt>),
    StmtIf(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    StmtExpr(Box<Expr>),
    StmtBlock(Vec<Box<Stmt>>),
    StmtBreak,
    StmtContinue,
    StmtReturn(Option<Box<Expr>>),
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

#[derive(PartialEq,Eq,Debug)]
pub struct Expr {
    pub pos: Position,
    pub node: ExprType,
}

impl Expr {
    pub fn new(pos: Position, expr: ExprType) -> Box<Expr> {
        box Expr { pos: pos, node: expr }
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum ExprType {
    ExprUn(UnOp,Box<Expr>),
    ExprBin(BinOp,Box<Expr>,Box<Expr>),
    ExprLitInt(i64),
    ExprLitStr(String),
    ExprLitBool(bool),
    ExprIdent(InternStr),
    ExprAssign(Box<Expr>,Box<Expr>),
}

