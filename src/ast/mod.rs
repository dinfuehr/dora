use ast::ElemType::ElemFunction;
use lexer::position::Position;
use interner::Interner;
use interner::Name;

pub mod visit;
pub mod dump;

pub struct Ast {
    elements: Vec<Elem>,
    interner: Interner,
}

impl Ast {
    pub fn new(elements: Vec<Elem>, interner: Interner) -> Ast {
        Ast {
            elements: elements,
            interner: interner
        }
    }

    pub fn function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let ElemFunction(ref fct) = e.node {
                if self.str(fct.name) == name { return Some(fct); }
            }
        }

        None
    }

    pub fn str(&self, name: Name) -> &str {
        self.interner.str(name)
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
pub enum Type {
    TypeBasic(Name),
    TypeUnit
}

#[derive(Debug)]
pub struct Function {
    pub name: Name,
    pub pos: Position,

    pub params: Vec<Param>,

    pub return_type: Type,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: Name,
    pub position: Position,
    pub data_type: Type,
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
    StmtVar(Name, Option<Type>, Option<Box<Expr>>),
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
    ExprIdent(Name),
    ExprAssign(Box<Expr>,Box<Expr>),
}

