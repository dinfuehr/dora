use ast::Elem::ElemFunction;

use lexer::position::Position;

#[derive(Debug)]
pub struct Program {
    pub elements: Vec<Elem>,
}

impl Program {
    #[cfg(test)]
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let ElemFunction(ref fct) = *e {
                if fct.name == name { return Some(fct); }
            }
        }

        None
    }
}

#[derive(Debug)]
pub enum Elem {
    ElemFunction(Function),
    ElemEnum(Enum),
    ElemTupleStruct(TupleStruct),
    ElemStruct(Struct),
    ElemAlias(Alias),
}

#[derive(Debug)]
pub struct Enum {
    pub name: String,
    pub type_params: TypeParams,
    pub values: Vec<EnumValue>,
}

#[derive(Debug)]
pub struct EnumValue {
    pub name: String,
    pub params: Vec<Type>
}

#[derive(PartialEq,Eq,Debug)]
pub enum Type {
    Basic(String),
    Generic(String,Vec<Type>),
    Slice(Box<Type>),
    Ptr(Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug)]
pub struct TupleStruct {
    pub name: String,
    pub type_params: TypeParams,
    pub params: Vec<Type>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct TypeParams {
    pub params: Vec<String>
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub params: Vec<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub data_type: Type,
}

#[derive(Debug)]
pub struct Alias {
    pub name: String,
    pub data_type: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub position: Position,

    pub type_params: TypeParams,
    pub params: Vec<Param>,

    pub return_type: Type,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: String,
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
    StmtVar(String, Option<Type>, Option<Box<Expr>>),
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
    pub position: Position,
    pub expr: ExprType,
}

impl Expr {
    pub fn new(pos: Position, expr: ExprType) -> Box<Expr> {
        box Expr { position: pos, expr: expr }
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum ExprType {
    ExprUn(UnOp,Box<Expr>),
    ExprBin(BinOp,Box<Expr>,Box<Expr>),
    ExprLitInt(i64),
    ExprLitStr(String),
    ExprLitBool(bool),
    ExprIdent(String),
    ExprAssign(Box<Expr>,Box<Expr>),
}

