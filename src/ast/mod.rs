use ast::ElemType::ElemFunction;
use lexer::position::Position;

pub mod visit;

#[derive(Debug)]
pub struct Ast {
    pub elements: Vec<Elem>,
}

impl Ast {
    pub fn function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let ElemFunction(ref fct) = e.node {
                if fct.name == name { return Some(fct); }
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
    pub params: Vec<TypeInfo>
}

#[derive(PartialEq,Eq,Debug)]
pub enum TypeInfo {
    Basic(String),
    Generic(String,Vec<TypeInfo>),
    Slice(Box<TypeInfo>),
    Ptr(Box<TypeInfo>),
    Tuple(Vec<TypeInfo>),
}

impl TypeInfo {
    pub fn is_unit(&self) -> bool {
        if let TypeInfo::Tuple(ref types) = *self {
            types.len() == 0
        } else {
            false
        }
    }

    pub fn is_int(&self) -> bool {
        if let TypeInfo::Basic(ref name) = *self {
            &name[..] == "int"
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct TupleStruct {
    pub name: String,
    pub type_params: TypeParams,
    pub params: Vec<TypeInfo>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct TypeParams {
    pub params: Vec<String>
}

impl TypeParams {
    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn empty(&self) -> bool {
        self.params.len() == 0
    }
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
    pub data_type: TypeInfo,
}

#[derive(Debug)]
pub struct Alias {
    pub name: String,
    pub data_type: TypeInfo,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub pos: Position,

    pub type_params: TypeParams,
    pub params: Vec<Param>,

    pub return_type: TypeInfo,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: String,
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
    StmtVar(String, Option<TypeInfo>, Option<Box<Expr>>),
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
    ExprIdent(String),
    ExprAssign(Box<Expr>,Box<Expr>),
}

