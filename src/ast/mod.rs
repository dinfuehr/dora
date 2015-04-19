use lexer::position::Position;

#[derive(Debug)]
pub struct Program {
    pub elements: Vec<TopLevelElement>,
}

impl Program {
    #[cfg(test)]
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let TopLevelElement::Function(ref fct) = *e {
                if fct.name == name { return Some(fct); }
            }
        }

        None
    }
}

#[derive(Debug)]
pub enum TopLevelElement {
    Function(Function),
    Enum(Enum),
    TupleStruct(TupleStruct),
    Struct(Struct),
    Alias(Alias),
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
    pub block: Box<Statement>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct Param {
    pub name: String,
    pub position: Position,
    pub data_type: Type,
}

#[derive(PartialEq,Eq,Debug)]
pub enum Statement {
    Var(VarStmt),
    While(WhileStmt),
    Loop(LoopStmt),
    If(IfStmt),
    Expr(ExprStmt),
    Block(BlockStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Return(ReturnStmt),
}

#[derive(PartialEq,Eq,Debug)]
pub struct VarStmt {
    pub position: Position,
    pub name: String,
    pub data_type: Option<Type>,
    pub expression: Option<Box<Expr>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct WhileStmt {
    pub position: Position,
    pub condition: Box<Expr>,
    pub block: Box<Statement>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct LoopStmt {
    pub position: Position,
    pub block: Box<Statement>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct IfStmt {
    pub position: Position,
    pub condition: Box<Expr>,
    pub then_block: Box<Statement>,
    pub else_block: Option<Box<Statement>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct ExprStmt {
    pub position: Position,
    pub expression: Box<Expr>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct BlockStmt {
    pub position: Position,
    pub statements: Vec<Box<Statement>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct BreakStmt {
    pub position: Position,
}

#[derive(PartialEq,Eq,Debug)]
pub struct ContinueStmt {
    pub position: Position,
}

#[derive(PartialEq,Eq,Debug)]
pub struct ReturnStmt {
    pub position: Position,
    pub expression: Option<Box<Expr>>,
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
    Un(UnOp,Box<Expr>),
    Bin(BinOp,Box<Expr>,Box<Expr>),
    LitInt(i64),
    LitStr(String),
    LitTrue,
    LitFalse,
    Ident(String),
    Assign(Box<Expr>,Box<Expr>),
}

