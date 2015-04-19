use lexer::position::Position;

pub mod visit;

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
pub struct Statement {
    pub position: Position,
    pub stmt: StatementType
}

impl Statement {
    pub fn new(pos: Position, stmt: StatementType) -> Box<Statement> {
        box Statement { position: pos, stmt: stmt }
    }

    #[cfg(test)]
    pub fn expr(pos: Position, expr: Box<Expr>) -> Box<Statement> {
        Statement::new(pos, StatementType::ExprStmt(expr))
    }

    #[cfg(test)]
    pub fn block(pos: Position, stmt: Box<Statement>) -> Box<Statement> {
        Statement::new(pos, StatementType::Block(vec![stmt]))
    }

    #[cfg(test)]
    pub fn block_with_stmts(pos: Position, stmt: Vec<Box<Statement>>) -> Box<Statement> {
        Statement::new(pos, StatementType::Block(stmt))
    }
}

#[derive(PartialEq,Eq,Debug)]
pub enum StatementType {
    Var(String,Option<Type>,Option<Box<Expr>>),
    While(Box<Expr>,Box<Statement>),
    Loop(Box<Statement>),
    If(Box<Expr>,Box<Statement>,Option<Box<Statement>>),
    ExprStmt(Box<Expr>),
    Block(Vec<Box<Statement>>),
    Break,
    Continue,
    Return(Option<Box<Expr>>),
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

    pub fn lit_int(pos: Position, value: i64) -> Box<Expr> {
        Expr::new(pos, ExprType::LitInt(value))
    }

    #[cfg(test)]
    pub fn lit_str(pos: Position, value: String) -> Box<Expr> {
        Expr::new(pos, ExprType::LitStr(value))
    }

    #[cfg(test)]
    pub fn lit_bool(pos: Position, value: bool) -> Box<Expr> {
        let ty = if value { ExprType::LitTrue } else { ExprType::LitFalse };

        Expr::new(pos, ty)
    }

    pub fn ident(pos: Position, value: String) -> Box<Expr> {
        box Expr {
            position: pos,
            expr: ExprType::Ident(value),
        }
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

