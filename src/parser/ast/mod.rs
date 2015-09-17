use std::fmt;
use std::hash::*;

use parser::ast::Elem::ElemFunction;
use parser::lexer::position::Position;
use parser::interner::{Interner, Name};

pub mod visit;
pub mod dump;
pub mod map;
pub mod ctxt;

pub struct Ast {
    pub elements: Vec<Elem>,
}

impl Ast {
    pub fn new(elements: Vec<Elem>) -> Ast {
        Ast {
            elements: elements
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct NodeId(pub usize);

impl Hash for NodeId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub enum Elem {
    ElemFunction(Function),
    ElemUnknown
}

impl Elem {
    pub fn to_function(&self) -> Option<&Function> {
        match *self {
            ElemFunction(ref fct) => Some(fct),
            _ => None
        }
    }
}

#[derive(Debug)]
pub enum Type {
    TypeBasic(TypeBasicType),
    TypeTuple(TypeTupleType),
    TypePtr(TypePtrType),
    TypeArray(TypeArrayType),
}

#[derive(Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub pos: Position,
    pub subtypes: Vec<Box<Type>>
}

#[derive(Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
}

#[derive(Debug)]
pub struct TypePtrType {
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
}

#[derive(Debug)]
pub struct TypeArrayType {
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
}

impl Type {
    pub fn create_basic(id: NodeId, pos: Position, name: Name) -> Type {
        Type::TypeBasic(TypeBasicType {
            id: id,
            pos: pos,
            name: name,
        })
    }

    pub fn create_ptr(id: NodeId, pos: Position, subtype: Box<Type>) -> Type {
        Type::TypePtr(TypePtrType {
            id: id,
            pos: pos,
            subtype: subtype
        })
    }

    pub fn create_array(id: NodeId, pos: Position, subtype: Box<Type>) -> Type {
        Type::TypeArray(TypeArrayType {
            id: id,
            pos: pos,
            subtype: subtype
        })
    }

    pub fn create_tuple(id: NodeId, pos: Position, subtypes: Vec<Box<Type>>) -> Type {
        Type::TypeTuple(TypeTupleType {
            id: id,
            pos: pos,
            subtypes: subtypes
        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasicType> {
        match *self {
            Type::TypeBasic(ref val) => Some(val),
            _ => None
        }
    }

    pub fn to_ptr(&self) -> Option<&TypePtrType> {
        match *self {
            Type::TypePtr(ref val) => Some(val),
            _ => None
        }
    }

    pub fn to_array(&self) -> Option<&TypeArrayType> {
        match *self {
            Type::TypeArray(ref val) => Some(val),
            _ => None
        }
    }

    pub fn to_tuple(&self) -> Option<&TypeTupleType> {
        match *self {
            Type::TypeTuple(ref val) => Some(val),
            _ => None
        }
    }

    pub fn to_string(&self, interner: &Interner) -> String {
        match *self {
            Type::TypeBasic(ref val) => {
                format!("{}", *interner.str(val.name))
            }

            Type::TypeTuple(ref val) => {
                let types : Vec<String> = val.subtypes.iter().map(|t| t.to_string(interner)).collect();

                format!("({})", types.connect(", "))
            }

            Type::TypePtr(ref val) => {
                format!("*{}", val.subtype.to_string(interner))
            }

            Type::TypeArray(ref val) => {
                format!("[{}]", val.subtype.to_string(interner))
            }
        }
    }

    pub fn pos(&self) -> Position {
        match *self {
            Type::TypeBasic(ref val) => val.pos,
            Type::TypeTuple(ref val) => val.pos,
            Type::TypePtr(ref val) => val.pos,
            Type::TypeArray(ref val) => val.pos,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Type::TypeBasic(ref val) => val.id,
            Type::TypeTuple(ref val) => val.id,
            Type::TypePtr(ref val) => val.id,
            Type::TypeArray(ref val) => val.id,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,

    pub params: Vec<Param>,

    pub return_type: Option<Type>,
    pub block: Box<Stmt>,
}

#[derive(Debug)]
pub struct Param {
    pub id: NodeId,
    pub idx: u32,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
}

#[derive(Debug)]
pub enum Stmt {
    StmtVar(StmtVarType),
    StmtWhile(StmtWhileType),
    StmtLoop(StmtLoopType),
    StmtIf(StmtIfType),
    StmtExpr(StmtExprType),
    StmtBlock(StmtBlockType),
    StmtBreak(StmtBreakType),
    StmtContinue(StmtContinueType),
    StmtReturn(StmtReturnType),
}

impl Stmt {
    pub fn create_var(id: NodeId, pos: Position, name: Name,
                      data_type: Option<Type>, expr: Option<Box<Expr>>) -> Stmt {
        Stmt::StmtVar(StmtVarType {
            id: id,
            pos: pos,
            name: name,
            data_type: data_type,
            expr: expr,
        })
    }

    pub fn create_while(id: NodeId, pos: Position, cond: Box<Expr>,
                        block: Box<Stmt>) -> Stmt {
        Stmt::StmtWhile(StmtWhileType {
            id: id,
            pos: pos,
            cond: cond,
            block: block,
        })
    }

    pub fn create_loop(id: NodeId, pos: Position, block: Box<Stmt>) -> Stmt {
        Stmt::StmtLoop(StmtLoopType {
            id: id,
            pos: pos,
            block: block,
        })
    }

    pub fn create_if(id: NodeId, pos: Position, cond: Box<Expr>,
                 then_block: Box<Stmt>, else_block: Option<Box<Stmt>>) -> Stmt {
        Stmt::StmtIf(StmtIfType {
            id: id,
            pos: pos,
            cond: cond,
            then_block: then_block,
            else_block: else_block,
        })
    }

    pub fn create_expr(id: NodeId, pos: Position, expr: Box<Expr>) -> Stmt {
        Stmt::StmtExpr(StmtExprType {
            id: id,
            pos: pos,
            expr: expr,
        })
    }

    pub fn create_block(id: NodeId, pos: Position, stmts: Vec<Box<Stmt>>) -> Stmt {
        Stmt::StmtBlock(StmtBlockType {
            id: id,
            pos: pos,
            stmts: stmts,
        })
    }

    pub fn create_break(id: NodeId, pos: Position) -> Stmt {
        Stmt::StmtBreak(StmtBreakType {
            id: id,
            pos: pos,
        })
    }

    pub fn create_continue(id: NodeId, pos: Position) -> Stmt {
        Stmt::StmtContinue(StmtContinueType {
            id: id,
            pos: pos,
        })
    }

    pub fn create_return(id: NodeId, pos: Position, expr: Option<Box<Expr>>) -> Stmt {
        Stmt::StmtReturn(StmtReturnType {
            id: id,
            pos: pos,
            expr: expr,
        })
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Stmt::StmtVar(ref stmt) => stmt.id,
            Stmt::StmtWhile(ref stmt) => stmt.id,
            Stmt::StmtLoop(ref stmt) => stmt.id,
            Stmt::StmtIf(ref stmt) => stmt.id,
            Stmt::StmtExpr(ref stmt) => stmt.id,
            Stmt::StmtBlock(ref stmt) => stmt.id,
            Stmt::StmtBreak(ref stmt) => stmt.id,
            Stmt::StmtContinue(ref stmt) => stmt.id,
            Stmt::StmtReturn(ref stmt) => stmt.id,
        }
    }

    pub fn pos(&self) -> Position {
        match *self {
            Stmt::StmtVar(ref stmt) => stmt.pos,
            Stmt::StmtWhile(ref stmt) => stmt.pos,
            Stmt::StmtLoop(ref stmt) => stmt.pos,
            Stmt::StmtIf(ref stmt) => stmt.pos,
            Stmt::StmtExpr(ref stmt) => stmt.pos,
            Stmt::StmtBlock(ref stmt) => stmt.pos,
            Stmt::StmtBreak(ref stmt) => stmt.pos,
            Stmt::StmtContinue(ref stmt) => stmt.pos,
            Stmt::StmtReturn(ref stmt) => stmt.pos,
        }
    }

    pub fn to_var(&self) -> Option<&StmtVarType> {
        match *self {
            Stmt::StmtVar(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            Stmt::StmtVar(_) => true,
            _ => false
        }
    }

    pub fn to_while(&self) -> Option<&StmtWhileType> {
        match *self {
            Stmt::StmtWhile(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            Stmt::StmtWhile(_) => true,
            _ => false
        }
    }

    pub fn to_loop(&self) -> Option<&StmtLoopType> {
        match *self {
            Stmt::StmtLoop(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_loop(&self) -> bool {
        match *self {
            Stmt::StmtLoop(_) => true,
            _ => false
        }
    }

    pub fn to_if(&self) -> Option<&StmtIfType> {
        match *self {
            Stmt::StmtIf(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_if(&self) -> bool {
        match *self {
            Stmt::StmtIf(_) => true,
            _ => false
        }
    }

    pub fn to_expr(&self) -> Option<&StmtExprType> {
        match *self {
            Stmt::StmtExpr(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            Stmt::StmtExpr(_) => true,
            _ => false
        }
    }

    pub fn to_block(&self) -> Option<&StmtBlockType> {
        match *self {
            Stmt::StmtBlock(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_block(&self) -> bool {
        match *self {
            Stmt::StmtBlock(_) => true,
            _ => false
        }
    }

    pub fn to_return(&self) -> Option<&StmtReturnType> {
        match *self {
            Stmt::StmtReturn(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            Stmt::StmtReturn(_) => true,
            _ => false
        }
    }

    pub fn to_break(&self) -> Option<&StmtBreakType> {
        match *self {
            Stmt::StmtBreak(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            Stmt::StmtBreak(_) => true,
            _ => false
        }
    }

    pub fn to_continue(&self) -> Option<&StmtContinueType> {
        match *self {
            Stmt::StmtContinue(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            Stmt::StmtContinue(_) => true,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct StmtVarType {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,

    pub data_type: Option<Type>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct StmtWhileType {
    pub id: NodeId,
    pub pos: Position,

    pub cond: Box<Expr>,
    pub block: Box<Stmt>,
}

#[derive(Debug)]
pub struct StmtLoopType {
    pub id: NodeId,
    pub pos: Position,
    pub block: Box<Stmt>,
}

#[derive(Debug)]
pub struct StmtIfType {
    pub id: NodeId,
    pub pos: Position,
    pub cond: Box<Expr>,
    pub then_block: Box<Stmt>,
    pub else_block: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct StmtBlockType {
    pub id: NodeId,
    pub pos: Position,
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Debug)]
pub struct StmtReturnType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct StmtBreakType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Debug)]
pub struct StmtContinueType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(PartialEq, Eq, Debug)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
    BitNot
}

impl ToString for UnOp {
    fn to_string(&self) -> String {
        let repr = match *self {
            UnOp::Plus => "+",
            UnOp::Neg => "-",
            UnOp::Not => "!",
            UnOp::BitNot => "~"
        };

        repr.into()
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl CmpOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "!=",
            CmpOp::Lt => "<",
            CmpOp::Le => "<=",
            CmpOp::Gt => ">",
            CmpOp::Ge => ">=",
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cmp(CmpOp),
    Or,
    And,
    BitOr,
    BitAnd,
    BitXor,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Cmp(op) => op.as_str(),
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::BitOr => "|",
            BinOp::BitAnd => "&",
            BinOp::BitXor => "^"
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    ExprUn(ExprUnType),
    ExprBin(ExprBinType),
    ExprLitInt(ExprLitIntType),
    ExprLitStr(ExprLitStrType),
    ExprLitBool(ExprLitBoolType),
    ExprIdent(ExprIdentType),
    ExprCall(ExprCallType),
    ExprAssign(ExprAssignType),
}

impl Expr {
    pub fn create_un(id: NodeId, pos: Position, op: UnOp, opnd: Box<Expr>) -> Expr {
        Expr::ExprUn(ExprUnType {
            id: id,
            pos: pos,
            op: op,
            opnd: opnd,
        })
    }

    pub fn create_bin(id: NodeId, pos: Position, op: BinOp,
                      lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprBin(ExprBinType {
            id: id,
            pos: pos,
            op: op,
            lhs: lhs,
            rhs: rhs,
        })
    }

    pub fn create_lit_int(id: NodeId, pos: Position, value: i32) -> Expr {
        Expr::ExprLitInt(ExprLitIntType {
            id: id,
            pos: pos,
            value: value,
        })
    }

    pub fn create_lit_str(id: NodeId, pos: Position, value: String) -> Expr {
        Expr::ExprLitStr(ExprLitStrType {
            id: id,
            pos: pos,
            value: value,
        })
    }


    pub fn create_lit_bool(id: NodeId, pos: Position, value: bool) -> Expr {
        Expr::ExprLitBool(ExprLitBoolType {
            id: id,
            pos: pos,
            value: value,
        })
    }

    pub fn create_ident(id: NodeId, pos: Position, name: Name) -> Expr {
        Expr::ExprIdent(ExprIdentType {
            id: id,
            pos: pos,
            name: name,
        })
    }

    pub fn create_call(id: NodeId, pos: Position, name: Name, args: Vec<Box<Expr>>) -> Expr {
        Expr::ExprCall(ExprCallType {
            id: id,
            pos: pos,
            name: name,
            args: args
        })
    }

    pub fn create_assign(id: NodeId, pos: Position,
                         lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprAssign(ExprAssignType {
            id: id,
            pos: pos,
            lhs: lhs,
            rhs: rhs,
        })
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            Expr::ExprUn(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_un(&self) -> bool {
        match *self {
            Expr::ExprUn(_) => true,
            _ => false
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            Expr::ExprBin(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            Expr::ExprBin(_) => true,
            _ => false
        }
    }

    pub fn to_assign(&self) -> Option<&ExprAssignType> {
        match *self {
            Expr::ExprAssign(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_assign(&self) -> bool {
        match *self {
            Expr::ExprAssign(_) => true,
            _ => false
        }
    }

    pub fn to_ident(&self) -> Option<&ExprIdentType> {
        match *self {
            Expr::ExprIdent(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            Expr::ExprIdent(_) => true,
            _ => false
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            Expr::ExprCall(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            Expr::ExprCall(_) => true,
            _ => false
        }
    }

    pub fn to_lit_int(&self) -> Option<&ExprLitIntType> {
        match *self {
            Expr::ExprLitInt(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            Expr::ExprLitInt(_) => true,
            _ => false
        }
    }

    pub fn to_lit_str(&self) -> Option<&ExprLitStrType> {
        match *self {
            Expr::ExprLitStr(ref val) => Some(val),
            _ => None
        }
    }

    pub fn to_lit_bool(&self) -> Option<&ExprLitBoolType> {
        match *self {
            Expr::ExprLitBool(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_lit_bool(&self) -> bool {
        match *self {
            Expr::ExprLitBool(_) => true,
            _ => false
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Expr::ExprUn(ref val) => val.id,
            Expr::ExprBin(ref val) => val.id,
            Expr::ExprLitInt(ref val) => val.id,
            Expr::ExprLitStr(ref val) => val.id,
            Expr::ExprLitBool(ref val) => val.id,
            Expr::ExprIdent(ref val) => val.id,
            Expr::ExprAssign(ref val) => val.id,
            Expr::ExprCall(ref val) => val.id
        }
    }
}

#[derive(Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub pos: Position,

    pub op: UnOp,
    pub opnd: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub pos: Position,

    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub pos: Position,

    pub value: i32,
}

#[derive(Debug)]
pub struct ExprLitStrType {
    pub id: NodeId,
    pub pos: Position,

    pub value: String,
}

#[derive(Debug)]
pub struct ExprLitBoolType {
    pub id: NodeId,
    pub pos: Position,

    pub value: bool,
}

#[derive(Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
}

#[derive(Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct ExprAssignType {
    pub id: NodeId,
    pub pos: Position,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>
}
