use std::fmt;

use ast::Elem::ElemFunction;
use lexer::position::Position;
use interner::Interner;
use interner::Name;

pub mod visit;
pub mod dump;

pub struct Ast {
    pub elements: Vec<Elem>,
    interner: Interner,
}

impl Ast {
    pub fn new(elements: Vec<Elem>, interner: Interner) -> Ast {
        Ast {
            elements: elements,
            interner: interner
        }
    }

    pub fn find_function(&self, name: &str) -> Option<&Function> {
        for e in &self.elements {
            if let ElemFunction(ref fct) = *e {
                if self.str(fct.name) == name { return Some(fct); }
            }
        }

        None
    }

    pub fn str(&self, name: Name) -> &str {
        self.interner.str(name)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct NodeId(pub u32);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub enum Elem {
    ElemFunction(Function),
    ElemUnknown
}

#[derive(Debug)]
pub struct Type {
    pub id: NodeId,
    pub pos: Option<Position>,
    pub builtin: BuiltinType,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BuiltinType {
    Unit,
    Int,
    Str,
}

impl Type {
    pub fn create(id: NodeId, pos: Position, builtin: BuiltinType) -> Type {
        Type {
            id: id,
            pos: Some(pos),
            builtin: builtin,
        }
    }

    pub fn create_implicit(id: NodeId, builtin: BuiltinType) -> Type {
        Type {
            id: id,
            pos: None,
            builtin: builtin
        }
    }

    pub fn is_int(&self) -> bool {
        self.builtin == BuiltinType::Int
    }

    pub fn is_unit(&self) -> bool {
        self.builtin == BuiltinType::Unit
    }

    pub fn is_str(&self) -> bool {
        self.builtin == BuiltinType::Str
    }
}

pub struct Function {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,

    pub params: Vec<Param>,

    pub return_type: Type,
    pub block: Box<Stmt>,
}

#[derive(Debug)]
pub struct Param {
    pub id: NodeId,
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
            Stmt::StmtVar(ref val) => val.id,
            Stmt::StmtWhile(ref val) => val.id,
            Stmt::StmtLoop(ref val) => val.id,
            Stmt::StmtIf(ref val) => val.id,
            Stmt::StmtExpr(ref val) => val.id,
            Stmt::StmtBlock(ref val) => val.id,
            Stmt::StmtBreak(ref val) => val.id,
            Stmt::StmtContinue(ref val) => val.id,
            Stmt::StmtReturn(ref val) => val.id,
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
}

#[derive(PartialEq, Eq, Debug)]
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

#[derive(Debug)]
pub enum Expr {
    ExprUn(ExprUnType),
    ExprBin(ExprBinType),
    ExprLitInt(ExprLitIntType),
    ExprLitStr(ExprLitStrType),
    ExprLitBool(ExprLitBoolType),
    ExprIdent(ExprIdentType),
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
        }
    }
}

#[derive(Debug)]
struct ExprUnType {
    pub id: NodeId,
    pub pos: Position,

    pub op: UnOp,
    pub opnd: Box<Expr>,
}

#[derive(Debug)]
struct ExprBinType {
    pub id: NodeId,
    pub pos: Position,

    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
struct ExprLitIntType {
    pub id: NodeId,
    pub pos: Position,

    pub value: i32,
}

#[derive(Debug)]
struct ExprLitStrType {
    pub id: NodeId,
    pub pos: Position,

    pub value: String,
}

#[derive(Debug)]
struct ExprLitBoolType {
    pub id: NodeId,
    pub pos: Position,

    pub value: bool,
}

#[derive(Debug)]
struct ExprIdentType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
}

#[derive(Debug)]
struct ExprAssignType {
    pub id: NodeId,
    pub pos: Position,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>
}
