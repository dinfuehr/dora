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

    pub fn function(&self, name: &str) -> Option<&Function> {
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

#[derive(Debug)]
pub enum Elem {
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
    pub pos: Position,
    pub data_type: Type,
}

#[derive(PartialEq,Eq,Debug)]
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
    pub fn create_var(pos: Position, name: Name,
                      data_type: Option<Type>, expr: Option<Box<Expr>>) -> Stmt {
        Stmt::StmtVar(StmtVarType {
            pos: pos,
            name: name,
            data_type: data_type,
            expr: expr,
        })
    }

    pub fn create_while(pos: Position, cond: Box<Expr>, block: Box<Stmt>) -> Stmt {
        Stmt::StmtWhile(StmtWhileType {
            pos: pos,
            cond: cond,
            block: block,
        })
    }

    pub fn create_loop(pos: Position, block: Box<Stmt>) -> Stmt {
        Stmt::StmtLoop(StmtLoopType {
            pos: pos,
            block: block,
        })
    }

    pub fn create_if(pos: Position, cond: Box<Expr>,
                 then_block: Box<Stmt>, else_block: Option<Box<Stmt>>) -> Stmt {
        Stmt::StmtIf(StmtIfType {
            pos: pos,
            cond: cond,
            then_block: then_block,
            else_block: else_block,
        })
    }

    pub fn create_expr(pos: Position, expr: Box<Expr>) -> Stmt {
        Stmt::StmtExpr(StmtExprType {
            pos: pos,
            expr: expr,
        })
    }

    pub fn create_block(pos: Position, stmts: Vec<Box<Stmt>>) -> Stmt {
        Stmt::StmtBlock(StmtBlockType {
            pos: pos,
            stmts: stmts,
        })
    }

    pub fn create_break(pos: Position) -> Stmt {
        Stmt::StmtBreak(StmtBreakType {
            pos: pos,
        })
    }

    pub fn create_continue(pos: Position) -> Stmt {
        Stmt::StmtContinue(StmtContinueType {
            pos: pos,
        })
    }

    pub fn create_return(pos: Position, expr: Option<Box<Expr>>) -> Stmt {
        Stmt::StmtReturn(StmtReturnType {
            pos: pos,
            expr: expr,
        })
    }
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtVarType {
    pub pos: Position,
    pub name: Name,
    pub data_type: Option<Type>,
    pub expr: Option<Box<Expr>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtWhileType {
    pub pos: Position,
    pub cond: Box<Expr>,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtLoopType {
    pub pos: Position,
    pub block: Box<Stmt>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtIfType {
    pub pos: Position,
    pub cond: Box<Expr>,
    pub then_block: Box<Stmt>,
    pub else_block: Option<Box<Stmt>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtExprType {
    pub pos: Position,
    pub expr: Box<Expr>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtBlockType {
    pub pos: Position,
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtReturnType {
    pub pos: Position,
    pub expr: Option<Box<Expr>>,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtBreakType {
    pub pos: Position,
}

#[derive(PartialEq,Eq,Debug)]
pub struct StmtContinueType {
    pub pos: Position,
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
    pub fn create_un(pos: Position, op: UnOp, opnd: Box<Expr>) -> Expr {
        Expr::ExprUn(ExprUnType {
            pos: pos,
            op: op,
            opnd: opnd,
        })
    }

    pub fn create_bin(pos: Position, op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprBin(ExprBinType {
            pos: pos,
            op: op,
            lhs: lhs,
            rhs: rhs,
        })
    }

    pub fn create_lit_int(pos: Position, value: i32) -> Expr {
        Expr::ExprLitInt(ExprLitIntType {
            pos: pos,
            value: value,
        })
    }

    pub fn create_lit_str(pos: Position, value: String) -> Expr {
        Expr::ExprLitStr(ExprLitStrType {
            pos: pos,
            value: value,
        })
    }


    pub fn create_lit_bool(pos: Position, value: bool) -> Expr {
        Expr::ExprLitBool(ExprLitBoolType {
            pos: pos,
            value: value,
        })
    }

    pub fn create_ident(pos: Position, name: Name) -> Expr {
        Expr::ExprIdent(ExprIdentType {
            pos: pos,
            name: name,
        })
    }

    pub fn create_assign(pos: Position, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprAssign(ExprAssignType {
            pos: pos,
            lhs: lhs,
            rhs: rhs,
        })
    }
}

#[derive(PartialEq,Eq,Debug)]
struct ExprUnType {
    pos: Position,
    op: UnOp,
    opnd: Box<Expr>,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprBinType {
    pos: Position,
    op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprLitIntType {
    pos: Position,
    value: i32,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprLitStrType {
    pos: Position,
    value: String,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprLitBoolType {
    pos: Position,
    value: bool,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprIdentType {
    pos: Position,
    name: Name,
}

#[derive(PartialEq,Eq,Debug)]
struct ExprAssignType {
    pos: Position,
    lhs: Box<Expr>,
    rhs: Box<Expr>
}
