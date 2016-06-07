use std::cell::RefCell;
use std::fmt;
use std::hash::*;
use std::slice::Iter;

use ast::Elem::*;
use class::{ClassId, FieldId};
use ctxt::{CtorType, FctId, IdentType, VarId};
use lexer::position::Position;
use interner::{Interner, Name};
use ty::BuiltinType;

pub mod visit;
pub mod dump;

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Elem {
    ElemFunction(Function),
    ElemClass(Class),
}

impl Elem {
    pub fn id(&self) -> NodeId {
        match *self {
            ElemFunction(ref fct) => fct.id,
            ElemClass(ref class) => class.id,
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match *self {
            ElemFunction(ref fct) => Some(fct),
            _ => None
        }
    }

    pub fn to_class(&self) -> Option<&Class> {
        match *self {
            ElemClass(ref class) => Some(class),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    TypeSelf,
    TypeBasic(TypeBasicType),
    TypeTuple(TypeTupleType),
    TypePtr(TypePtrType),
    TypeArray(TypeArrayType),
}

impl Type {
    pub fn is_self(&self) -> bool {
        match *self {
            Type::TypeSelf => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub pos: Position,
    pub subtypes: Vec<Box<Type>>
}

#[derive(Clone, Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct TypePtrType {
    pub id: NodeId,
    pub pos: Position,
    pub subtype: Box<Type>,
}

#[derive(Clone, Debug)]
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
            Type::TypeSelf => {
                "Self".into()
            }

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
            Type::TypeSelf => panic!("no position for Self"),
            Type::TypeBasic(ref val) => val.pos,
            Type::TypeTuple(ref val) => val.pos,
            Type::TypePtr(ref val) => val.pos,
            Type::TypeArray(ref val) => val.pos,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Type::TypeSelf => panic!("no id for Self"),
            Type::TypeBasic(ref val) => val.id,
            Type::TypeTuple(ref val) => val.id,
            Type::TypePtr(ref val) => val.id,
            Type::TypeArray(ref val) => val.id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub parent_class: Option<ParentClass>,
    pub derivable: bool,
    pub ctor_params: Vec<PrimaryCtorParam>,

    pub ctors: Vec<Function>,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct PrimaryCtorParam {
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub field: bool,
    pub reassignable: bool,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl PrimaryCtorParam {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ParentClass {
    pub name: Name,
    pub pos: Position,
    pub params: Vec<Box<Expr>>,
    pub cls: RefCell<Option<ClassId>>,
}

impl ParentClass {
    pub fn new(name: Name, pos: Position, params: Vec<Box<Expr>>) -> ParentClass {
        ParentClass {
            name: name,
            pos: pos,
            cls: RefCell::new(None),
            params: params,
        }
    }

    pub fn set_cls(&self, cls: ClassId) {
        *self.cls.borrow_mut() = Some(cls);
    }

    pub fn cls(&self) -> ClassId {
        self.cls.borrow().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub expr: Option<Box<Expr>>,
    pub reassignable: bool,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub method: bool,
    pub overridable: bool,
    pub overrides: bool,
    pub ctor: Option<CtorType>,

    pub params: Vec<Param>,
    pub throws: bool,

    pub return_type: Option<Type>,
    pub block: Box<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Modifiers(Vec<ModifierElement>);

impl Modifiers {
    pub fn new() -> Modifiers {
        Modifiers(Vec::new())
    }

    pub fn contains(&self, modifier: Modifier) -> bool {
        self.0.iter().find(|el| el.value == modifier).is_some()
    }

    pub fn add(&mut self, modifier: Modifier, pos: Position) {
        self.0.push(ModifierElement {
            value: modifier,
            pos: pos
        });
    }

    pub fn iter(&self) -> Iter<ModifierElement> {
        self.0.iter()
    }
}

#[derive(Clone, Debug)]
pub struct ModifierElement {
    pub value: Modifier,
    pub pos: Position,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Modifier { Override, Open }

impl Modifier {
    pub fn name(&self) -> &'static str {
        match *self {
            Modifier::Open => "open",
            Modifier::Override => "override",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub idx: u32,
    pub reassignable: bool,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub info: RefCell<Option<VarId>>,
}

impl Param {
    pub fn var(&self) -> VarId {
        self.info.borrow().unwrap()
    }

    pub fn set_var(&self, var: VarId) {
        *self.info.borrow_mut() = Some(var);
    }
}

#[derive(Clone, Debug)]
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
    StmtThrow(StmtThrowType),
    StmtTry(StmtTryType),
}

impl Stmt {
    pub fn create_var(id: NodeId, pos: Position, name: Name, reassignable: bool,
                      data_type: Option<Type>, expr: Option<Box<Expr>>) -> Stmt {
        Stmt::StmtVar(StmtVarType {
            id: id,
            pos: pos,
            name: name,
            reassignable: reassignable,
            data_type: data_type,
            expr: expr,
            info: RefCell::new(None),
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

    pub fn create_throw(id: NodeId, pos: Position, expr: Box<Expr>) -> Stmt {
        Stmt::StmtThrow(StmtThrowType {
            id: id,
            pos: pos,
            expr: expr,
        })
    }

    pub fn create_try(id: NodeId, pos: Position, try_block: Box<Stmt>,
                      catch_blocks: Vec<CatchBlock>,
                      finally_block: Option<FinallyBlock>) -> Stmt {
        Stmt::StmtTry(StmtTryType {
            id: id,
            pos: pos,
            try_block: try_block,
            catch_blocks: catch_blocks,
            finally_block: finally_block,
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
            Stmt::StmtThrow(ref stmt) => stmt.id,
            Stmt::StmtTry(ref stmt) => stmt.id,
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
            Stmt::StmtThrow(ref stmt) => stmt.pos,
            Stmt::StmtTry(ref stmt) => stmt.pos,
        }
    }

    pub fn to_throw(&self) -> Option<&StmtThrowType> {
        match *self {
            Stmt::StmtThrow(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_throw(&self) -> bool {
        match *self {
            Stmt::StmtThrow(_) => true,
            _ => false
        }
    }

    pub fn to_try(&self) -> Option<&StmtTryType> {
        match *self {
            Stmt::StmtTry(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_try(&self) -> bool {
        match *self {
            Stmt::StmtTry(_) => true,
            _ => false
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

#[derive(Clone, Debug)]
pub struct StmtVarType {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub reassignable: bool,

    pub data_type: Option<Type>,
    pub expr: Option<Box<Expr>>,
    pub info: RefCell<Option<VarId>>,
}

impl StmtVarType {
    pub fn var(&self) -> VarId {
        self.info.borrow().unwrap()
    }

    pub fn set_var(&self, var: VarId) {
        *self.info.borrow_mut() = Some(var);
    }
}

#[derive(Clone, Debug)]
pub struct StmtWhileType {
    pub id: NodeId,
    pub pos: Position,

    pub cond: Box<Expr>,
    pub block: Box<Stmt>,
}

#[derive(Clone, Debug)]
pub struct StmtLoopType {
    pub id: NodeId,
    pub pos: Position,
    pub block: Box<Stmt>,
}

#[derive(Clone, Debug)]
pub struct StmtIfType {
    pub id: NodeId,
    pub pos: Position,
    pub cond: Box<Expr>,
    pub then_block: Box<Stmt>,
    pub else_block: Option<Box<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct StmtBlockType {
    pub id: NodeId,
    pub pos: Position,
    pub stmts: Vec<Box<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct StmtReturnType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct StmtBreakType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct StmtContinueType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct StmtThrowType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct StmtTryType {
    pub id: NodeId,
    pub pos: Position,
    pub try_block: Box<Stmt>,
    pub catch_blocks: Vec<CatchBlock>,
    pub finally_block: Option<FinallyBlock>,
}

#[derive(Clone, Debug)]
pub struct CatchBlock {
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub ty: RefCell<Option<BuiltinType>>,
    pub var: RefCell<Option<VarId>>,
    pub block: Box<Stmt>,
}

impl CatchBlock {
    pub fn new(name: Name, pos: Position, data_type: Type, block: Box<Stmt>) -> CatchBlock {
        CatchBlock {
            name: name,
            pos: pos,
            data_type: data_type,
            ty: RefCell::new(None),
            var: RefCell::new(None),
            block: block
        }
    }

    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }

    pub fn set_var(&self, var: VarId) {
        *self.var.borrow_mut() = Some(var);
    }

    pub fn var(&self) -> VarId {
        self.var.borrow().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct FinallyBlock {
    pub block: Box<Stmt>,
    pub offset: RefCell<Option<i32>>,
}

impl FinallyBlock {
    pub fn new(block: Box<Stmt>) -> FinallyBlock {
        FinallyBlock {
            block: block,
            offset: RefCell::new(None),
        }
    }

    pub fn offset(&self) -> i32 {
        self.offset.borrow().unwrap()
    }

    pub fn set_offset(&self, offset: i32) {
        *self.offset.borrow_mut() = Some(offset);
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
    BitNot
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Plus => "+",
            UnOp::Neg => "-",
            UnOp::Not => "!",
            UnOp::BitNot => "~"
        }
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
    Is,
    IsNot,
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
            CmpOp::Is => "===",
            CmpOp::IsNot => "!==",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
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

    pub fn is_compare(&self) -> bool {
        match *self {
            BinOp::Cmp(cmp) if cmp != CmpOp::Is && cmp != CmpOp::IsNot => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    ExprUn(ExprUnType),
    ExprBin(ExprBinType),
    ExprLitInt(ExprLitIntType),
    ExprLitStr(ExprLitStrType),
    ExprLitBool(ExprLitBoolType),
    ExprIdent(ExprIdentType),
    ExprCall(ExprCallType),
    ExprSuperCall(ExprSuperCallType),
    ExprAssign(ExprAssignType),
    ExprField(ExprFieldType),
    ExprSelf(ExprSelfType),
    ExprNil(ExprNilType),
    ExprArray(ExprArrayType),
}

impl Expr {
    pub fn create_un(id: NodeId, pos: Position, op: UnOp, opnd: Box<Expr>) -> Expr {
        Expr::ExprUn(ExprUnType {
            id: id,
            pos: pos,
            op: op,
            opnd: opnd,
            ty: RefCell::new(None),
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
            ty: RefCell::new(None),
        })
    }

    pub fn create_array(id: NodeId, pos: Position,
                        object: Box<Expr>, index: Box<Expr>) -> Expr {
        Expr::ExprArray(ExprArrayType {
            id: id,
            pos: pos,
            object: object,
            index: index,
            ty: RefCell::new(None),
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

    pub fn create_this(id: NodeId, pos: Position) -> Expr {
        Expr::ExprSelf(ExprSelfType {
            id: id,
            pos: pos,
            ty: RefCell::new(None),
        })
    }

    pub fn create_nil(id: NodeId, pos: Position) -> Expr {
        Expr::ExprNil(ExprNilType {
            id: id,
            pos: pos,
            ty: RefCell::new(None),
        })
    }

    pub fn create_ident(id: NodeId, pos: Position, name: Name) -> Expr {
        Expr::ExprIdent(ExprIdentType {
            id: id,
            pos: pos,
            name: name,
            info: RefCell::new(None),
            ty: RefCell::new(None),
        })
    }

    pub fn create_call(id: NodeId, pos: Position, name: Name,
                       with_self: bool, args: Vec<Box<Expr>>) -> Expr {
        Expr::ExprCall(ExprCallType {
            id: id,
            pos: pos,
            name: name,
            args: args,
            with_self: with_self,
            ty: RefCell::new(None),
        })
    }

    pub fn create_super_call(id: NodeId, pos: Position, args: Vec<Box<Expr>>) -> Expr {
        Expr::ExprSuperCall(ExprSuperCallType {
            id: id,
            pos: pos,
            args: args,
            fct_id: RefCell::new(None),
            cls_id: RefCell::new(None),
        })
    }

    pub fn create_assign(id: NodeId, pos: Position,
                         lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprAssign(ExprAssignType {
            id: id,
            pos: pos,
            lhs: lhs,
            rhs: rhs,
            ty: RefCell::new(None),
        })
    }

    pub fn create_field(id: NodeId, pos: Position,
                       object: Box<Expr>, name: Name) -> Expr {
        Expr::ExprField(ExprFieldType {
            id: id,
            pos: pos,
            object: object,
            name: name,
            info: RefCell::new(None),
            ty: RefCell::new(None),
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

    pub fn is_lit_true(&self) -> bool {
        match *self {
            Expr::ExprLitBool(ref lit) if lit.value => true,
            _ => false,
        }
    }

    pub fn to_field(&self) -> Option<&ExprFieldType> {
        match *self {
            Expr::ExprField(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_field(&self) -> bool {
        match *self {
            Expr::ExprField(_) => true,
            _ => false
        }
    }

    pub fn to_array(&self) -> Option<&ExprArrayType> {
        match *self {
            Expr::ExprArray(ref val) => Some(val),
            _ => None
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Expr::ExprArray(_) => true,
            _ => false
        }
    }

    pub fn is_this(&self) -> bool {
        match *self {
            Expr::ExprSelf(_) => true,
            _ => false
        }
    }

    pub fn is_nil(&self) -> bool {
        match *self {
            Expr::ExprNil(_) => true,
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
            Expr::ExprCall(ref val) => val.id,
            Expr::ExprSuperCall(ref val) => val.id,
            Expr::ExprField(ref val) => val.id,
            Expr::ExprSelf(ref val) => val.id,
            Expr::ExprNil(ref val) => val.id,
            Expr::ExprArray(ref val) => val.id,
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match *self {
            Expr::ExprUn(ref val) => val.ty(),
            Expr::ExprBin(ref val) => val.ty(),
            Expr::ExprLitInt(ref val) => BuiltinType::Int,
            Expr::ExprLitStr(ref val) => BuiltinType::Str,
            Expr::ExprLitBool(ref val) => BuiltinType::Bool,
            Expr::ExprIdent(ref val) => val.ty(),
            Expr::ExprAssign(ref val) => val.ty(),
            Expr::ExprCall(ref val) => val.ty(),
            Expr::ExprSuperCall(ref val) => BuiltinType::Unit,
            Expr::ExprField(ref val) => val.ty(),
            Expr::ExprSelf(ref val) => val.ty(),
            Expr::ExprNil(ref val) => val.ty(),
            Expr::ExprArray(ref val) => val.ty(),
        }
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        match *self {
            Expr::ExprUn(ref val) => val.set_ty(ty),
            Expr::ExprBin(ref val) => val.set_ty(ty),
            Expr::ExprLitInt(ref val) => panic!("unimplemented"),
            Expr::ExprLitStr(ref val) => panic!("unimplemented"),
            Expr::ExprLitBool(ref val) => panic!("unimplemented"),
            Expr::ExprIdent(ref val) => val.set_ty(ty),
            Expr::ExprAssign(ref val) => val.set_ty(ty),
            Expr::ExprCall(ref val) => val.set_ty(ty),
            Expr::ExprSuperCall(ref val) => panic!("unimplemented"),
            Expr::ExprField(ref val) => val.set_ty(ty),
            Expr::ExprSelf(ref val) => val.set_ty(ty),
            Expr::ExprNil(ref val) => val.set_ty(ty),
            Expr::ExprArray(ref val) => val.set_ty(ty),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprSuperCallType {
    pub id: NodeId,
    pub pos: Position,
    pub args: Vec<Box<Expr>>,
    pub fct_id: RefCell<Option<FctId>>,
    pub cls_id: RefCell<Option<ClassId>>,
}

impl ExprSuperCallType {
    pub fn fct_id(&self) -> FctId {
        self.fct_id.borrow().unwrap()
    }

    pub fn set_fct_id(&self, id: FctId) {
        *self.fct_id.borrow_mut() = Some(id);
    }

    pub fn class_id(&self) -> ClassId {
        self.cls_id.borrow().unwrap()
    }

    pub fn set_class_id(&self, id: ClassId) {
        *self.cls_id.borrow_mut() = Some(id);
    }
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub pos: Position,

    pub op: UnOp,
    pub opnd: Box<Expr>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprUnType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub pos: Position,

    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprBinType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprArrayType {
    pub id: NodeId,
    pub pos: Position,

    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprArrayType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub pos: Position,

    pub value: i32,
}

#[derive(Clone, Debug)]
pub struct ExprLitStrType {
    pub id: NodeId,
    pub pos: Position,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitBoolType {
    pub id: NodeId,
    pub pos: Position,

    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub pos: Position,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprSelfType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprNilType {
    pub id: NodeId,
    pub pos: Position,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprNilType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
    pub info: RefCell<Option<IdentType>>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprIdentType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }

    pub fn ident_type(&self) -> IdentType {
        self.info.borrow().unwrap()
    }

    pub fn set_field(&self, cls: ClassId, field: FieldId) {
        let info = IdentType::Field(cls, field);
        *self.info.borrow_mut() = Some(info);
    }

    pub fn set_var(&self, var: VarId) {
        let info = IdentType::Var(var);
        *self.info.borrow_mut() = Some(info);
    }

    pub fn var(&self) -> VarId {
        match self.ident_type() {
            IdentType::Var(var_id) => var_id,
            _ => unreachable!("var expected")
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
    pub with_self: bool,
    pub args: Vec<Box<Expr>>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprCallType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprAssignType {
    pub id: NodeId,
    pub pos: Position,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprAssignType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ExprFieldType {
    pub id: NodeId,
    pub pos: Position,

    pub object: Box<Expr>,
    pub name: Name,
    pub info: RefCell<Option<(ClassId, FieldId)>>,
    pub ty: RefCell<Option<BuiltinType>>,
}

impl ExprFieldType {
    pub fn ty(&self) -> BuiltinType {
        self.ty.borrow().unwrap()
    }

    pub fn set_ty(&self, ty: BuiltinType) {
        *self.ty.borrow_mut() = Some(ty);
    }

    pub fn has_cls_and_field(&self) -> bool {
        self.info.borrow().is_some()
    }

    pub fn cls_and_field(&self) -> (ClassId, FieldId) {
        self.info.borrow().unwrap()
    }

    pub fn set_cls_and_field(&self, class: ClassId, field: FieldId) {
        *self.info.borrow_mut() = Some((class, field));
    }
}
