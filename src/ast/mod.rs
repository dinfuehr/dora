use std::fmt;
use std::ops::Index;
use std::slice::Iter;

use ast::Elem::*;
use lexer::position::Position;
use lexer::token::{FloatSuffix, IntSuffix};
use interner::{Interner, Name};

pub mod visit;
pub mod dump;

#[derive(Clone, Debug)]
pub struct Ast {
    pub files: Vec<File>,
    pub next_id: NodeId,
}

impl Ast {
    pub fn new() -> Ast {
        Ast {
            files: Vec::new(),
            next_id: NodeId(1),
        }
    }

    pub fn generate_id(&mut self) -> NodeId {
        let ret = self.next_id;
        self.next_id = NodeId(ret.0 + 1);

        ret
    }

    #[cfg(test)]
    pub fn fct0(&self) -> &Function {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_function()
                .unwrap()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &Function {
        self.files
            .last()
            .unwrap()
            .elements[index]
                .to_function()
                .unwrap()
    }

    #[cfg(test)]
    pub fn cls0(&self) -> &Class {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_class()
                .unwrap()
    }

    #[cfg(test)]
    pub fn cls(&self, index: usize) -> &Class {
        self.files
            .last()
            .unwrap()
            .elements[index]
                .to_class()
                .unwrap()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &Struct {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_struct()
                .unwrap()
    }

    #[cfg(test)]
    pub fn trai(&self, index: usize) -> &Trait {
        self.files
            .last()
            .unwrap()
            .elements[index]
                .to_trait()
                .unwrap()
    }

    #[cfg(test)]
    pub fn trait0(&self) -> &Trait {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_trait()
                .unwrap()
    }

    #[cfg(test)]
    pub fn impl0(&self) -> &Impl {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_impl()
                .unwrap()
    }

    #[cfg(test)]
    pub fn global0(&self) -> &Global {
        self.files
            .last()
            .unwrap()
            .elements[0]
                .to_global()
                .unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct File {
    pub path: String,
    pub elements: Vec<Elem>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub enum Elem {
    ElemFunction(Function),
    ElemClass(Class),
    ElemStruct(Struct),
    ElemTrait(Trait),
    ElemImpl(Impl),
    ElemGlobal(Global),
}

impl Elem {
    pub fn id(&self) -> NodeId {
        match self {
            &ElemFunction(ref fct) => fct.id,
            &ElemClass(ref class) => class.id,
            &ElemStruct(ref s) => s.id,
            &ElemTrait(ref t) => t.id,
            &ElemImpl(ref i) => i.id,
            &ElemGlobal(ref g) => g.id,
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &ElemFunction(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_class(&self) -> Option<&Class> {
        match self {
            &ElemClass(ref class) => Some(class),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&Struct> {
        match self {
            &ElemStruct(ref struc) => Some(struc),
            _ => None,
        }
    }

    pub fn to_trait(&self) -> Option<&Trait> {
        match self {
            &ElemTrait(ref trai) => Some(trai),
            _ => None,
        }
    }

    pub fn to_impl(&self) -> Option<&Impl> {
        match self {
            &ElemImpl(ref ximpl) => Some(ximpl),
            _ => None,
        }
    }

    pub fn to_global(&self) -> Option<&Global> {
        match self {
            &ElemGlobal(ref global) => Some(global),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Global {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub reassignable: bool,
    pub data_type: Type,
    pub expr: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    TypeSelf(TypeSelfType),
    TypeBasic(TypeBasicType),
    TypeTuple(TypeTupleType),
    TypePtr(TypePtrType),
    TypeArray(TypeArrayType),
}

#[derive(Clone, Debug)]
pub struct TypeSelfType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub pos: Position,
    pub subtypes: Vec<Box<Type>>,
}

#[derive(Clone, Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub params: Vec<Box<Type>>,
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
    pub fn create_self(id: NodeId, pos: Position) -> Type {
        Type::TypeSelf(TypeSelfType { id: id, pos: pos })
    }

    pub fn create_basic(id: NodeId, pos: Position, name: Name, params: Vec<Box<Type>>) -> Type {
        Type::TypeBasic(TypeBasicType {
                            id: id,
                            pos: pos,
                            name: name,
                            params: params,
                        })
    }

    pub fn create_ptr(id: NodeId, pos: Position, subtype: Box<Type>) -> Type {
        Type::TypePtr(TypePtrType {
                          id: id,
                          pos: pos,
                          subtype: subtype,
                      })
    }

    pub fn create_array(id: NodeId, pos: Position, subtype: Box<Type>) -> Type {
        Type::TypeArray(TypeArrayType {
                            id: id,
                            pos: pos,
                            subtype: subtype,
                        })
    }

    pub fn create_tuple(id: NodeId, pos: Position, subtypes: Vec<Box<Type>>) -> Type {
        Type::TypeTuple(TypeTupleType {
                            id: id,
                            pos: pos,
                            subtypes: subtypes,
                        })
    }

    pub fn to_basic(&self) -> Option<&TypeBasicType> {
        match *self {
            Type::TypeBasic(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_ptr(&self) -> Option<&TypePtrType> {
        match *self {
            Type::TypePtr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_array(&self) -> Option<&TypeArrayType> {
        match *self {
            Type::TypeArray(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_tuple(&self) -> Option<&TypeTupleType> {
        match *self {
            Type::TypeTuple(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_string(&self, interner: &Interner) -> String {
        match *self {
            Type::TypeSelf(_) => "Self".into(),
            Type::TypeBasic(ref val) => format!("{}", *interner.str(val.name)),

            Type::TypeTuple(ref val) => {
                let types: Vec<String> = val.subtypes
                    .iter()
                    .map(|t| t.to_string(interner))
                    .collect();

                format!("({})", types.join(", "))
            }

            Type::TypePtr(ref val) => format!("*{}", val.subtype.to_string(interner)),

            Type::TypeArray(ref val) => format!("[{}]", val.subtype.to_string(interner)),
        }
    }

    pub fn pos(&self) -> Position {
        match *self {
            Type::TypeSelf(ref val) => val.pos,
            Type::TypeBasic(ref val) => val.pos,
            Type::TypeTuple(ref val) => val.pos,
            Type::TypePtr(ref val) => val.pos,
            Type::TypeArray(ref val) => val.pos,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Type::TypeSelf(ref val) => val.id,
            Type::TypeBasic(ref val) => val.id,
            Type::TypeTuple(ref val) => val.id,
            Type::TypePtr(ref val) => val.id,
            Type::TypeArray(ref val) => val.id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub id: NodeId,
    pub trait_name: Name,
    pub class_name: Name,
    pub pos: Position,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub parent_class: Option<ParentClass>,
    pub has_open: bool,
    pub internal: bool,
    pub primary_ctor: bool,

    pub ctors: Vec<Function>,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: Name,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct PrimaryCtorParam {
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub field: bool,
    pub reassignable: bool,
}

#[derive(Clone, Debug)]
pub struct ParentClass {
    pub name: Name,
    pub pos: Position,
    pub params: Vec<Box<Expr>>,
}

impl ParentClass {
    pub fn new(name: Name, pos: Position, params: Vec<Box<Expr>>) -> ParentClass {
        ParentClass {
            name: name,
            pos: pos,
            params: params,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub primary_ctor: bool,
    pub expr: Option<Box<Expr>>,
    pub reassignable: bool,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub method: bool,
    pub has_open: bool,
    pub has_override: bool,
    pub has_final: bool,
    pub is_pub: bool,
    pub is_static: bool,
    pub internal: bool,
    pub ctor: CtorType,

    pub params: Vec<Param>,
    pub throws: bool,

    pub return_type: Option<Type>,
    pub block: Option<Box<Stmt>>,
}

impl Function {
    pub fn block(&self) -> &Stmt {
        self.block.as_ref().unwrap()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CtorType {
    None,
    Primary,
    Secondary,
}

impl CtorType {
    pub fn is(&self) -> bool {
        match *self {
            CtorType::Primary | CtorType::Secondary => true,
            _ => false,
        }
    }

    pub fn is_primary(&self) -> bool {
        match *self {
            CtorType::Primary => true,
            _ => false,
        }
    }

    pub fn is_secondary(&self) -> bool {
        match *self {
            CtorType::Secondary => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Modifiers(Vec<ModifierElement>);

impl Modifiers {
    pub fn new() -> Modifiers {
        Modifiers(Vec::new())
    }

    pub fn contains(&self, modifier: Modifier) -> bool {
        self.0
            .iter()
            .find(|el| el.value == modifier)
            .is_some()
    }

    pub fn add(&mut self, modifier: Modifier, pos: Position) {
        self.0.push(ModifierElement {
                        value: modifier,
                        pos: pos,
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
pub enum Modifier {
    Override,
    Open,
    Final,
    Internal,
    Pub,
    Static,
}

impl Modifier {
    pub fn name(&self) -> &'static str {
        match *self {
            Modifier::Open => "open",
            Modifier::Override => "override",
            Modifier::Final => "final",
            Modifier::Internal => "internal",
            Modifier::Pub => "pub",
            Modifier::Static => "static",
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
    StmtDo(StmtDoType),
    StmtSpawn(StmtSpawnType),
}

impl Stmt {
    pub fn create_var(id: NodeId,
                      pos: Position,
                      name: Name,
                      reassignable: bool,
                      data_type: Option<Type>,
                      expr: Option<Box<Expr>>)
                      -> Stmt {
        Stmt::StmtVar(StmtVarType {
                          id: id,
                          pos: pos,
                          name: name,
                          reassignable: reassignable,
                          data_type: data_type,
                          expr: expr,
                      })
    }

    pub fn create_while(id: NodeId, pos: Position, cond: Box<Expr>, block: Box<Stmt>) -> Stmt {
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

    pub fn create_if(id: NodeId,
                     pos: Position,
                     cond: Box<Expr>,
                     then_block: Box<Stmt>,
                     else_block: Option<Box<Stmt>>)
                     -> Stmt {
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
        Stmt::StmtBreak(StmtBreakType { id: id, pos: pos })
    }

    pub fn create_continue(id: NodeId, pos: Position) -> Stmt {
        Stmt::StmtContinue(StmtContinueType { id: id, pos: pos })
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

    pub fn create_do(id: NodeId,
                     pos: Position,
                     do_block: Box<Stmt>,
                     catch_blocks: Vec<CatchBlock>,
                     finally_block: Option<FinallyBlock>)
                     -> Stmt {
        Stmt::StmtDo(StmtDoType {
                         id: id,
                         pos: pos,
                         do_block: do_block,
                         catch_blocks: catch_blocks,
                         finally_block: finally_block,
                     })
    }

    pub fn create_spawn(id: NodeId, pos: Position, expr: Box<Expr>) -> Stmt {
        Stmt::StmtSpawn(StmtSpawnType {
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
            Stmt::StmtThrow(ref stmt) => stmt.id,
            Stmt::StmtDo(ref stmt) => stmt.id,
            Stmt::StmtSpawn(ref stmt) => stmt.id,
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
            Stmt::StmtDo(ref stmt) => stmt.pos,
            Stmt::StmtSpawn(ref stmt) => stmt.pos,
        }
    }

    pub fn to_throw(&self) -> Option<&StmtThrowType> {
        match *self {
            Stmt::StmtThrow(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_throw(&self) -> bool {
        match *self {
            Stmt::StmtThrow(_) => true,
            _ => false,
        }
    }

    pub fn to_do(&self) -> Option<&StmtDoType> {
        match *self {
            Stmt::StmtDo(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_try(&self) -> bool {
        match *self {
            Stmt::StmtDo(_) => true,
            _ => false,
        }
    }

    pub fn to_var(&self) -> Option<&StmtVarType> {
        match *self {
            Stmt::StmtVar(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            Stmt::StmtVar(_) => true,
            _ => false,
        }
    }

    pub fn to_while(&self) -> Option<&StmtWhileType> {
        match *self {
            Stmt::StmtWhile(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            Stmt::StmtWhile(_) => true,
            _ => false,
        }
    }

    pub fn to_loop(&self) -> Option<&StmtLoopType> {
        match *self {
            Stmt::StmtLoop(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_loop(&self) -> bool {
        match *self {
            Stmt::StmtLoop(_) => true,
            _ => false,
        }
    }

    pub fn to_if(&self) -> Option<&StmtIfType> {
        match *self {
            Stmt::StmtIf(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_if(&self) -> bool {
        match *self {
            Stmt::StmtIf(_) => true,
            _ => false,
        }
    }

    pub fn to_expr(&self) -> Option<&StmtExprType> {
        match *self {
            Stmt::StmtExpr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            Stmt::StmtExpr(_) => true,
            _ => false,
        }
    }

    pub fn to_block(&self) -> Option<&StmtBlockType> {
        match *self {
            Stmt::StmtBlock(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_block(&self) -> bool {
        match *self {
            Stmt::StmtBlock(_) => true,
            _ => false,
        }
    }

    pub fn to_return(&self) -> Option<&StmtReturnType> {
        match *self {
            Stmt::StmtReturn(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            Stmt::StmtReturn(_) => true,
            _ => false,
        }
    }

    pub fn to_break(&self) -> Option<&StmtBreakType> {
        match *self {
            Stmt::StmtBreak(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            Stmt::StmtBreak(_) => true,
            _ => false,
        }
    }

    pub fn to_continue(&self) -> Option<&StmtContinueType> {
        match *self {
            Stmt::StmtContinue(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            Stmt::StmtContinue(_) => true,
            _ => false,
        }
    }

    pub fn to_spawn(&self) -> Option<&StmtSpawnType> {
        match *self {
            Stmt::StmtSpawn(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_spawn(&self) -> bool {
        match *self {
            Stmt::StmtSpawn(_) => true,
            _ => false,
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
pub struct StmtDoType {
    pub id: NodeId,
    pub pos: Position,
    pub do_block: Box<Stmt>,
    pub catch_blocks: Vec<CatchBlock>,
    pub finally_block: Option<FinallyBlock>,
}

#[derive(Clone, Debug)]
pub struct StmtSpawnType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CatchBlock {
    pub id: NodeId,
    pub name: Name,
    pub pos: Position,
    pub data_type: Type,
    pub block: Box<Stmt>,
}

impl CatchBlock {
    pub fn new(id: NodeId,
               name: Name,
               pos: Position,
               data_type: Type,
               block: Box<Stmt>)
               -> CatchBlock {
        CatchBlock {
            id: id,
            name: name,
            pos: pos,
            data_type: data_type,
            block: block,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FinallyBlock {
    pub block: Box<Stmt>,
}

impl FinallyBlock {
    pub fn new(block: Box<Stmt>) -> FinallyBlock {
        FinallyBlock { block: block }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Plus => "+",
            UnOp::Neg => "-",
            UnOp::Not => "!",
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
    ShiftL,
    ShiftR,
    UnShiftR,
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
            BinOp::BitXor => "^",
            BinOp::ShiftL => "<<",
            BinOp::ShiftR => ">>",
            BinOp::UnShiftR => ">>>",
        }
    }

    pub fn is_compare(&self) -> bool {
        match *self {
            BinOp::Cmp(cmp) if cmp != CmpOp::Is && cmp != CmpOp::IsNot => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    ExprUn(ExprUnType),
    ExprBin(ExprBinType),
    ExprLitChar(ExprLitCharType),
    ExprLitInt(ExprLitIntType),
    ExprLitFloat(ExprLitFloatType),
    ExprLitStr(ExprLitStrType),
    ExprLitBool(ExprLitBoolType),
    ExprLitStruct(ExprLitStructType),
    ExprIdent(ExprIdentType),
    ExprCall(ExprCallType),
    ExprDelegation(ExprDelegationType),
    ExprAssign(ExprAssignType),
    ExprField(ExprFieldType),
    ExprSelf(ExprSelfType),
    ExprSuper(ExprSuperType),
    ExprNil(ExprNilType),
    ExprArray(ExprArrayType),
    ExprConv(ExprConvType),
    ExprTry(ExprTryType),
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

    pub fn create_try(id: NodeId, pos: Position, expr: Box<Expr>, mode: TryMode) -> Expr {
        Expr::ExprTry(ExprTryType {
                          id: id,
                          pos: pos,
                          expr: expr,
                          mode: mode,
                      })
    }

    pub fn create_bin(id: NodeId,
                      pos: Position,
                      op: BinOp,
                      lhs: Box<Expr>,
                      rhs: Box<Expr>)
                      -> Expr {
        Expr::ExprBin(ExprBinType {
                          id: id,
                          pos: pos,
                          op: op,
                          lhs: lhs,
                          rhs: rhs,
                      })
    }

    pub fn create_conv(id: NodeId,
                       pos: Position,
                       object: Box<Expr>,
                       data_type: Box<Type>,
                       is: bool)
                       -> Expr {
        Expr::ExprConv(ExprConvType {
                           id: id,
                           pos: pos,
                           object: object,
                           data_type: data_type,
                           is: is,
                       })
    }

    pub fn create_array(id: NodeId, pos: Position, object: Box<Expr>, index: Box<Expr>) -> Expr {
        Expr::ExprArray(ExprArrayType {
                            id: id,
                            pos: pos,
                            object: object,
                            index: index,
                        })
    }

    pub fn create_lit_char(id: NodeId, pos: Position, value: char) -> Expr {
        Expr::ExprLitChar(ExprLitCharType {
                              id: id,
                              pos: pos,
                              value: value,
                          })
    }

    pub fn create_lit_int(id: NodeId, pos: Position, value: u64, suffix: IntSuffix) -> Expr {
        Expr::ExprLitInt(ExprLitIntType {
                             id: id,
                             pos: pos,
                             value: value,
                             suffix: suffix,
                         })
    }

    pub fn create_lit_float(id: NodeId, pos: Position, value: f64, suffix: FloatSuffix) -> Expr {
        Expr::ExprLitFloat(ExprLitFloatType {
                               id: id,
                               pos: pos,
                               value: value,
                               suffix: suffix,
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

    pub fn create_lit_struct(id: NodeId, pos: Position, path: Path, args: Vec<StructArg>) -> Expr {
        Expr::ExprLitStruct(ExprLitStructType {
                                id: id,
                                pos: pos,
                                path: path,
                                args: args,
                            })
    }

    pub fn create_this(id: NodeId, pos: Position) -> Expr {
        Expr::ExprSelf(ExprSelfType { id: id, pos: pos })
    }

    pub fn create_super(id: NodeId, pos: Position) -> Expr {
        Expr::ExprSuper(ExprSuperType { id: id, pos: pos })
    }

    pub fn create_nil(id: NodeId, pos: Position) -> Expr {
        Expr::ExprNil(ExprNilType { id: id, pos: pos })
    }

    pub fn create_ident(id: NodeId,
                        pos: Position,
                        name: Name,
                        type_params: Option<Vec<Type>>)
                        -> Expr {
        Expr::ExprIdent(ExprIdentType {
                            id: id,
                            pos: pos,
                            name: name,
                            type_params: type_params,
                        })
    }

    pub fn create_call(id: NodeId,
                       pos: Position,
                       path: Path,
                       object: Option<Box<Expr>>,
                       args: Vec<Box<Expr>>,
                       type_params: Option<Vec<Type>>)
                       -> Expr {
        Expr::ExprCall(ExprCallType {
                           id: id,
                           pos: pos,
                           path: path,
                           args: args,
                           object: object,
                           type_params: type_params,
                       })
    }

    pub fn create_delegation(id: NodeId,
                             pos: Position,
                             ty: DelegationType,
                             args: Vec<Box<Expr>>)
                             -> Expr {
        Expr::ExprDelegation(ExprDelegationType {
                                 id: id,
                                 pos: pos,
                                 ty: ty,
                                 args: args,
                             })
    }

    pub fn create_assign(id: NodeId, pos: Position, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
        Expr::ExprAssign(ExprAssignType {
                             id: id,
                             pos: pos,
                             lhs: lhs,
                             rhs: rhs,
                         })
    }

    pub fn create_field(id: NodeId, pos: Position, object: Box<Expr>, name: Name) -> Expr {
        Expr::ExprField(ExprFieldType {
                            id: id,
                            pos: pos,
                            object: object,
                            name: name,
                        })
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            Expr::ExprUn(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_un(&self) -> bool {
        match *self {
            Expr::ExprUn(_) => true,
            _ => false,
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            Expr::ExprBin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            Expr::ExprBin(_) => true,
            _ => false,
        }
    }

    pub fn to_assign(&self) -> Option<&ExprAssignType> {
        match *self {
            Expr::ExprAssign(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_assign(&self) -> bool {
        match *self {
            Expr::ExprAssign(_) => true,
            _ => false,
        }
    }

    pub fn to_ident(&self) -> Option<&ExprIdentType> {
        match *self {
            Expr::ExprIdent(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            Expr::ExprIdent(_) => true,
            _ => false,
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            Expr::ExprCall(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            Expr::ExprCall(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_char(&self) -> Option<&ExprLitCharType> {
        match *self {
            Expr::ExprLitChar(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_char(&self) -> bool {
        match *self {
            Expr::ExprLitChar(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_int(&self) -> Option<&ExprLitIntType> {
        match *self {
            Expr::ExprLitInt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            Expr::ExprLitInt(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_float(&self) -> Option<&ExprLitFloatType> {
        match *self {
            Expr::ExprLitFloat(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_float(&self) -> bool {
        match *self {
            Expr::ExprLitFloat(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_struct(&self) -> Option<&ExprLitStructType> {
        match *self {
            Expr::ExprLitStruct(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_struct(&self) -> bool {
        match *self {
            Expr::ExprLitStruct(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_str(&self) -> Option<&ExprLitStrType> {
        match *self {
            Expr::ExprLitStr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_lit_bool(&self) -> Option<&ExprLitBoolType> {
        match *self {
            Expr::ExprLitBool(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_bool(&self) -> bool {
        match *self {
            Expr::ExprLitBool(_) => true,
            _ => false,
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
            _ => None,
        }
    }

    pub fn is_field(&self) -> bool {
        match *self {
            Expr::ExprField(_) => true,
            _ => false,
        }
    }

    pub fn to_array(&self) -> Option<&ExprArrayType> {
        match *self {
            Expr::ExprArray(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Expr::ExprArray(_) => true,
            _ => false,
        }
    }

    pub fn to_delegation(&self) -> Option<&ExprDelegationType> {
        match *self {
            Expr::ExprDelegation(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_delegation(&self) -> bool {
        match *self {
            Expr::ExprDelegation(_) => true,
            _ => false,
        }
    }

    pub fn is_this(&self) -> bool {
        match *self {
            Expr::ExprSelf(_) => true,
            _ => false,
        }
    }

    pub fn is_super(&self) -> bool {
        match *self {
            Expr::ExprSuper(_) => true,
            _ => false,
        }
    }

    pub fn to_super(&self) -> Option<&ExprSuperType> {
        match *self {
            Expr::ExprSuper(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_nil(&self) -> bool {
        match *self {
            Expr::ExprNil(_) => true,
            _ => false,
        }
    }

    pub fn to_conv(&self) -> Option<&ExprConvType> {
        match *self {
            Expr::ExprConv(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_conv(&self) -> bool {
        match *self {
            Expr::ExprConv(_) => true,
            _ => false,
        }
    }

    pub fn to_try(&self) -> Option<&ExprTryType> {
        match *self {
            Expr::ExprTry(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_try(&self) -> bool {
        match *self {
            Expr::ExprTry(_) => true,
            _ => false,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            Expr::ExprUn(ref val) => val.id,
            Expr::ExprBin(ref val) => val.id,
            Expr::ExprLitChar(ref val) => val.id,
            Expr::ExprLitInt(ref val) => val.id,
            Expr::ExprLitFloat(ref val) => val.id,
            Expr::ExprLitStr(ref val) => val.id,
            Expr::ExprLitBool(ref val) => val.id,
            Expr::ExprLitStruct(ref val) => val.id,
            Expr::ExprIdent(ref val) => val.id,
            Expr::ExprAssign(ref val) => val.id,
            Expr::ExprCall(ref val) => val.id,
            Expr::ExprDelegation(ref val) => val.id,
            Expr::ExprField(ref val) => val.id,
            Expr::ExprSelf(ref val) => val.id,
            Expr::ExprSuper(ref val) => val.id,
            Expr::ExprNil(ref val) => val.id,
            Expr::ExprArray(ref val) => val.id,
            Expr::ExprConv(ref val) => val.id,
            Expr::ExprTry(ref val) => val.id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprLitStructType {
    pub id: NodeId,
    pub pos: Position,
    pub path: Path,
    pub args: Vec<StructArg>,
}

#[derive(Clone, Debug)]
pub struct StructArg {
    pub id: NodeId,
    pub pos: Position,
    pub name: Name,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprConvType {
    pub id: NodeId,
    pub pos: Position,
    pub object: Box<Expr>,
    pub is: bool,
    pub data_type: Box<Type>,
}

#[derive(Clone, Debug)]
pub struct ExprTryType {
    pub id: NodeId,
    pub pos: Position,
    pub expr: Box<Expr>,
    pub mode: TryMode,
}

#[derive(Clone, Debug)]
pub enum TryMode {
    Normal,
    Else(Box<Expr>),
    Opt,
    Force,
}

impl TryMode {
    pub fn is_normal(&self) -> bool {
        match self {
            &TryMode::Normal => true,
            _ => false,
        }
    }

    pub fn is_else(&self) -> bool {
        match self {
            &TryMode::Else(_) => true,
            _ => false,
        }
    }

    pub fn is_force(&self) -> bool {
        match self {
            &TryMode::Force => true,
            _ => false,
        }
    }

    pub fn is_opt(&self) -> bool {
        match self {
            &TryMode::Opt => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprDelegationType {
    pub id: NodeId,
    pub pos: Position,
    pub ty: DelegationType, // true for this class, false for super class
    pub args: Vec<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DelegationType {
    This,
    Super,
}

impl DelegationType {
    pub fn is_this(&self) -> bool {
        match *self {
            DelegationType::This => true,
            _ => false,
        }
    }

    pub fn is_super(&self) -> bool {
        match *self {
            DelegationType::Super => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub pos: Position,

    pub op: UnOp,
    pub opnd: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub pos: Position,

    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprArrayType {
    pub id: NodeId,
    pub pos: Position,

    pub object: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprLitCharType {
    pub id: NodeId,
    pub pos: Position,

    pub value: char,
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub pos: Position,

    pub value: u64,
    pub suffix: IntSuffix,
}

#[derive(Clone, Debug)]
pub struct ExprLitFloatType {
    pub id: NodeId,
    pub pos: Position,

    pub value: f64,
    pub suffix: FloatSuffix,
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
pub struct ExprSuperType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct ExprNilType {
    pub id: NodeId,
    pub pos: Position,
}

#[derive(Clone, Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub pos: Position,

    pub name: Name,
    pub type_params: Option<Vec<Type>>,
}

#[derive(Clone, Debug)]
pub struct Path {
    pub path: Vec<Name>,
}

impl Path {
    pub fn new(name: Name) -> Path {
        Path { path: vec![name] }
    }

    pub fn name(&self) -> Name {
        assert_eq!(1, self.path.len());

        self.path[0]
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }
}

impl Index<usize> for Path {
    type Output = Name;

    fn index(&self, idx: usize) -> &Name {
        &self.path[idx]
    }
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub pos: Position,

    pub path: Path,
    pub object: Option<Box<Expr>>,
    pub args: Vec<Box<Expr>>,
    pub type_params: Option<Vec<Type>>,
}

#[derive(Clone, Debug)]
pub struct ExprAssignType {
    pub id: NodeId,
    pub pos: Position,

    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprFieldType {
    pub id: NodeId,
    pub pos: Position,

    pub object: Box<Expr>,
    pub name: Name,
}
