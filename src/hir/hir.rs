use self::Type::*;

use hir::graph::Graph;
use hir::instruction::Instr;

pub struct VarId(pub usize);
pub struct TempId(pub usize);

pub struct HIR {
    graph: Graph<(), Instr>,

    // arguments and local variables
    vars: Vec<VarDecl>,

    // temporary variables, only assigned once
    temps: Vec<TempDecl>,
}

pub struct VarDecl {
    ty: Type
}

pub struct TempDecl {
    ty: Type
}

pub enum Type {
    TyBool,
    TyInt(IntType),
    TyPtr(Box<Type>),
    TyStruct(StructType)
}

impl Type {
    pub fn to_string(&self) -> String {
        match *self {
            TyBool => "bool".into(),
            TyInt(inttype) => inttype.to_string().into(),
            TyPtr(ref subtype) => format!("*{}", subtype.to_string()),
            TyStruct(_) => "{struct}".into()
        }
    }
}

#[derive(Copy, Clone)]
pub enum IntType {
    UInt8,
    Int32,
    Int // size depending on architecture
}

impl IntType {
    pub fn to_string(&self) -> &'static str {
        match *self {
            IntType::UInt8 => "u8",
            IntType::Int32 => "i32",
            IntType::Int => "int"
        }
    }
}

struct StructType {
    size: usize,
    elems: Vec<StructElem>,
}

struct StructElem {
    name: String,
    ty: Type,
    offset: usize
}
