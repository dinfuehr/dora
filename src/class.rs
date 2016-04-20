use std::collections::HashMap;

use ast;
use ctxt::FctId;
use interner::Name;
use ty::BuiltinType;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ClassId(pub usize);

#[derive(Debug)]
pub struct Class<'ast> {
    pub id: ClassId,
    pub name: Name,
    pub builtin: BuiltinType,
    pub ctors: Vec<FctId>,
    pub props: Vec<Prop>,
    pub methods: Vec<FctId>,
    pub size: i32,
    pub ast: Option<&'ast ast::Class>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PropId(pub usize);

#[derive(Debug)]
pub struct Prop {
    pub id: PropId,
    pub name: Name,
    pub ty: BuiltinType,
    pub offset: i32,
}
