use std::collections::HashMap;

use ast;
use ctxt::FctContextId;
use interner::Name;
use ty::BuiltinType;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ClassId(pub usize);

#[derive(Debug)]
pub struct Class<'ast> {
    pub id: ClassId,
    pub name: Name,
    pub props: Vec<Prop>,
    pub size: i32,
    pub ast: Option<&'ast ast::Class>,
}

#[derive(Debug)]
pub struct Prop {
    pub name: Name,
    pub ty: BuiltinType,
    pub offset: i32,
}
