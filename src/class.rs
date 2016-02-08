use std::collections::HashMap;

use ctxt::FctContextId;
use interner::Name;
use ty::BuiltinType;

pub struct ClassId(usize);

pub struct Class {
    pub id: ClassId,
    pub name: Name,
    pub props: Vec<Prop>,
}

pub struct Prop {
    pub name: Name,
    pub ty: BuiltinType,
}
