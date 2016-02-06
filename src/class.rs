use std::collections::HashMap;

use ctxt::FctContextId;
use interner::Name;

pub struct ClassId(usize);

pub struct Class {
    pub id: ClassId,
    pub name: Name,
}
