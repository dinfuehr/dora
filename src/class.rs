use std::collections::HashMap;

use ctxt::FctContextId;
use interner::Name;

pub struct ClassInfoId(usize);

pub struct ClassInfo {
    pub id: ClassInfoId,
    pub name: Name,
    pub methods: HashMap<Name, FctContextId>,
}
