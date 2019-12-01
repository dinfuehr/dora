use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use parking_lot::Mutex;
use std::sync::Arc;

use crate::gc::Address;
use crate::ty::BuiltinType;
use crate::utils::GrowableVec;
use crate::vm::{FctId, FileId};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

impl GlobalId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u32> for GlobalId {
    fn from(data: u32) -> GlobalId {
        GlobalId(data)
    }
}

#[derive(Debug)]
pub struct GlobalData {
    pub id: GlobalId,
    pub file: FileId,
    pub pos: Position,
    pub ty: BuiltinType,
    pub reassignable: bool,
    pub name: Name,
    pub getter: Option<FctId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl GrowableVec<Mutex<GlobalData>> {
    pub fn idx(&self, index: GlobalId) -> Arc<Mutex<GlobalData>> {
        self.idx_usize(index.0 as usize)
    }
}
