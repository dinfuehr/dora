use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use parking_lot::RwLock;
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
    pub initializer: Option<FctId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl GlobalData {
    pub fn needs_initialization(&self) -> bool {
        self.initializer.is_some() && !self.is_initialized()
    }

    fn is_initialized(&self) -> bool {
        unsafe { *self.address_init.to_ptr::<bool>() }
    }
}

impl GrowableVec<RwLock<GlobalData>> {
    pub fn idx(&self, index: GlobalId) -> Arc<RwLock<GlobalData>> {
        self.idx_usize(index.0 as usize)
    }
}
