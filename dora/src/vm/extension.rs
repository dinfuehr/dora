use parking_lot::RwLock;

use std::ops::Index;

use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use std::collections::HashMap;

use crate::ty::{SourceType, TypeListId};
use crate::vm::{FctId, FileId, TypeParam};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExtensionId(u32);

impl From<usize> for ExtensionId {
    fn from(data: usize) -> ExtensionId {
        ExtensionId(data as u32)
    }
}

impl ExtensionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct ExtensionData {
    pub id: ExtensionId,
    pub file: FileId,
    pub pos: Position,
    pub type_params: Vec<TypeParam>,
    pub ty: SourceType,
    pub methods: Vec<FctId>,
    pub instance_names: HashMap<Name, FctId>,
    pub static_names: HashMap<Name, FctId>,
}

impl ExtensionData {
    pub fn type_param(&self, id: TypeListId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }
}

impl Index<ExtensionId> for Vec<RwLock<ExtensionData>> {
    type Output = RwLock<ExtensionData>;

    fn index(&self, index: ExtensionId) -> &RwLock<ExtensionData> {
        &self[index.to_usize()]
    }
}
