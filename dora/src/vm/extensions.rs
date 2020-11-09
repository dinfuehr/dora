use parking_lot::RwLock;

use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use crate::ty::{SourceType, TypeListId};
use crate::vm::{FctId, FileId, NamespaceId, TypeParam};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

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
    pub file_id: FileId,
    pub ast: Arc<ast::Impl>,
    pub namespace_id: NamespaceId,
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
