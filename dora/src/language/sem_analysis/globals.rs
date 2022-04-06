use parking_lot::RwLock;
use std::sync::Arc;

use crate::gc::Address;
use crate::language::sem_analysis::{namespace_path, FctDefinitionId, NamespaceId};
use crate::language::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{FileId, VM};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalDefinitionId(u32);

impl GlobalDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u32> for GlobalDefinitionId {
    fn from(data: u32) -> GlobalDefinitionId {
        GlobalDefinitionId(data)
    }
}

#[derive(Debug)]
pub struct GlobalDefinition {
    pub id: GlobalDefinitionId,
    pub file_id: FileId,
    pub ast: Arc<ast::Global>,
    pub pos: Position,
    pub namespace_id: NamespaceId,
    pub is_pub: bool,
    pub ty: SourceType,
    pub mutable: bool,
    pub name: Name,
    pub initializer: Option<FctDefinitionId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl GlobalDefinition {
    pub fn needs_initialization(&self) -> bool {
        self.initializer.is_some() && !self.is_initialized()
    }

    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    fn is_initialized(&self) -> bool {
        unsafe { *self.address_init.to_ptr::<bool>() }
    }
}

impl GrowableVec<RwLock<GlobalDefinition>> {
    pub fn idx(&self, index: GlobalDefinitionId) -> Arc<RwLock<GlobalDefinition>> {
        self.idx_usize(index.0 as usize)
    }
}
