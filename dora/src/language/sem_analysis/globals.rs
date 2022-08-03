use std::convert::TryInto;
use std::sync::Arc;

use crate::gc::Address;
use crate::language::sem_analysis::{
    module_path, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, SemAnalysis,
    SourceFileId, Visibility,
};
use crate::language::ty::SourceType;
use crate::utils::Id;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalDefinitionId(u32);

impl Id for GlobalDefinition {
    type IdType = GlobalDefinitionId;

    fn id_to_usize(id: GlobalDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> GlobalDefinitionId {
        GlobalDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut GlobalDefinition, id: GlobalDefinitionId) {
        value.id = Some(id);
    }
}

impl GlobalDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct GlobalDefinition {
    pub id: Option<GlobalDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Global>,
    pub pos: Position,
    pub visibility: Visibility,
    pub ty: SourceType,
    pub mutable: bool,
    pub name: Name,
    pub initializer: Option<FctDefinitionId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl GlobalDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Global>,
    ) -> GlobalDefinition {
        GlobalDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            pos: node.pos,
            name: node.name,
            visibility: Visibility::from_ast(node.is_pub),
            ty: SourceType::Unit,
            mutable: node.mutable,
            initializer: None,
            address_init: Address::null(),
            address_value: Address::null(),
        }
    }

    pub fn id(&self) -> GlobalDefinitionId {
        self.id.expect("id missing")
    }

    pub fn needs_initialization(&self) -> bool {
        self.initializer.is_some() && !self.is_initialized()
    }

    pub fn name(&self, sa: &SemAnalysis) -> String {
        module_path(sa, self.module_id, self.name)
    }

    fn is_initialized(&self) -> bool {
        unsafe { *self.address_init.to_ptr::<bool>() }
    }
}
