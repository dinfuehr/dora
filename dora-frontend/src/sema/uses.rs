use std::cell::OnceCell;

use crate::{
    element_parser::ParsedModifierList,
    sema::{ModuleDefinitionId, PackageDefinitionId, SourceFileId, Visibility},
};
use dora_parser::ast;
use id_arena::Id;

pub type UseDefinitionId = Id<UseDefinition>;

pub struct UseDefinition {
    pub id: OnceCell<UseDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub visibility: Visibility,
}

impl UseDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        modifiers: ParsedModifierList,
    ) -> UseDefinition {
        UseDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast_id,
            visibility: modifiers.visibility(),
        }
    }

    pub fn id(&self) -> UseDefinitionId {
        self.id.get().cloned().expect("missing id")
    }
}
