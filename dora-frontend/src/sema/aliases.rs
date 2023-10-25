use crate::{interner::Name, program_parser::ParsedModifierList};
use std::cell::OnceCell;
use std::sync::Arc;

use id_arena::Id;

use crate::sema::{ModuleDefinitionId, PackageDefinitionId, SourceFileId};
use dora_parser::ast;

pub type AliasDefinitionId = Id<AliasDefinition>;

pub struct AliasDefinition {
    pub id: OnceCell<AliasDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub node: Arc<ast::TypeAlias>,
    pub modifiers: ParsedModifierList,
    pub name: Name,
}

impl AliasDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::TypeAlias>,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> AliasDefinition {
        AliasDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            node: node.clone(),
            modifiers,
            name,
        }
    }
}
