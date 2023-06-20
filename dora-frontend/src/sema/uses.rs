use std::sync::Arc;

use crate::{
    program_parser::ParsedModifierList,
    sema::{ModuleDefinitionId, PackageDefinitionId, SourceFileId, Visibility},
};
use dora_parser::ast;

pub struct UseDefinition {
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::UsePath>,
    pub visibility: Visibility,
}

impl UseDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::UsePath>,
        modifiers: ParsedModifierList,
    ) -> UseDefinition {
        UseDefinition {
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            visibility: modifiers.visibility(),
        }
    }
}
