use std::sync::Arc;

use crate::sema::{ModuleDefinitionId, PackageDefinitionId, SourceFileId};
use dora_parser::ast;

pub struct UseDefinition {
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Use>,
}

impl UseDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Use>,
    ) -> UseDefinition {
        UseDefinition {
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
        }
    }
}
