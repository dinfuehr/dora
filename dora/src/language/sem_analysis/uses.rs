use std::sync::Arc;

use crate::language::sem_analysis::{ModuleDefinitionId, SourceFileId};
use dora_parser::ast;

pub struct UseDefinition {
    pub ast: Arc<ast::Use>,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
}

impl UseDefinition {
    pub fn new(
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        node: &Arc<ast::Use>,
    ) -> UseDefinition {
        UseDefinition {
            module_id,
            file_id,
            ast: node.clone(),
        }
    }
}
