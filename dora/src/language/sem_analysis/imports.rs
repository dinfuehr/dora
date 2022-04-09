use std::sync::Arc;

use crate::language::sem_analysis::NamespaceDefinitionId;
use crate::vm::FileId;
use dora_parser::ast;

pub struct ImportDefinition {
    pub ast: Arc<ast::Import>,
    pub namespace_id: NamespaceDefinitionId,
    pub file_id: FileId,
}
