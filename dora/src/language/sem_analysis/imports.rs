use std::sync::Arc;

use crate::language::sem_analysis::NamespaceId;
use crate::vm::FileId;
use dora_parser::ast;

pub struct ImportData {
    pub ast: Arc<ast::Import>,
    pub namespace_id: NamespaceId,
    pub file_id: FileId,
}
