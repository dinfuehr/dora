use std::sync::Arc;

use crate::vm::{FileId, NamespaceId};
use dora_parser::ast;

pub struct ImportData {
    pub ast: Arc<ast::Import>,
    pub namespace_id: NamespaceId,
    pub file_id: FileId,
}
