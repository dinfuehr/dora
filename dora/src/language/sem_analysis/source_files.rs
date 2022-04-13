use std::path::PathBuf;
use std::sync::Arc;

use crate::language::sem_analysis::NamespaceDefinitionId;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceFileId(u32);

impl From<u32> for SourceFileId {
    fn from(data: u32) -> SourceFileId {
        SourceFileId(data)
    }
}

impl SourceFileId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct SourceFile {
    pub id: SourceFileId,
    pub path: PathBuf,
    pub content: Arc<String>,
    pub namespace_id: NamespaceDefinitionId,
}
