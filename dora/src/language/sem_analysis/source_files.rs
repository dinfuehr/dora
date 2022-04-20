use std::path::PathBuf;
use std::sync::Arc;

use crate::language::sem_analysis::ModuleDefinitionId;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceFileId(pub usize);

impl SourceFileId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

pub struct SourceFile {
    pub id: SourceFileId,
    pub path: PathBuf,
    pub content: Arc<String>,
    pub module_id: ModuleDefinitionId,
}
