use std::path::PathBuf;
use std::sync::Arc;

use crate::language::sem_analysis::{ModuleDefinitionId, PackageDefinitionId};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceFileId(pub usize);

impl SourceFileId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

pub struct SourceFile {
    pub id: SourceFileId,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub path: PathBuf,
    pub content: Arc<String>,
}
