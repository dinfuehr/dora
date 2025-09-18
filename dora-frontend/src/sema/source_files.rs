use std::cell::OnceCell;
use std::path::PathBuf;
use std::sync::Arc;

use crate::sema::{ModuleDefinitionId, PackageDefinitionId};
use dora_parser::ast;
use id_arena::Id;

pub type SourceFileId = Id<SourceFile>;

pub struct SourceFile {
    pub id: OnceCell<SourceFileId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub path: PathBuf,
    pub content: Arc<String>,
    pub line_starts: Vec<u32>,
    pub ast: OnceCell<Arc<ast::File>>,
}

impl SourceFile {
    pub fn ast(&self) -> &Arc<ast::File> {
        self.ast.get().expect("missing ast")
    }
}
