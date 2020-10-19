use crate::sym::SymLevel;
use crate::vm::FileId;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct NamespaceId(usize);

impl NamespaceId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for NamespaceId {
    fn from(data: usize) -> NamespaceId {
        NamespaceId(data)
    }
}

#[derive(Debug)]
pub struct NamespaceData {
    pub id: NamespaceId,
    pub file: FileId,
    pub pos: Position,
    pub namespace_id: Option<NamespaceId>,
    pub name: Name,
    pub table: SymLevel,
}
