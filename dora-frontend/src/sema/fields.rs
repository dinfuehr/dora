use crate::interner::Name;
use crate::sema::Visibility;
use crate::{ParsedType, SourceType, Span};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldIndex(pub usize);

impl FieldIndex {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldIndex {
    fn from(data: usize) -> FieldIndex {
        FieldIndex(data)
    }
}

#[derive(Debug)]
pub struct FieldDefinition {
    pub name: Option<Name>,
    pub span: Option<Span>,
    pub index: FieldIndex,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub visibility: Visibility,
}

impl FieldDefinition {
    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }
}
