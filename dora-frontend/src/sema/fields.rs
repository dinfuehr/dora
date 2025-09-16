use std::cell::OnceCell;

use id_arena::Id;

use crate::interner::Name;
use crate::sema::{ElementId, Visibility};
use crate::{ParsedType, SourceType, Span};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FatFieldDefinitionId {
    pub owner: ElementId,
    pub index: FieldIndex,
}

pub type FieldDefinitionId = Id<FieldDefinition>;

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
    pub id: OnceCell<FieldDefinitionId>,
    pub name: Option<Name>,
    pub span: Option<Span>,
    pub index: FieldIndex,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub visibility: Visibility,
}

impl FieldDefinition {
    pub fn id(&self) -> FieldDefinitionId {
        self.id.get().cloned().expect("missing id")
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }
}
