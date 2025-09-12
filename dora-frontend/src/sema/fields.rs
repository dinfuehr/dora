use std::ops::{Index, IndexMut};

use crate::interner::Name;
use crate::sema::Visibility;
use crate::{ParsedType, SourceType, Span};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldDefinitionId(pub usize);

impl FieldDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldDefinitionId {
    fn from(data: usize) -> FieldDefinitionId {
        FieldDefinitionId(data)
    }
}

#[derive(Debug)]
pub struct FieldDefinition {
    pub id: FieldDefinitionId,
    pub name: Option<Name>,
    pub span: Option<Span>,
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

impl Index<FieldDefinitionId> for Vec<FieldDefinition> {
    type Output = FieldDefinition;

    fn index(&self, index: FieldDefinitionId) -> &FieldDefinition {
        &self[index.0]
    }
}

impl IndexMut<FieldDefinitionId> for Vec<FieldDefinition> {
    fn index_mut(&mut self, index: FieldDefinitionId) -> &mut FieldDefinition {
        &mut self[index.0]
    }
}
