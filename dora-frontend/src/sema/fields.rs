use id_arena::Id;
use std::rc::Rc;

use crate::interner::Name;
use crate::sema::{
    Element, ElementId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, Visibility,
};
use crate::{ParsedType, SourceType, Span};

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
    pub id: Option<FieldDefinitionId>,
    pub name: Option<Name>,
    pub span: Option<Span>,
    pub index: FieldIndex,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub visibility: Visibility,
    pub file_id: Option<SourceFileId>,
    pub module_id: ModuleDefinitionId,
    pub package_id: PackageDefinitionId,
}

impl FieldDefinition {
    pub fn id(&self) -> FieldDefinitionId {
        self.id.expect("missing id")
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }
}

impl Element for FieldDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Field(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing file_id")
    }

    fn span(&self) -> Span {
        self.span.expect("missing span")
    }

    fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id
    }

    fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        unreachable!()
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        None
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn children(&self) -> &[ElementId] {
        &[]
    }
}
