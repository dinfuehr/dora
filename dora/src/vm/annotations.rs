use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{FileId, TypeParam};
use dora_parser::ast::{AnnotationParam, Modifier};
use dora_parser::interner::Name;
use dora_parser::Position;
use parking_lot::RwLock;
use std::sync::Arc;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct AnnotationId(usize);

impl AnnotationId {
    pub fn max() -> AnnotationId {
        AnnotationId(usize::max_value())
    }
}

impl From<AnnotationId> for usize {
    fn from(data: AnnotationId) -> usize {
        data.0
    }
}

impl From<usize> for AnnotationId {
    fn from(data: usize) -> AnnotationId {
        AnnotationId(data)
    }
}

impl GrowableVec<RwLock<Annotation>> {
    pub fn idx(&self, index: AnnotationId) -> Arc<RwLock<Annotation>> {
        self.idx_usize(index.0)
    }
}

#[derive(Debug)]
pub struct Annotation {
    pub id: AnnotationId,
    pub file_id: FileId,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub internal: Option<Modifier>,

    pub type_params: Option<Vec<TypeParam>>,
    pub term_params: Option<Vec<AnnotationParam>>,
}
