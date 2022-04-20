use std::sync::Arc;

use crate::language::ty::SourceType;
use crate::utils::GrowableVec;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInstanceId(usize);

impl From<usize> for StructInstanceId {
    fn from(data: usize) -> StructInstanceId {
        StructInstanceId(data)
    }
}

impl GrowableVec<StructInstance> {
    pub fn idx(&self, index: StructInstanceId) -> Arc<StructInstance> {
        self.idx_usize(index.0)
    }
}

pub struct StructInstance {
    pub fields: Vec<StructInstanceField>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

impl StructInstance {
    pub fn contains_references(&self) -> bool {
        !self.ref_fields.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct StructInstanceField {
    pub offset: i32,
    pub ty: SourceType,
}
