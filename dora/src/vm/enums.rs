use parking_lot::RwLock;

use std::sync::Arc;

use crate::language::sem_analysis::{EnumDefinition, EnumDefinitionId};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::GrowableVec;
use crate::vm::ClassInstanceId;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumInstanceId(u32);

impl From<usize> for EnumInstanceId {
    fn from(data: usize) -> EnumInstanceId {
        EnumInstanceId(data as u32)
    }
}

impl GrowableVec<EnumInstance> {
    pub fn idx(&self, index: EnumInstanceId) -> Arc<EnumInstance> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Debug)]
pub struct EnumInstance {
    pub id: EnumInstanceId,
    pub enum_id: EnumDefinitionId,
    pub type_params: SourceTypeArray,
    pub layout: EnumLayout,
    pub variants: RwLock<Vec<Option<ClassInstanceId>>>,
}

impl EnumInstance {
    pub fn field_id(&self, xenum: &EnumDefinition, variant_id: usize, element: u32) -> u32 {
        let variant = &xenum.variants[variant_id];
        let mut units = 0;

        for ty in &variant.types[0..element as usize] {
            if ty.is_unit() {
                units += 1;
            }
        }

        1 + element - units
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EnumLayout {
    Int,
    Ptr,
    Tagged,
}

#[derive(Debug)]
pub struct EnumDefVariant {
    pub types: Vec<SourceType>,
}
