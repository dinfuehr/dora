use crate::language::ty::SourceType;
use crate::utils::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInstanceId(usize);

impl Id for StructInstance {
    type IdType = StructInstanceId;

    fn id_to_usize(id: StructInstanceId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> StructInstanceId {
        StructInstanceId(value.try_into().unwrap())
    }

    fn store_id(_value: &mut StructInstance, _id: StructInstanceId) {}
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
