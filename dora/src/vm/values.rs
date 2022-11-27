use crate::language::ty::SourceType;
use crate::utils::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueInstanceId(usize);

impl Id for ValueInstance {
    type IdType = ValueInstanceId;

    fn id_to_usize(id: ValueInstanceId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ValueInstanceId {
        ValueInstanceId(value.try_into().unwrap())
    }

    fn store_id(_value: &mut ValueInstance, _id: ValueInstanceId) {}
}

pub struct ValueInstance {
    pub fields: Vec<ValueInstanceField>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

impl ValueInstance {
    pub fn contains_references(&self) -> bool {
        !self.ref_fields.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct ValueInstanceField {
    pub offset: i32,
    pub ty: SourceType,
}
