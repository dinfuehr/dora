use crate::mem;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstanceSize {
    Fixed(i32),
    PrimitiveArray(i32),
    ObjArray,
    UnitArray,
    TupleArray(i32),
    StructArray(i32),
    FreeArray,
    CodeObject,
    Str,
}

impl InstanceSize {
    pub fn element_size(&self) -> Option<i32> {
        match self {
            InstanceSize::PrimitiveArray(esize) => Some(*esize),
            InstanceSize::ObjArray => Some(mem::ptr_width()),
            InstanceSize::Str => Some(1),
            InstanceSize::Fixed(_) => None,
            InstanceSize::FreeArray => unreachable!(),
            InstanceSize::TupleArray(esize) => Some(*esize),
            InstanceSize::StructArray(esize) => Some(*esize),
            InstanceSize::UnitArray => None,
            InstanceSize::CodeObject => Some(1),
        }
    }
}
