use crate::mem;
use crate::object::Header;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstanceSize {
    Fixed(i32),
    PrimitiveArray(i32),
    ObjArray,
    UnitArray,
    ValueArray(i32),
    FreeArray,
    CodeObject,
    Str,
}

impl InstanceSize {
    pub fn instance_size(&self) -> Option<i32> {
        match self {
            InstanceSize::PrimitiveArray(_) => None,
            InstanceSize::ObjArray => None,
            InstanceSize::Str => None,
            InstanceSize::Fixed(value) => Some(*value),
            InstanceSize::FreeArray => None,
            InstanceSize::ValueArray(_) => None,
            InstanceSize::UnitArray => Some(Header::array_size()),
            InstanceSize::CodeObject => None,
        }
    }

    pub fn element_size(&self) -> Option<i32> {
        match self {
            InstanceSize::PrimitiveArray(esize) => Some(*esize),
            InstanceSize::ObjArray => Some(mem::ptr_width()),
            InstanceSize::Str => Some(1),
            InstanceSize::Fixed(_) => None,
            InstanceSize::FreeArray => Some(mem::ptr_width()),
            InstanceSize::ValueArray(esize) => Some(*esize),
            InstanceSize::UnitArray => None,
            InstanceSize::CodeObject => Some(mem::ptr_width()),
        }
    }
}
