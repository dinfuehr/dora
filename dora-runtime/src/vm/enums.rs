use parking_lot::RwLock;

use crate::shape::Shape;
use crate::utils::Id;
use crate::vm::VM;
use dora_bytecode::ty::BytecodeTypeArray;
use dora_bytecode::{module_path_name, EnumData, EnumId};

pub fn enum_definition_name(enum_: &EnumData, vm: &VM) -> String {
    module_path_name(&vm.program, enum_.module_id, &enum_.name)
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumInstanceId(u32);

impl Id for EnumInstance {
    type IdType = EnumInstanceId;

    fn id_to_usize(id: EnumInstanceId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> EnumInstanceId {
        EnumInstanceId(value.try_into().unwrap())
    }

    fn store_id(_value: &mut EnumInstance, _id: EnumInstanceId) {}
}

#[derive(Debug)]
pub struct EnumInstance {
    pub enum_id: EnumId,
    pub type_params: BytecodeTypeArray,
    pub layout: EnumLayout,
    pub variants: RwLock<Vec<Option<*const Shape>>>,
}

impl EnumInstance {
    pub fn field_id(&self, enum_: &EnumData, variant_idx: u32, element_idx: u32) -> u32 {
        let variant = &enum_.variants[variant_idx as usize];
        let mut units = 0;

        for ty in &variant.arguments[0..element_idx as usize] {
            if ty.is_unit() {
                units += 1;
            }
        }

        1 + element_idx - units
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EnumLayout {
    Int,
    Ptr,
    Tagged,
}
