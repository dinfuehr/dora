use parking_lot::RwLock;

use crate::vm::{module_path_with_name_str, ClassInstanceId, VM};
use dora_frontend::bytecode::ty::BytecodeTypeArray;
use dora_frontend::bytecode::{EnumData, EnumId};
use dora_frontend::language::sem_analysis::ModuleDefinitionId;
use dora_frontend::Id;

pub fn enum_definition_name(enum_: &EnumData, vm: &VM) -> String {
    module_path_with_name_str(
        vm,
        ModuleDefinitionId(enum_.module_id.0 as usize),
        &enum_.name,
    )
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
    pub variants: RwLock<Vec<Option<ClassInstanceId>>>,
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
