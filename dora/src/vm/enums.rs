use parking_lot::RwLock;

use crate::bytecode::ty::BytecodeTypeArray;
use crate::language::sem_analysis::{EnumDefinition, EnumDefinitionId};
use crate::vm::{display_ty_raw, module_path, ClassInstanceId, VM};
use dora_frontend::Id;

pub fn enum_definition_name(enum_: &EnumDefinition, vm: &VM) -> String {
    module_path(vm, enum_.module_id, enum_.name)
}

pub fn enum_definition_name_with_params(
    enum_: &EnumDefinition,
    vm: &VM,
    type_list: &BytecodeTypeArray,
) -> String {
    let name = vm.interner.str(enum_.name);

    if type_list.len() > 0 {
        let type_list = type_list
            .iter()
            .map(|p| display_ty_raw(vm, &p))
            .collect::<Vec<_>>()
            .join(", ");

        format!("{}[{}]", name, type_list)
    } else {
        name.to_string()
    }
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
    pub enum_id: EnumDefinitionId,
    pub type_params: BytecodeTypeArray,
    pub layout: EnumLayout,
    pub variants: RwLock<Vec<Option<ClassInstanceId>>>,
}

impl EnumInstance {
    pub fn field_id(
        &self,
        enum_: &EnumDefinition,
        variant_idx: usize,
        element_idx: usize,
    ) -> usize {
        let variant = &enum_.variants[variant_idx];
        let mut units = 0;

        for ty in &variant.types[0..element_idx as usize] {
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
