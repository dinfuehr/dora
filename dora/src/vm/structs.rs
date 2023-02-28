use crate::bytecode::{BytecodeType, BytecodeTypeArray};
use crate::vm::{display_ty_raw, module_path, StructDefinition, VM};
use dora_frontend::Id;

impl StructDefinition {
    pub fn name_vm(&self, vm: &VM) -> String {
        module_path(vm, self.module_id, self.name)
    }

    pub fn name_with_params_vm(&self, vm: &VM, type_params: &BytecodeTypeArray) -> String {
        let mut name = self.name_vm(vm);

        if type_params.len() > 0 {
            let type_params = type_params
                .iter()
                .map(|p| display_ty_raw(vm, &p))
                .collect::<Vec<_>>()
                .join(", ");

            name.push('[');
            name.push_str(&type_params);
            name.push(']');
        }

        name
    }
}

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
    pub ty: BytecodeType,
}
