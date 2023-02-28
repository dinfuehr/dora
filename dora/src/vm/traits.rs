use crate::bytecode::BytecodeTypeArray;
use crate::language::sem_analysis::TraitDefinition;
use crate::vm::{display_ty_raw, module_path, VM};

impl TraitDefinition {
    pub fn name_vm(&self, sa: &VM) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params_vm(&self, vm: &VM, type_list: &BytecodeTypeArray) -> String {
        let name = module_path(vm, self.module_id, self.name);

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
}
