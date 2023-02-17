use crate::language::sem_analysis::TraitDefinition;
use crate::language::ty::SourceTypeArray;
use crate::vm::{module_path, VM};

impl TraitDefinition {
    pub fn name_vm(&self, sa: &VM) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params_vm(&self, sa: &VM, type_list: &SourceTypeArray) -> String {
        let name = module_path(sa, self.module_id, self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name_vm(sa))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
    }
}
