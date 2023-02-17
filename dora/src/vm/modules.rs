use crate::vm::{ModuleDefinition, ModuleDefinitionId, VM};
use dora_parser::interner::Name;

impl ModuleDefinition {
    pub fn name_vm(&self, vm: &VM) -> String {
        let mut path = String::new();

        for &module_id in &self.parents {
            let module = &vm.modules[module_id].read();

            if let Some(name) = module.name {
                if !path.is_empty() {
                    path.push_str("::");
                }

                path.push_str(&vm.interner.str(name));
            }
        }

        if let Some(name) = self.name {
            if !path.is_empty() {
                path.push_str("::");
            }

            path.push_str(&vm.interner.str(name));
        }

        path
    }
}

pub fn module_path(sa: &VM, module_id: ModuleDefinitionId, name: Name) -> String {
    let module = &sa.modules[module_id].read();
    let mut result = module.name_vm(sa);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&sa.interner.str(name));
    result
}

pub fn module_contains(
    vm: &VM,
    parent_id: ModuleDefinitionId,
    child_id: ModuleDefinitionId,
) -> bool {
    if parent_id == child_id {
        return true;
    }

    let module = &vm.modules[child_id].read();
    module.parents.contains(&parent_id)
}
