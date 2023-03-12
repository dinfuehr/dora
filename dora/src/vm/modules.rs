use crate::vm::{ModuleDefinitionId, VM};
use dora_parser::interner::Name;

pub fn module_path_with_name(vm: &VM, module_id: ModuleDefinitionId, name: Name) -> String {
    module_path_with_name_str(vm, module_id, &vm.interner.str(name))
}

pub fn module_path_with_name_str(vm: &VM, module_id: ModuleDefinitionId, name: &str) -> String {
    let mut result = module_path(vm, module_id);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(name);
    result
}

pub fn module_path(vm: &VM, module_id: ModuleDefinitionId) -> String {
    let start_module = &vm.modules[module_id].read();
    let mut path = String::new();

    for &module_id in &start_module.parents {
        let module = &vm.modules[module_id].read();

        if let Some(name) = module.name {
            if !path.is_empty() {
                path.push_str("::");
            }

            path.push_str(&vm.interner.str(name));
        }
    }

    if let Some(name) = start_module.name {
        if !path.is_empty() {
            path.push_str("::");
        }

        path.push_str(&vm.interner.str(name));
    }

    path
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
