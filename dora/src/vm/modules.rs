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
    let mut path = String::new();

    let current_id = module_id.to_usize();

    // Do not print name for the top program module.
    if current_id == vm.program_module_id().to_usize() {
        return "".into();
    }

    let module = &vm.program.modules[current_id];
    path.push_str(&module.name);

    let mut module_id = module.parent_id;

    while let Some(current_id) = module_id {
        let current_id = current_id.0 as usize;

        // Do not print name for the top program module.
        if current_id == vm.program_module_id().to_usize() {
            break;
        }

        let module = &vm.program.modules[current_id];
        assert_ne!("<root>", module.name);
        path.insert_str(0, "::");
        path.insert_str(0, &module.name);
        module_id = module.parent_id;
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
