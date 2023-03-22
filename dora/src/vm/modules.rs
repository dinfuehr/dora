use crate::vm::{ModuleDefinitionId, VM};

pub fn module_path_name(vm: &VM, module_id: ModuleDefinitionId, name: &str) -> String {
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
