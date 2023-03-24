use crate::vm::VM;
use dora_bytecode::ModuleId;

pub fn module_path_name(vm: &VM, module_id: ModuleId, name: &str) -> String {
    let mut result = module_path(vm, module_id);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(name);
    result
}

pub fn module_path(vm: &VM, module_id: ModuleId) -> String {
    let mut path = String::new();

    let current_id = module_id;

    // Do not print name for the top program module.
    if current_id == vm.program_module_id() {
        return "".into();
    }

    let module = &vm.program.modules[current_id.0 as usize];
    path.push_str(&module.name);

    let mut module_id = module.parent_id;

    while let Some(current_id) = module_id {
        // Do not print name for the top program module.
        if current_id == vm.program_module_id() {
            break;
        }

        let module = &vm.program.modules[current_id.0 as usize];
        assert_ne!("<root>", module.name);
        path.insert_str(0, "::");
        path.insert_str(0, &module.name);
        module_id = module.parent_id;
    }

    path
}
