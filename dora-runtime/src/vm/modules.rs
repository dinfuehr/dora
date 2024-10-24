use dora_bytecode::{ModuleId, Program};

pub fn module_path_name(prog: &Program, module_id: ModuleId, name: &str) -> String {
    let mut result = module_path(prog, module_id);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(name);
    result
}

pub fn module_path(prog: &Program, module_id: ModuleId) -> String {
    let mut path = String::new();

    let current_id = module_id;

    // Do not print name for the top program module.
    if current_id == prog.program_module_id() {
        return "".into();
    }

    let module = prog.module(current_id);
    path.push_str(&module.name);

    let mut module_id = module.parent_id;

    while let Some(current_id) = module_id {
        // Do not print name for the top program module.
        if current_id == prog.program_module_id() {
            break;
        }

        let module = prog.module(current_id);
        assert_ne!("<root>", module.name);
        path.insert_str(0, "::");
        path.insert_str(0, &module.name);
        module_id = module.parent_id;
    }

    path
}
