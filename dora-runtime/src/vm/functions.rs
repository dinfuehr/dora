use crate::vm::{module_path, module_path_name, VM};
use dora_bytecode::{FunctionId, FunctionKind};

pub fn display_fct(vm: &VM, fct_id: FunctionId) -> String {
    let fct = &vm.program.functions[fct_id.0 as usize];
    let mut repr = match fct.kind {
        FunctionKind::Trait(trait_id) => {
            let trait_ = &vm.program.traits[trait_id.0 as usize];
            module_path_name(vm, trait_.module_id, &trait_.name)
        }

        FunctionKind::Method => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<extension block>");
            result
        }

        FunctionKind::Impl(..) => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<impl block>");
            result
        }

        FunctionKind::Function => return module_path_name(vm, fct.module_id, &fct.name),

        FunctionKind::Lambda(_) => "lamba".into(),
    };

    repr.push_str("::");
    repr.push_str(&fct.name);
    repr
}
