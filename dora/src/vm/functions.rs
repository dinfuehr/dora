use crate::vm::{module_path, module_path_with_name, VM};
use dora_frontend::language::sem_analysis::{FctDefinitionId, FctParent};

pub fn display_fct(vm: &VM, fct_id: FctDefinitionId) -> String {
    let fct = vm.fcts.idx(fct_id);
    let fct = fct.read();
    let mut repr = match fct.parent {
        FctParent::Trait(trait_id) => {
            let trait_ = vm.traits[trait_id].read();
            module_path_with_name(vm, trait_.module_id, trait_.name)
        }

        FctParent::Extension(..) => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<impl block>");
            result
        }

        FctParent::Impl(..) => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<impl block>");
            result
        }

        FctParent::None => {
            return module_path_with_name(vm, fct.module_id, fct.name);
        }

        FctParent::Function(_) => "lamba".into(),
    };

    if !fct.has_parent() || fct.is_static {
        repr.push_str("::");
    } else {
        repr.push_str("#");
    }

    repr.push_str(&vm.interner.str(fct.name));
    repr
}
