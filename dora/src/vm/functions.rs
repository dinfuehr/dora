use crate::vm::{module_path, module_path_name, VM};
use dora_bytecode::FunctionId;
use dora_frontend::language::sem_analysis::{FctDefinitionId, FctParent, ModuleDefinitionId};

pub fn display_fct(vm: &VM, fct_id: FunctionId) -> String {
    let fct = vm.fcts.idx(FctDefinitionId(fct_id.0 as usize));
    let fct = fct.read();
    let mut repr = match fct.parent {
        FctParent::Trait(trait_id) => {
            let trait_ = &vm.program.traits[trait_id.to_usize()];
            module_path_name(
                vm,
                ModuleDefinitionId(trait_.module_id.0 as usize),
                &trait_.name,
            )
        }

        FctParent::Extension(..) => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<extension block>");
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
            let name = vm.interner.str(fct.name);
            return module_path_name(vm, fct.module_id, &name);
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
