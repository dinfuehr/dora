use crate::language::sem_analysis::TraitDefinition;
use crate::vm::{module_path, VM};

impl TraitDefinition {
    pub fn name_vm(&self, sa: &VM) -> String {
        module_path(sa, self.module_id, self.name)
    }
}
