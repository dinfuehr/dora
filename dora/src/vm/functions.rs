use crate::language::sem_analysis::{FctDefinition, FctParent};
use crate::vm::{module_path, path_for_type, VM};

impl FctDefinition {
    pub fn display_name_vm(&self, sa: &VM) -> String {
        let mut repr = match self.parent {
            FctParent::Trait(trait_id) => {
                let trait_ = sa.traits[trait_id].read();
                trait_.name_vm(sa)
            }

            FctParent::Extension(extension_id) => {
                let extension = &sa.extensions[extension_id];
                let extension = extension.read();
                path_for_type(sa, extension.ty.clone())
            }

            FctParent::Impl(impl_id) => {
                let impl_ = &sa.impls[impl_id];
                let impl_ = impl_.read();
                path_for_type(sa, impl_.extended_ty.clone())
            }

            FctParent::None => {
                return module_path(sa, self.module_id, self.name);
            }

            FctParent::Function(_) => "lamba".into(),
        };

        if !self.has_parent() || self.is_static {
            repr.push_str("::");
        } else {
            repr.push_str("#");
        }

        repr.push_str(&sa.interner.str(self.name));
        repr
    }
}
