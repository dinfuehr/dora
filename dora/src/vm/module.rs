use parking_lot::RwLock;
use std::sync::Arc;

use crate::semck::specialize::replace_type_param;
use crate::size::InstanceSize;
use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{FctId, Field, FieldDef, FileId, NamespaceId, TraitId, VM};

use crate::vtable::VTableBox;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use std::collections::HashSet;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ModuleId(usize);

impl ModuleId {
    pub fn max() -> ModuleId {
        ModuleId(usize::max_value())
    }
}

impl From<ModuleId> for usize {
    fn from(data: ModuleId) -> usize {
        data.0
    }
}

impl From<usize> for ModuleId {
    fn from(data: usize) -> ModuleId {
        ModuleId(data)
    }
}

impl GrowableVec<RwLock<Module>> {
    pub fn idx(&self, index: ModuleId) -> Arc<RwLock<Module>> {
        self.idx_usize(index.0)
    }
}

pub static DISPLAY_SIZE: usize = 6;

#[derive(Debug)]
pub struct Module {
    pub id: ModuleId,
    pub file: FileId,
    pub namespace_id: Option<NamespaceId>,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub parent_class: Option<SourceType>,
    pub internal: bool,
    pub internal_resolved: bool,
    pub has_constructor: bool,

    pub constructor: Option<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub virtual_fcts: Vec<FctId>,

    pub traits: Vec<TraitId>,
}

pub fn find_methods_in_module(
    vm: &VM,
    object_type: SourceType,
    name: Name,
) -> Vec<(SourceType, FctId)> {
    let mut ignores = HashSet::new();

    let mut module_type = object_type;

    loop {
        let module_id = module_type.module_id().expect("no module");
        let module = vm.modules.idx(module_id);
        let module = module.read();

        for &method in &module.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name {
                if let Some(overrides) = method.overrides {
                    ignores.insert(overrides);
                }

                if !ignores.contains(&method.id) {
                    return vec![(module_type, method.id)];
                }
            }
        }

        if let Some(parent_class) = module.parent_class.clone() {
            let type_list = module_type.type_params(vm);
            module_type = replace_type_param(vm, parent_class, &type_list, None);
        } else {
            break;
        }
    }

    Vec::new()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ModuleDefId(usize);

impl ModuleDefId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ModuleDefId {
    fn from(data: usize) -> ModuleDefId {
        ModuleDefId(data)
    }
}

impl GrowableVec<RwLock<ModuleDef>> {
    pub fn idx(&self, index: ModuleDefId) -> Arc<RwLock<ModuleDef>> {
        self.idx_usize(index.0)
    }
}

#[derive(Debug)]
pub struct ModuleDef {
    pub id: ModuleDefId,
    pub mod_id: Option<ModuleId>,
    pub parent_id: Option<ModuleDefId>,
    pub fields: Vec<FieldDef>,
    pub size: InstanceSize,
    pub ref_fields: Vec<i32>,
    pub vtable: Option<VTableBox>,
}

impl ModuleDef {
    pub fn name(&self, vm: &VM) -> String {
        if let Some(module_id) = self.mod_id {
            let module = vm.modules.idx(module_id);
            let module = module.read();
            let name = vm.interner.str(module.name);

            format!("{}", name)
        } else {
            "<Unknown>".into()
        }
    }
}
