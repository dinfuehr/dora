use dora_parser::interner::Name;

use crate::language::sem_analysis::ModuleDefinitionId;
use crate::utils::Id;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct PackageDefinitionId(pub usize);

impl PackageDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl Id for PackageDefinition {
    type IdType = PackageDefinitionId;

    fn id_to_usize(id: PackageDefinitionId) -> usize {
        id.0
    }

    fn usize_to_id(value: usize) -> PackageDefinitionId {
        PackageDefinitionId(value)
    }

    fn store_id(value: &mut PackageDefinition, id: PackageDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct PackageDefinition {
    pub id: Option<PackageDefinitionId>,
    pub name: PackageName,
    pub top_level_module_id: Option<ModuleDefinitionId>,
}

impl PackageDefinition {
    pub fn new(name: PackageName, module_id: ModuleDefinitionId) -> PackageDefinition {
        PackageDefinition {
            id: None,
            name,
            top_level_module_id: Some(module_id),
        }
    }

    pub fn iname(&self) -> Option<Name> {
        match self.name {
            PackageName::External(name) => Some(name),
            _ => None,
        }
    }

    pub fn top_level_module_id(&self) -> ModuleDefinitionId {
        self.top_level_module_id.expect("uninitialized module id")
    }
}

#[derive(Debug)]
pub enum PackageName {
    Stdlib,
    Program,
    External(Name),
}
