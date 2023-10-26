use std::collections::HashMap;
use std::sync::Arc;

use parking_lot::RwLock;

use crate::sema::ModuleDefinitionId;
use crate::{Name, SymTable, SymbolKind};
use id_arena::Id;

pub type PackageDefinitionId = Id<PackageDefinition>;

#[derive(Debug)]
pub struct PackageDefinition {
    pub id: Option<PackageDefinitionId>,
    pub name: PackageName,
    pub top_level_module_id: Option<ModuleDefinitionId>,
    pub dependencies: Vec<PackageDependency>,
    pub dependency_names: HashMap<Name, PackageDefinitionId>,
    pub table: Arc<RwLock<SymTable>>,
}

impl PackageDefinition {
    pub fn new(name: PackageName, module_id: ModuleDefinitionId) -> PackageDefinition {
        PackageDefinition {
            id: None,
            name,
            top_level_module_id: Some(module_id),
            dependencies: Vec::new(),
            dependency_names: HashMap::new(),
            table: Arc::new(RwLock::new(SymTable::new())),
        }
    }

    pub fn top_level_module_id(&self) -> ModuleDefinitionId {
        self.top_level_module_id.expect("uninitialized module id")
    }

    pub fn add_dependency(
        &mut self,
        name: Name,
        package_id: PackageDefinitionId,
        top_level_module_id: ModuleDefinitionId,
    ) -> bool {
        let table = self.table.write();

        if table.get(name).is_some() {
            false
        } else {
            let old_value = self
                .table
                .write()
                .insert(name, SymbolKind::Module(top_level_module_id));
            assert!(old_value.is_none());
            self.dependencies
                .push(PackageDependency { name, package_id });
            true
        }
    }
}

#[derive(Debug)]
pub struct PackageDependency {
    pub name: Name,
    pub package_id: PackageDefinitionId,
}

#[derive(Debug)]
pub enum PackageName {
    Stdlib,
    Boots,
    Program,
    External(String),
}
