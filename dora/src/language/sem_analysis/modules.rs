use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::sym::SymTable;
use crate::language::SourceFileId;
use crate::utils::Id;
use crate::vm::VM;

use dora_parser::ast;
use dora_parser::interner::Name;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ModuleDefinitionId(pub usize);

impl ModuleDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ModuleDefinitionId {
    fn from(data: usize) -> ModuleDefinitionId {
        ModuleDefinitionId(data)
    }
}

impl Id for ModuleDefinition {
    type IdType = ModuleDefinitionId;

    fn id_to_usize(id: ModuleDefinitionId) -> usize {
        id.0
    }

    fn usize_to_id(value: usize) -> ModuleDefinitionId {
        ModuleDefinitionId(value)
    }

    fn store_id(value: &mut ModuleDefinition, id: ModuleDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ModuleDefinition {
    pub id: Option<ModuleDefinitionId>,
    pub parent_module_id: Option<ModuleDefinitionId>,
    pub file_id: Option<SourceFileId>,
    pub ast: Option<Arc<ast::Module>>,
    pub name: Option<Name>,
    pub table: Arc<RwLock<SymTable>>,
    pub is_pub: bool,
    pub parents: Vec<ModuleDefinitionId>,
    pub depth: usize,
}

impl ModuleDefinition {
    pub fn predefined(name: Option<Name>) -> ModuleDefinition {
        ModuleDefinition {
            id: None,
            ast: None,
            file_id: None,
            parent_module_id: None,
            name,
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub: true,
            parents: Vec::new(),
            depth: 0,
        }
    }

    pub fn new(
        vm: &mut VM,
        parent_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &Arc<ast::Module>,
    ) -> ModuleDefinition {
        let parent = &vm.namespaces[parent_id].read();
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        ModuleDefinition {
            id: None,
            ast: Some(ast.clone()),
            file_id: Some(file_id),
            parent_module_id: Some(parent_id),
            name: Some(ast.name),
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub: ast.is_pub,
            parents,
            depth,
        }
    }

    pub fn name(&self, vm: &VM) -> String {
        let mut path = String::new();

        for &namespace_id in &self.parents {
            let namespace = &vm.namespaces[namespace_id].read();

            if let Some(name) = namespace.name {
                if !path.is_empty() {
                    path.push_str("::");
                }

                path.push_str(&vm.interner.str(name));
            }
        }

        if let Some(name) = self.name {
            if !path.is_empty() {
                path.push_str("::");
            }

            path.push_str(&vm.interner.str(name));
        }

        path
    }
}

pub fn namespace_package(vm: &VM, namespace_id: ModuleDefinitionId) -> ModuleDefinitionId {
    let namespace = &vm.namespaces[namespace_id].read();

    if let Some(&global_id) = namespace.parents.first() {
        global_id
    } else {
        namespace_id
    }
}

pub fn namespace_path(vm: &VM, namespace_id: ModuleDefinitionId, name: Name) -> String {
    let namespace = &vm.namespaces[namespace_id].read();
    let mut result = namespace.name(vm);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&vm.interner.str(name));
    result
}
