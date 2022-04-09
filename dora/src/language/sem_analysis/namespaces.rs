use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::sym::SymTable;
use crate::utils::Id;
use crate::vm::VM;

use dora_parser::ast::Namespace;
use dora_parser::interner::Name;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct NamespaceDefinitionId(pub usize);

impl NamespaceDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for NamespaceDefinitionId {
    fn from(data: usize) -> NamespaceDefinitionId {
        NamespaceDefinitionId(data)
    }
}

impl Id for NamespaceDefinition {
    type IdType = NamespaceDefinitionId;

    fn id_to_usize(id: NamespaceDefinitionId) -> usize {
        id.0
    }

    fn usize_to_id(value: usize) -> NamespaceDefinitionId {
        NamespaceDefinitionId(value)
    }

    fn store_id(value: &mut NamespaceDefinition, id: NamespaceDefinitionId) {
        value.id = id;
    }
}

#[derive(Debug)]
pub struct NamespaceDefinition {
    pub id: NamespaceDefinitionId,
    pub ast: Option<Arc<Namespace>>,
    pub parent_namespace_id: Option<NamespaceDefinitionId>,
    pub name: Option<Name>,
    pub table: Arc<RwLock<SymTable>>,
    pub is_pub: bool,
    pub parents: Vec<NamespaceDefinitionId>,
    pub depth: usize,
}

impl NamespaceDefinition {
    pub fn predefined(id: NamespaceDefinitionId, name: Option<Name>) -> NamespaceDefinition {
        NamespaceDefinition {
            id,
            ast: None,
            parent_namespace_id: None,
            name,
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub: true,
            parents: Vec::new(),
            depth: 0,
        }
    }

    pub fn new(
        vm: &mut VM,
        parent_id: NamespaceDefinitionId,
        ast: &Arc<Namespace>,
    ) -> NamespaceDefinition {
        let id: NamespaceDefinitionId = vm.namespaces.len().into();

        let parent = &vm.namespaces[parent_id].read();
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        NamespaceDefinition {
            id,
            ast: Some(ast.clone()),
            parent_namespace_id: Some(parent_id),
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

pub fn namespace_package(vm: &VM, namespace_id: NamespaceDefinitionId) -> NamespaceDefinitionId {
    let namespace = &vm.namespaces[namespace_id].read();

    if let Some(&global_id) = namespace.parents.first() {
        global_id
    } else {
        namespace_id
    }
}

pub fn namespace_path(vm: &VM, namespace_id: NamespaceDefinitionId, name: Name) -> String {
    let namespace = &vm.namespaces[namespace_id].read();
    let mut result = namespace.name(vm);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&vm.interner.str(name));
    result
}
