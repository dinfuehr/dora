use parking_lot::RwLock;
use std::sync::Arc;

use crate::language::sym::SymTable;
use crate::vm::VM;

use dora_parser::interner::Name;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct NamespaceId(pub usize);

impl NamespaceId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for NamespaceId {
    fn from(data: usize) -> NamespaceId {
        NamespaceId(data)
    }
}

#[derive(Debug)]
pub struct NamespaceData {
    pub id: NamespaceId,
    pub parent_namespace_id: Option<NamespaceId>,
    pub name: Option<Name>,
    pub table: Arc<RwLock<SymTable>>,
    pub is_pub: bool,
    pub parents: Vec<NamespaceId>,
    pub depth: usize,
}

impl NamespaceData {
    pub fn predefined(id: NamespaceId, name: Option<Name>) -> NamespaceData {
        NamespaceData {
            id,
            parent_namespace_id: None,
            name,
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub: true,
            parents: Vec::new(),
            depth: 0,
        }
    }

    pub fn new(vm: &mut VM, parent_id: NamespaceId, name: Name, is_pub: bool) -> NamespaceId {
        let id: NamespaceId = vm.namespaces.len().into();

        let parent = &vm.namespaces[parent_id.to_usize()];
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        let data = NamespaceData {
            id,
            parent_namespace_id: Some(parent_id),
            name: Some(name),
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub,
            parents,
            depth,
        };

        vm.namespaces.push(data);
        id
    }

    pub fn name(&self, vm: &VM) -> String {
        let mut path = String::new();

        for &namespace_id in &self.parents {
            let namespace = &vm.namespaces[namespace_id.to_usize()];

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

pub fn namespace_package(vm: &VM, namespace_id: NamespaceId) -> NamespaceId {
    let namespace = &vm.namespaces[namespace_id.to_usize()];

    if let Some(&global_id) = namespace.parents.first() {
        global_id
    } else {
        namespace_id
    }
}

pub fn namespace_path(vm: &VM, namespace_id: NamespaceId, name: Name) -> String {
    let namespace = &vm.namespaces[namespace_id.to_usize()];
    let mut result = namespace.name(vm);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&vm.interner.str(name));
    result
}
