use parking_lot::RwLock;
use std::sync::Arc;

use crate::sym::SymTable;
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
}

impl NamespaceData {
    pub fn new(id: NamespaceId, parent_id: Option<NamespaceId>) -> NamespaceData {
        NamespaceData {
            id,
            parent_namespace_id: parent_id,
            name: None,
            table: Arc::new(RwLock::new(SymTable::new())),
        }
    }

    pub fn new_with_name(
        id: NamespaceId,
        parent_id: Option<NamespaceId>,
        name: Name,
    ) -> NamespaceData {
        NamespaceData {
            id,
            parent_namespace_id: parent_id,
            name: Some(name),
            table: Arc::new(RwLock::new(SymTable::new())),
        }
    }

    pub fn name(&self, vm: &VM) -> String {
        let mut path = Vec::new();
        let mut owner_namespace_id = self.parent_namespace_id;

        while let Some(namespace_id) = owner_namespace_id {
            let ns = &vm.namespaces[namespace_id.to_usize()];

            if let Some(name) = ns.name {
                path.push(vm.interner.str(name).to_string());
            }

            owner_namespace_id = ns.parent_namespace_id;
        }

        path.reverse();

        if let Some(name) = self.name {
            path.push(vm.interner.str(name).to_string());
        }

        path.join("::")
    }

    pub fn path_with_name(vm: &VM, namespace_id: Option<NamespaceId>, name: Name) -> String {
        if let Some(namespace_id) = namespace_id {
            let ns = &vm.namespaces[namespace_id.to_usize()];
            let mut result = ns.name(vm);
            result.push_str("::");
            result.push_str(&vm.interner.str(name));
            result
        } else {
            vm.interner.str(name).to_string()
        }
    }
}

pub fn namespace_contains(vm: &VM, parent_id: NamespaceId, mut namespace_id: NamespaceId) -> bool {
    loop {
        if parent_id == namespace_id {
            return true;
        }

        let namespace = &vm.namespaces[namespace_id.to_usize()];

        if let Some(parent_namespace_id) = namespace.parent_namespace_id {
            namespace_id = parent_namespace_id;
        } else {
            return false;
        }
    }
}
