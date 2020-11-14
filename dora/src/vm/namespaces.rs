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
    pub is_pub: bool,
    pub parents: Vec<NamespaceId>,
}

impl NamespaceData {
    pub fn predefined(id: NamespaceId, name: Option<Name>, is_pub: bool) -> NamespaceData {
        NamespaceData {
            id,
            parent_namespace_id: None,
            name,
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub,
            parents: Vec::new(),
        }
    }

    pub fn new(vm: &mut VM, parent_id: NamespaceId, name: Name, is_pub: bool) -> NamespaceId {
        let id: NamespaceId = vm.namespaces.len().into();

        let parent = &vm.namespaces[parent_id.to_usize()];
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let data = NamespaceData {
            id,
            parent_namespace_id: Some(parent_id),
            name: Some(name),
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub,
            parents,
        };

        vm.namespaces.push(data);
        id
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
}

pub fn namespace_contains(vm: &VM, parent_id: NamespaceId, namespace_id: NamespaceId) -> bool {
    if parent_id == namespace_id {
        return true;
    }

    let namespace = &vm.namespaces[namespace_id.to_usize()];
    namespace.parents.contains(&parent_id)
}

pub fn package_namespace(vm: &VM, mut namespace_id: NamespaceId) -> NamespaceId {
    loop {
        let namespace = &vm.namespaces[namespace_id.to_usize()];

        if let Some(parent_namespace_id) = namespace.parent_namespace_id {
            namespace_id = parent_namespace_id;
        } else {
            return namespace.id;
        }
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

pub fn namespace_accessible_from(
    vm: &VM,
    namespace_id: NamespaceId,
    mut from_id: NamespaceId,
) -> bool {
    {
        let namespace = &vm.namespaces[namespace_id.to_usize()];

        if namespace.is_pub {
            // public namespaces are available everywhere
            return true;
        }

        // namespace is available in its immediate parent
        if namespace.parent_namespace_id == Some(from_id) {
            return true;
        }
    }

    loop {
        if from_id == namespace_id {
            return true;
        }

        let namespace = &vm.namespaces[from_id.to_usize()];

        if let Some(parent_namespace_id) = namespace.parent_namespace_id {
            from_id = parent_namespace_id;
        } else {
            return false;
        }
    }
}
