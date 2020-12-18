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

pub fn namespace_contains(vm: &VM, parent_id: NamespaceId, child_id: NamespaceId) -> bool {
    if parent_id == child_id {
        return true;
    }

    let namespace = &vm.namespaces[child_id.to_usize()];
    namespace.parents.contains(&parent_id)
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

pub fn namespace_accessible_from(vm: &VM, target_id: NamespaceId, from_id: NamespaceId) -> bool {
    accessible_from(vm, target_id, true, from_id)
}

pub fn accessible_from(
    vm: &VM,
    target_id: NamespaceId,
    element_pub: bool,
    from_id: NamespaceId,
) -> bool {
    // each namespace can access itself
    if target_id == from_id {
        return true;
    }

    // namespaces can access all their parents
    if namespace_contains(vm, target_id, from_id) {
        return true;
    }

    // find the common parent of both namespaces
    let common_parent_id = common_parent(vm, target_id, from_id);

    let target = &vm.namespaces[target_id.to_usize()];

    if let Some(common_parent_id) = common_parent_id {
        let common_parent_depth = vm.namespaces[common_parent_id.to_usize()].depth;

        if common_parent_depth + 1 == target.depth {
            // siblings are accessible
            element_pub
        } else {
            let start_depth = common_parent_depth + 2;
            for ns_id in &target.parents[start_depth..] {
                let ns = &vm.namespaces[ns_id.to_usize()];
                if !ns.is_pub {
                    return false;
                }
            }

            target.is_pub && element_pub
        }
    } else {
        // no common parent: means we try to access another package
        // the whole path needs to be public
        for ns_id in &target.parents {
            let ns = &vm.namespaces[ns_id.to_usize()];
            if !ns.is_pub {
                return false;
            }
        }

        target.is_pub && element_pub
    }
}

fn common_parent(vm: &VM, lhs_id: NamespaceId, rhs_id: NamespaceId) -> Option<NamespaceId> {
    if lhs_id == rhs_id {
        return Some(lhs_id);
    }

    let lhs = &vm.namespaces[lhs_id.to_usize()];
    let rhs = &vm.namespaces[rhs_id.to_usize()];

    if lhs.depth > rhs.depth {
        if lhs.parents[rhs.depth] == rhs_id {
            return Some(rhs_id);
        } else {
            // do nothing
        }
    } else if rhs.depth > lhs.depth {
        if rhs.parents[lhs.depth] == lhs_id {
            return Some(lhs_id);
        } else {
            // do nothing
        }
    }

    let start = std::cmp::min(lhs.depth, rhs.depth);

    for depth in (0..start).rev() {
        if lhs.parents[depth] == rhs.parents[depth] {
            return Some(lhs.parents[depth]);
        }
    }

    None
}
