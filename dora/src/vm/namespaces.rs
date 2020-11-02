use parking_lot::RwLock;
use std::sync::Arc;

use crate::sym::SymTable;
use crate::vm::{FileId, VM};

use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct NamespaceId(usize);

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
    pub file_id: FileId,
    pub pos: Position,
    pub namespace_id: Option<NamespaceId>,
    pub name: Name,
    pub table: Arc<RwLock<SymTable>>,
}

impl NamespaceData {
    pub fn name(&self, vm: &VM) -> String {
        let mut path = String::new();
        let mut owner_namespace_id = self.namespace_id;

        while let Some(namespace_id) = owner_namespace_id {
            if !path.is_empty() {
                path.push_str("::");
            }

            let ns = &vm.namespaces[namespace_id.to_usize()];
            path.push_str(&vm.interner.str(ns.name));

            owner_namespace_id = ns.namespace_id;
        }

        if path.is_empty() {
            vm.interner.str(self.name).to_string()
        } else {
            path.push_str("::");
            path.push_str(&vm.interner.str(self.name));
            path
        }
    }

    pub fn path_with_name(vm: &VM, namespace_id: Option<NamespaceId>, name: Name) -> String {
        if let Some(namespace_id) = namespace_id {
            let ns = &vm.namespaces[namespace_id.to_usize()];
            let mut result = ns.name(vm);
            result.push_str("::");
            result.push_str(&vm.interner.str(ns.name));
            result
        } else {
            vm.interner.str(name).to_string()
        }
    }
}
