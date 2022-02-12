use parking_lot::RwLock;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;

use crate::language::sym::SymTable;
use crate::vm::{AnnotationDefinition, FileId, SemAnalysis, VM};

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
    pub file_id: FileId,
    pub ast: Option<Arc<ast::Namespace>>,
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
            file_id: FileId(0),
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
        file_id: FileId,
        parent_id: NamespaceId,
        ast: &Arc<ast::Namespace>,
    ) -> NamespaceData {
        let id: NamespaceId = vm.namespaces.len().into();

        let parent = &vm.namespaces[parent_id.to_usize()].read();
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        NamespaceData {
            id,
            file_id,
            ast: Some(ast.clone()),
            parent_namespace_id: Some(parent_id),
            name: Some(ast.name),
            table: Arc::new(RwLock::new(SymTable::new())),
            is_pub: ast.is_pub,
            parents,
            depth,
        }
    }

    pub fn init_modifiers(&mut self, sa: &SemAnalysis) {
        #[allow(unused_variables)]
        if let Some(ast) = &self.ast {
            let annotation_usages = &ast::AnnotationUsages::new();
            self.is_pub = AnnotationDefinition::is_pub(annotation_usages, sa);
            AnnotationDefinition::reject_modifiers(
                sa,
                self.file_id,
                annotation_usages,
                &[
                    sa.known.annotations.abstract_,
                    sa.known.annotations.final_,
                    sa.known.annotations.internal,
                    sa.known.annotations.open,
                    sa.known.annotations.optimize_immediately,
                    sa.known.annotations.override_,
                    sa.known.annotations.static_,
                    sa.known.annotations.test,
                ],
            );
        }
    }

    pub fn name(&self, vm: &VM) -> String {
        let mut path = String::new();

        for &namespace_id in &self.parents {
            let namespace = &vm.namespaces[namespace_id.to_usize()].read();

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
    let namespace = &vm.namespaces[namespace_id.to_usize()].read();

    if let Some(&global_id) = namespace.parents.first() {
        global_id
    } else {
        namespace_id
    }
}

pub fn namespace_path(vm: &VM, namespace_id: NamespaceId, name: Name) -> String {
    let namespace = &vm.namespaces[namespace_id.to_usize()].read();
    let mut result = namespace.name(vm);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&vm.interner.str(name));
    result
}
