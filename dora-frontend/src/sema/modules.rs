use parking_lot::RwLock;
use std::cell::OnceCell;
use std::sync::Arc;

use crate::program_parser::ParsedModifierList;
use crate::sema::{PackageDefinitionId, Sema, Visibility};
use crate::sym::SymTable;
use crate::SourceFileId;

use crate::interner::Name;
use dora_parser::ast;
use id_arena::Id;

pub type ModuleDefinitionId = Id<ModuleDefinition>;

#[derive(Debug)]
pub struct ModuleDefinition {
    pub id: Option<ModuleDefinitionId>,
    pub package_id: Option<PackageDefinitionId>,
    pub parent_module_id: Option<ModuleDefinitionId>,
    pub file_id: Option<SourceFileId>,
    pub ast: Option<Arc<ast::Module>>,
    pub name: Option<Name>,
    pub table: OnceCell<Arc<RwLock<SymTable>>>,
    pub visibility: Visibility,
    pub parents: Vec<ModuleDefinitionId>,
    pub depth: usize,
}

impl ModuleDefinition {
    pub(crate) fn new_top_level(name: Option<Name>) -> ModuleDefinition {
        ModuleDefinition {
            id: None,
            package_id: None,
            ast: None,
            file_id: None,
            parent_module_id: None,
            name,
            table: OnceCell::new(),
            visibility: Visibility::Public,
            parents: Vec::new(),
            depth: 0,
        }
    }

    pub(crate) fn new_inner(
        sa: &mut Sema,
        package_id: PackageDefinitionId,
        parent_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: &Arc<ast::Module>,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> ModuleDefinition {
        let parent = &sa.modules[parent_id];
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        ModuleDefinition {
            id: None,
            package_id: Some(package_id),
            ast: Some(ast.clone()),
            file_id: Some(file_id),
            parent_module_id: Some(parent_id),
            name: Some(name),
            table: OnceCell::new(),
            visibility: modifiers.visibility(),
            parents,
            depth,
        }
    }

    pub fn id(&self) -> ModuleDefinitionId {
        self.id.expect("missing id")
    }

    pub fn package_id(&self) -> PackageDefinitionId {
        self.package_id.expect("uninitialized package_id")
    }

    pub fn table(&self) -> Arc<RwLock<SymTable>> {
        self.table.get().cloned().expect("missing table")
    }

    pub fn name(&self, sa: &Sema) -> String {
        let mut path = String::new();

        for &module_id in &self.parents {
            let module = &sa.modules[module_id];

            if let Some(name) = module.name {
                if !path.is_empty() {
                    path.push_str("::");
                }

                path.push_str(&sa.interner.str(name));
            }
        }

        if let Some(name) = self.name {
            if !path.is_empty() {
                path.push_str("::");
            }

            path.push_str(&sa.interner.str(name));
        }

        path
    }
}

pub fn module_package(sa: &Sema, module_id: ModuleDefinitionId) -> ModuleDefinitionId {
    let module = &sa.modules[module_id];

    if let Some(&global_id) = module.parents.first() {
        global_id
    } else {
        module_id
    }
}

pub fn module_path(sa: &Sema, module_id: ModuleDefinitionId, name: Name) -> String {
    let module = &sa.modules[module_id];
    let mut result = module.name(sa);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&sa.interner.str(name));
    result
}
