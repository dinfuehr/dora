use crate::{interner::Name, program_parser::ParsedModifierList, ty::SourceType};
use std::cell::OnceCell;
use std::sync::Arc;

use id_arena::Id;

use crate::sema::{
    ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, SourceFileId, TraitDefinitionId,
};
use dora_parser::ast;

pub type AliasDefinitionId = Id<AliasDefinition>;

pub enum AliasParent {
    None,
    Trait(TraitDefinitionId),
    Impl(ImplDefinitionId),
}

pub struct AliasDefinition {
    pub id: OnceCell<AliasDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub parent: AliasParent,
    pub node: Arc<ast::TypeAlias>,
    pub modifiers: ParsedModifierList,
    pub name: Name,
    pub ty: OnceCell<SourceType>,
}

impl AliasDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        parent: AliasParent,
        node: &Arc<ast::TypeAlias>,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> AliasDefinition {
        AliasDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            parent,
            node: node.clone(),
            modifiers,
            name,
            ty: OnceCell::new(),
        }
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().cloned().expect("missing ty")
    }
}
