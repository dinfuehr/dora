use crate::{interner::Name, program_parser::ParsedModifierList, ty::SourceType, ParsedType};
use std::cell::OnceCell;
use std::sync::Arc;

use id_arena::Id;

use crate::sema::{
    ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, SourceFileId, TraitDefinitionId,
    Visibility,
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
    pub bounds: OnceCell<Vec<Box<ParsedType>>>,
    pub visibility: Visibility,
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
            visibility: modifiers.visibility(),
            modifiers,
            name,
            ty: OnceCell::new(),
            bounds: OnceCell::new(),
        }
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().cloned().expect("missing ty")
    }

    pub fn bounds(&self) -> &Vec<Box<ParsedType>> {
        self.bounds.get().expect("missing bounds")
    }
}
