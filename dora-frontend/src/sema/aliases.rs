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

impl AliasParent {
    pub fn is_trait(&self) -> bool {
        match self {
            AliasParent::Trait(..) => true,
            _ => false,
        }
    }
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
    pub parsed_ty: Option<ParsedType>,
    pub bounds: Vec<AliasBound>,
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
        bounds: Vec<AliasBound>,
    ) -> AliasDefinition {
        let parsed_ty = if let Some(ref ty) = node.ty {
            Some(ParsedType::new_ast(ty.clone()))
        } else {
            None
        };

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
            parsed_ty,
            bounds,
        }
    }

    pub fn parsed_ty(&self) -> Option<&ParsedType> {
        self.parsed_ty.as_ref()
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().expect("missing type").ty()
    }

    pub fn bounds(&self) -> &[AliasBound] {
        &self.bounds
    }
}

pub struct AliasBound {
    pub ty_ast: ast::Type,
    pub parsed_ty: ParsedType,
}

impl AliasBound {
    pub fn new(ast: ast::Type) -> AliasBound {
        AliasBound {
            ty_ast: ast.clone(),
            parsed_ty: ParsedType::new_ast(ast),
        }
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }
}
