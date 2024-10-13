use crate::{
    interner::Name,
    program_parser::ParsedModifierList,
    ty::{SourceType, TraitType},
    ParsedTraitType, ParsedType,
};
use std::cell::OnceCell;
use std::rc::Rc;
use std::sync::Arc;

use id_arena::Id;

use crate::sema::{
    Element, ElementId, ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TraitDefinitionId, TypeParamDefinition, Visibility,
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

    pub fn is_impl(&self) -> bool {
        match self {
            AliasParent::Impl(..) => true,
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
    pub type_param_definition: Rc<TypeParamDefinition>,
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
        type_param_definition: Rc<TypeParamDefinition>,
        bounds: Vec<AliasBound>,
        parsed_ty: Option<ParsedType>,
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
            type_param_definition,
            parsed_ty,
            bounds,
        }
    }

    pub fn id(&self) -> AliasDefinitionId {
        self.id.get().cloned().expect("missing id")
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

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
    }
}

impl Element for AliasDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Alias(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id
    }

    fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id
    }

    fn type_param_definition(&self) -> Option<&Rc<TypeParamDefinition>> {
        None
    }

    fn to_alias(&self) -> Option<&AliasDefinition> {
        Some(self)
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        None
    }
}

pub struct AliasBound {
    pub ty_ast: ast::Type,
    pub parsed_ty: ParsedTraitType,
}

impl AliasBound {
    pub fn new(ast: ast::Type) -> AliasBound {
        AliasBound {
            ty_ast: ast.clone(),
            parsed_ty: ParsedTraitType::new_ast(ast),
        }
    }

    pub fn ty(&self) -> Option<TraitType> {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedTraitType {
        &self.parsed_ty
    }
}
