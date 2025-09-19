use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;

use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    AliasDefinitionId, Element, ElementId, FctDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TraitDefinitionId, TypeParamDefinition,
};
use crate::ty::SourceType;
use crate::{ParsedTraitType, ParsedType, TraitType};
use id_arena::Id;

pub type ImplDefinitionId = Id<ImplDefinition>;

#[derive(Debug)]
pub struct ImplDefinition {
    pub id: OnceCell<ImplDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub declaration_span: Span,
    pub span: Span,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub parsed_trait_ty: ParsedTraitType,
    pub parsed_extended_ty: ParsedType,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub aliases: OnceCell<Vec<AliasDefinitionId>>,
    pub trait_method_map: OnceCell<HashMap<FctDefinitionId, FctDefinitionId>>,
    pub trait_alias_map: OnceCell<HashMap<AliasDefinitionId, AliasDefinitionId>>,
}

impl ImplDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Impl,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ImplDefinition {
        ImplDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast_id,
            type_param_definition,
            declaration_span: node.declaration_span,
            span: node.span,
            parsed_trait_ty: ParsedTraitType::new_ast(
                node.trait_type
                    .as_ref()
                    .expect("missing trait type")
                    .clone(),
            ),
            parsed_extended_ty: ParsedType::new_ast(node.extended_type.clone()),
            methods: OnceCell::new(),
            aliases: OnceCell::new(),
            trait_method_map: OnceCell::new(),
            trait_alias_map: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ImplDefinitionId {
        self.id.get().expect("id missing").clone()
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Impl {
        sa.file(self.file_id())
            .node(self.ast_id)
            .to_impl()
            .expect("impl missing")
    }

    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        self.trait_ty().map(|t| t.trait_id)
    }

    pub fn trait_ty(&self) -> Option<TraitType> {
        self.parsed_trait_ty().ty()
    }

    pub fn parsed_trait_ty(&self) -> &ParsedTraitType {
        &self.parsed_trait_ty
    }

    pub fn extended_ty(&self) -> SourceType {
        self.parsed_extended_ty().ty()
    }

    pub fn parsed_extended_ty(&self) -> &ParsedType {
        &self.parsed_extended_ty
    }

    pub fn trait_method_map(&self) -> &HashMap<FctDefinitionId, FctDefinitionId> {
        self.trait_method_map
            .get()
            .expect("missing trait_method_map")
    }

    pub fn trait_alias_map(&self) -> &HashMap<AliasDefinitionId, AliasDefinitionId> {
        self.trait_alias_map.get().expect("missing trait_alias_map")
    }

    pub fn get_method_for_trait_method_id(
        &self,
        trait_method_id: FctDefinitionId,
    ) -> Option<FctDefinitionId> {
        self.trait_method_map().get(&trait_method_id).cloned()
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("missing methods")
    }

    pub fn aliases(&self) -> &[AliasDefinitionId] {
        self.aliases.get().expect("missing methods")
    }
}

impl Element for ImplDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Impl(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id
    }

    fn span(&self) -> Span {
        self.span
    }

    fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id
    }

    fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
    }

    fn to_impl(&self) -> Option<&ImplDefinition> {
        Some(self)
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        Some(self.extended_ty())
    }

    fn visibility(&self) -> super::Visibility {
        unreachable!()
    }
}
