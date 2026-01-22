use crate::{
    ParsedTraitType, ParsedType,
    element_collector::Annotations,
    interner::Name,
    ty::{SourceType, TraitType},
};
use std::cell::OnceCell;
use std::rc::Rc;

use id_arena::Id;

use crate::Span;
use crate::sema::{
    Element, ElementId, ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TraitDefinitionId, TypeParamDefinition, TypeRefArena, TypeRefArenaBuilder,
    Visibility, lower_type,
};
use dora_parser::ast::{self, SyntaxNodeBase};

pub type AliasDefinitionId = Id<AliasDefinition>;

#[derive(PartialEq, Eq, Debug)]
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

    pub fn is_none(&self) -> bool {
        match self {
            AliasParent::None => true,
            _ => false,
        }
    }

    pub fn to_trait_id(&self) -> Option<TraitDefinitionId> {
        match self {
            AliasParent::Trait(id) => Some(*id),
            _ => None,
        }
    }
}

pub struct AliasDefinition {
    pub id: OnceCell<AliasDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub span: Span,
    pub parent: AliasParent,
    pub syntax_node_ptr: ast::SyntaxNodePtr,
    pub modifiers: Annotations,
    pub name: Name,
    pub parsed_ty: Option<ParsedType>,
    pub type_refs: OnceCell<TypeRefArena>,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub bounds: Vec<AliasBound>,
    pub visibility: Visibility,
    pub idx_in_trait: Option<usize>,
}

impl AliasDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        parent: AliasParent,
        ast: ast::AstAlias,
        modifiers: Annotations,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
        bounds: Vec<AliasBound>,
        parsed_ty: Option<ParsedType>,
        idx_in_trait: Option<usize>,
    ) -> AliasDefinition {
        let syntax_node_ptr = ast.syntax_node().as_ptr();

        AliasDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            parent,
            span: ast.span(),
            syntax_node_ptr,
            visibility: modifiers.visibility(),
            modifiers,
            name,
            type_param_definition,
            parsed_ty,
            type_refs: OnceCell::new(),
            bounds,
            idx_in_trait,
        }
    }

    pub fn id(&self) -> AliasDefinitionId {
        self.id.get().cloned().expect("missing id")
    }

    pub fn ast(&self, sa: &Sema) -> ast::AstAlias {
        let file = sa.file(self.file_id).ast();
        file.syntax_by_ptr::<ast::AstAlias>(self.syntax_node_ptr)
    }

    pub fn parsed_ty(&self) -> Option<&ParsedType> {
        self.parsed_ty.as_ref()
    }

    pub fn set_type_refs(&self, type_refs: TypeRefArena) {
        assert!(self.type_refs.set(type_refs).is_ok());
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().expect("missing type").ty()
    }

    pub fn bounds(&self) -> &[AliasBound] {
        &self.bounds
    }

    pub fn idx_in_trait(&self) -> usize {
        self.idx_in_trait.expect("missing idx")
    }
}

impl Element for AliasDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Alias(self.id())
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

    fn to_alias(&self) -> Option<&AliasDefinition> {
        Some(self)
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        None
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn type_ref_arena(&self) -> &TypeRefArena {
        self.type_refs.get().expect("missing type refs")
    }

    fn children(&self) -> &[ElementId] {
        &[]
    }
}

pub struct AliasBound {
    pub parsed_ty: ParsedTraitType,
}

impl AliasBound {
    pub fn new(
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        file_id: SourceFileId,
        ast: ast::AstType,
    ) -> AliasBound {
        let type_ref_id = lower_type(sa, type_ref_arena, file_id, ast);
        AliasBound {
            parsed_ty: ParsedTraitType::new(type_ref_id),
        }
    }

    pub fn ty(&self) -> Option<TraitType> {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedTraitType {
        &self.parsed_ty
    }
}
