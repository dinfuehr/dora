use std::cell::OnceCell;
use std::rc::Rc;

use dora_parser::GreenId;
use dora_parser::ast::SyntaxNodeBase;
use dora_parser::{Span, ast};
use id_arena::Id;

use crate::SourceType;
use crate::element_collector::Annotations;
use crate::sema::{
    Element, ElementId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, Visibility,
};

pub type UseDefinitionId = Id<UseDefinition>;

pub struct UseDefinition {
    pub id: OnceCell<UseDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub syntax_node_ptr: ast::SyntaxNodePtr,
    pub path_ast_id: GreenId,
    pub span: Span,
    pub visibility: Visibility,
}

impl UseDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstUse,
        modifiers: Annotations,
    ) -> UseDefinition {
        UseDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            syntax_node_ptr: ast.as_ptr(),
            path_ast_id: ast.path().id(),
            span: ast.span(),
            visibility: modifiers.visibility(),
        }
    }

    pub fn id(&self) -> UseDefinitionId {
        self.id.get().cloned().expect("missing id")
    }

    pub fn ast(&self, sa: &Sema) -> ast::AstUse {
        let file = sa.file(self.file_id).ast();
        file.syntax_by_ptr::<ast::AstUse>(self.syntax_node_ptr)
    }
}

impl Element for UseDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Use(self.id())
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
        unreachable!()
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        None
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn children(&self) -> &[ElementId] {
        &[]
    }
}
