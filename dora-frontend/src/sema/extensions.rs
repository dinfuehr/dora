use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::ParsedType;
use crate::interner::Name;
use crate::sema::{
    Element, ElementId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition,
};
use crate::ty::SourceType;
use id_arena::Id;

use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

pub type ExtensionDefinitionId = Id<ExtensionDefinition>;

#[derive(Debug)]
pub struct ExtensionDefinition {
    pub id: OnceCell<ExtensionDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub syntax_node_ptr: ast::SyntaxNodePtr,
    pub span: Span,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub parsed_ty: ParsedType,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub instance_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub children: OnceCell<Vec<ElementId>>,
}

impl ExtensionDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstImpl,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ExtensionDefinition {
        ExtensionDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            syntax_node_ptr: ast.as_ptr(),
            span: ast.span(),
            type_param_definition,
            parsed_ty: ParsedType::new_ast(file_id, ast.extended_type()),
            methods: OnceCell::new(),
            instance_names: RefCell::new(HashMap::new()),
            static_names: RefCell::new(HashMap::new()),
            children: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ExtensionDefinitionId {
        self.id.get().cloned().expect("id missing")
    }

    pub fn ast(&self, sa: &Sema) -> ast::AstImpl {
        let file = sa.file(self.file_id()).ast();
        file.node_by_ptr::<ast::AstImpl>(self.syntax_node_ptr)
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("missing value")
    }
}

impl Element for ExtensionDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Extension(self.id())
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

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        Some(self.ty())
    }

    fn visibility(&self) -> super::Visibility {
        unreachable!()
    }

    fn children(&self) -> &[ElementId] {
        self.children.get().expect("missing children")
    }
}
