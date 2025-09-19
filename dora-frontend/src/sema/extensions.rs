use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::interner::Name;
use crate::sema::{
    Element, ElementId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition,
};
use crate::ty::SourceType;
use crate::ParsedType;
use id_arena::Id;

use dora_parser::ast;
use dora_parser::Span;

pub type ExtensionDefinitionId = Id<ExtensionDefinition>;

#[derive(Debug)]
pub struct ExtensionDefinition {
    pub id: OnceCell<ExtensionDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub extended_type: ast::AstId,
    pub span: Span,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub parsed_ty: ParsedType,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub instance_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: RefCell<HashMap<Name, FctDefinitionId>>,
}

impl ExtensionDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Impl,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ExtensionDefinition {
        ExtensionDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast_id,
            span: node.span,
            extended_type: node.extended_type,
            type_param_definition,
            parsed_ty: ParsedType::new_ast(node.extended_type.clone()),
            methods: OnceCell::new(),
            instance_names: RefCell::new(HashMap::new()),
            static_names: RefCell::new(HashMap::new()),
        }
    }

    pub fn id(&self) -> ExtensionDefinitionId {
        self.id.get().cloned().expect("id missing")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Impl {
        sa.file(self.file_id())
            .node(self.ast_id)
            .to_impl()
            .expect("impl missing")
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
}
