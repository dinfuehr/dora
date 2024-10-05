use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::interner::Name;
use crate::sema::{
    FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, SourceFileId, TypeParamDefinition,
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
    pub ast: Arc<ast::Impl>,
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
        node: &Arc<ast::Impl>,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ExtensionDefinition {
        ExtensionDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
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

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
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
