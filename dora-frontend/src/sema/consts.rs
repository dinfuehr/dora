use id_arena::Id;
use std::cell::OnceCell;
use std::rc::Rc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
pub use dora_bytecode::ConstValue;
use dora_parser::Span;
use dora_parser::ast;

use crate::ParsedType;
use crate::sema::{
    Element, ElementId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, Visibility, module_path,
};
use crate::ty::SourceType;

pub type ConstDefinitionId = Id<ConstDefinition>;

#[derive(Debug)]
pub struct ConstDefinition {
    pub id: Option<ConstDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub visibility: Visibility,
    pub span: Span,
    pub name: Name,
    pub parsed_ty: ParsedType,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub expr: ast::AstId,
    pub value: OnceCell<ConstValue>,
}

impl ConstDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Const,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> ConstDefinition {
        ConstDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast_id,
            span: node.span,
            name,
            visibility: modifiers.visibility(),
            type_param_definition: TypeParamDefinition::empty(),
            parsed_ty: ParsedType::new_ast(node.data_type.clone()),
            expr: node.expr.clone(),
            value: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ConstDefinitionId {
        self.id.expect("id missing")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn value(&self) -> &ConstValue {
        self.value.get().expect("uninitialized")
    }
}

impl Element for ConstDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Const(self.id())
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
        None
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }
}
