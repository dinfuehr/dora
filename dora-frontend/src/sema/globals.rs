use std::cell::OnceCell;
use std::rc::Rc;

use crate::ParsedType;
use crate::element_parser::ParsedModifierList;
use crate::interner::Name;
use crate::sema::{
    AnalysisData, Element, ElementId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId,
    Sema, SourceFileId, TypeParamDefinition, Visibility, module_path,
};
use crate::ty::SourceType;
use dora_bytecode::BytecodeFunction;
use dora_parser::Span;
use dora_parser::ast;
use id_arena::Id;

pub type GlobalDefinitionId = Id<GlobalDefinition>;

#[derive(Debug)]
pub struct GlobalDefinition {
    pub id: Option<GlobalDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast_id: ast::AstId,
    pub initial_value_ast_id: Option<ast::AstId>,
    pub span: Span,
    pub visibility: Visibility,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub name: Name,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub initializer: OnceCell<FctDefinitionId>,
    pub analysis: OnceCell<AnalysisData>,
    pub bytecode: OnceCell<BytecodeFunction>,
}

impl GlobalDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Global,
        modifiers: ParsedModifierList,
        name: Name,
    ) -> GlobalDefinition {
        GlobalDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast_id,
            initial_value_ast_id: node.initial_value,
            span: node.span,
            name,
            visibility: modifiers.visibility(),
            parsed_ty: ParsedType::new_ast(node.data_type.clone()),
            mutable: node.mutable,
            type_param_definition: TypeParamDefinition::empty(),
            initializer: OnceCell::new(),
            analysis: OnceCell::new(),
            bytecode: OnceCell::new(),
        }
    }

    pub fn id(&self) -> GlobalDefinitionId {
        self.id.expect("id missing")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Global {
        sa.file(self.file_id())
            .ast()
            .node(self.ast_id)
            .to_global()
            .expect("missing global")
    }

    pub fn has_initial_value(&self) -> bool {
        self.initial_value_ast_id.is_some()
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

    pub fn analysis(&self) -> &AnalysisData {
        self.analysis.get().expect("missing analysis")
    }

    pub fn bytecode(&self) -> &BytecodeFunction {
        self.bytecode.get().expect("missing bytecode")
    }
}

impl Element for GlobalDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Global(self.id())
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

    fn children(&self) -> &[ElementId] {
        &[]
    }
}
