use std::cell::OnceCell;
use std::rc::Rc;

use crate::ParsedType;
use crate::element_collector::Annotations;
use crate::interner::Name;
use crate::sema::{
    Body, Element, ElementId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition, Visibility, module_path,
};
use crate::ty::SourceType;
use dora_bytecode::BytecodeFunction;
use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};
use id_arena::Id;

pub type GlobalDefinitionId = Id<GlobalDefinition>;

#[derive(Debug)]
pub struct GlobalDefinition {
    pub id: Option<GlobalDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub syntax_node_ptr: ast::SyntaxNodePtr,
    pub has_initial_value: bool,
    pub span: Span,
    pub visibility: Visibility,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub name: Name,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub initializer: OnceCell<FctDefinitionId>,
    pub body: OnceCell<Body>,
    pub bytecode: OnceCell<BytecodeFunction>,
}

impl GlobalDefinition {
    pub(crate) fn new(
        sa: &mut Sema,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstGlobal,
        modifiers: Annotations,
        name: Name,
    ) -> GlobalDefinition {
        GlobalDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            syntax_node_ptr: ast.as_ptr(),
            has_initial_value: ast.initial_value().is_some(),
            span: ast.span(),
            name,
            visibility: modifiers.visibility(),
            parsed_ty: ParsedType::new_ast_opt(sa, file_id, ast.data_type()),
            mutable: ast.mutable(),
            type_param_definition: TypeParamDefinition::empty(),
            initializer: OnceCell::new(),
            body: OnceCell::new(),
            bytecode: OnceCell::new(),
        }
    }

    pub fn id(&self) -> GlobalDefinitionId {
        self.id.expect("id missing")
    }

    pub fn ast(&self, sa: &Sema) -> ast::AstGlobal {
        let file = sa.file(self.file_id()).ast();
        file.syntax_by_ptr::<ast::AstGlobal>(self.syntax_node_ptr)
    }

    pub fn has_initial_value(&self) -> bool {
        self.has_initial_value
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

    pub fn analysis(&self) -> &Body {
        self.body()
    }

    pub fn body(&self) -> &Body {
        self.body.get().expect("missing body")
    }

    pub fn set_body(&self, body: Body) {
        assert!(self.body.set(body).is_ok());
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
