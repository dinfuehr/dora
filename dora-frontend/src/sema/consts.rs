use id_arena::Id;
use std::cell::OnceCell;
use std::rc::Rc;

use crate::element_collector::Annotations;
use crate::interner::Name;
pub use dora_bytecode::ConstValue;
use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::ParsedType;
use crate::sema::{
    Body, Element, ElementId, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, TypeRefArena, TypeRefArenaBuilder, Visibility, lower_type, module_path,
};
use crate::ty::SourceType;

pub type ConstDefinitionId = Id<ConstDefinition>;

#[derive(Debug)]
pub struct ConstDefinition {
    pub id: Option<ConstDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub syntax_node_ptr: ast::SyntaxNodePtr,
    pub visibility: Visibility,
    pub span: Span,
    pub name: Name,
    pub parsed_ty: ParsedType,
    pub type_refs: OnceCell<TypeRefArena>,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub value: OnceCell<ConstValue>,
    body: OnceCell<Body>,
}

impl ConstDefinition {
    pub(crate) fn new(
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast: ast::AstConst,
        modifiers: Annotations,
        name: Name,
    ) -> ConstDefinition {
        ConstDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            syntax_node_ptr: ast.as_ptr(),
            span: ast.span(),
            name,
            visibility: modifiers.visibility(),
            type_param_definition: TypeParamDefinition::empty(),
            parsed_ty: ParsedType::new_opt(
                ast.data_type()
                    .map(|ty| lower_type(sa, type_ref_arena, file_id, ty)),
            ),
            type_refs: OnceCell::new(),
            value: OnceCell::new(),
            body: OnceCell::new(),
        }
    }

    pub fn has_expr(&self, sa: &Sema) -> bool {
        self.ast(sa).expr().is_some()
    }

    pub fn body(&self) -> &Body {
        self.body.get().expect("body not initialized")
    }

    pub fn set_body(&self, body: Body) {
        assert!(self.body.set(body).is_ok());
    }

    pub fn id(&self) -> ConstDefinitionId {
        self.id.expect("id missing")
    }

    pub fn ast(&self, sa: &Sema) -> ast::AstConst {
        let file = sa.file(self.file_id).ast();
        file.syntax_by_ptr::<ast::AstConst>(self.syntax_node_ptr)
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

    pub fn set_type_refs(&self, type_refs: TypeRefArena) {
        assert!(self.type_refs.set(type_refs).is_ok());
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

    fn type_ref_arena(&self) -> &TypeRefArena {
        self.type_refs.get().expect("missing type refs")
    }

    fn children(&self) -> &[ElementId] {
        &[]
    }
}
