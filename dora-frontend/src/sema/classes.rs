use std::cell::{OnceCell, RefCell};
use std::rc::Rc;

use id_arena::Id;

use crate::element_parser::ParsedModifierList;
use crate::interner::Name;
use dora_parser::ast;

use crate::sema::{
    Element, ElementAccess, ElementId, ElementWithFields, ExtensionDefinitionId, FctDefinitionId,
    FieldDefinitionId, FieldIndex, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId,
    TypeParamDefinition, module_path,
};
use crate::{SourceType, SourceTypeArray, Span, specialize_for_element};

pub type ClassDefinitionId = Id<ClassDefinition>;

#[derive(Debug)]
pub struct ClassDefinition {
    pub id: Option<ClassDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: Option<SourceFileId>,
    pub ast_id: Option<ast::AstId>,
    pub span: Option<Span>,
    pub name: Name,
    pub ty: OnceCell<SourceType>,
    pub is_internal: bool,
    pub internal_resolved: bool,
    pub visibility: Visibility,
    pub field_name_style: ast::FieldNameStyle,

    pub field_ids: OnceCell<Vec<FieldDefinitionId>>,

    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,

    pub type_param_definition: Rc<TypeParamDefinition>,

    // true if this class is the generic Array class
    pub is_array: bool,
    pub is_str: bool,
}

impl ClassDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        ast_id: ast::AstId,
        node: &ast::Class,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id: Some(file_id),
            ast_id: Some(ast_id),
            span: Some(node.span),
            name,
            ty: OnceCell::new(),
            is_internal: modifiers.is_internal,
            internal_resolved: false,
            visibility: modifiers.visibility(),
            field_name_style: node.field_name_style,
            field_ids: OnceCell::new(),
            extensions: RefCell::new(Vec::new()),
            type_param_definition,
            is_array: false,
            is_str: false,
        }
    }

    pub fn new_without_source(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: Option<SourceFileId>,
        span: Option<Span>,
        name: Name,
        visibility: Visibility,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast_id: None,
            span,
            name,
            ty: OnceCell::new(),
            is_internal: false,
            internal_resolved: false,
            visibility,
            field_name_style: ast::FieldNameStyle::Positional,
            field_ids: OnceCell::new(),
            extensions: RefCell::new(Vec::new()),
            type_param_definition,
            is_array: false,
            is_str: false,
        }
    }

    pub fn id(&self) -> ClassDefinitionId {
        self.id.expect("missing id")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Class {
        sa.file(self.file_id())
            .node(self.ast_id.expect("ast missing"))
            .to_class()
            .expect("class expected")
    }

    pub fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing source file")
    }

    pub fn span(&self) -> Span {
        self.span.expect("missing position")
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("not initialized").clone()
    }

    pub fn field_by_name(&self, sa: &Sema, name: Name) -> FieldIndex {
        for &field_id in self.field_ids() {
            let field = sa.field(field_id);
            if field.name == Some(name) {
                return field.index;
            }
        }

        panic!("field not found!")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids.get().expect("missing fields")
    }

    pub fn field_id(&self, index: FieldIndex) -> FieldDefinitionId {
        self.field_ids()[index.to_usize()]
    }

    pub fn name_with_params(&self, sa: &Sema, type_list: &SourceTypeArray) -> String {
        let name = sa.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name(sa))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
    }

    pub fn all_fields_are_public(&self, sa: &Sema) -> bool {
        // "Internal" classes don't have any outside visible fields.
        if self.is_internal {
            return false;
        }

        for &field_id in self.field_ids() {
            let field = sa.field(field_id);
            if !field.visibility.is_public() {
                return false;
            }
        }

        true
    }
}

impl Element for ClassDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Class(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing file_id")
    }

    fn span(&self) -> Span {
        self.span()
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

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn children(&self) -> &[ElementId] {
        unimplemented!()
    }
}

impl ElementAccess for ClassDefinition {
    type Id = ClassDefinitionId;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self {
        sa.class(id)
    }

    fn id(&self) -> Self::Id {
        self.id.expect("missing id")
    }
}

impl ElementWithFields for ClassDefinition {
    fn field_name_style(&self) -> ast::FieldNameStyle {
        self.field_name_style
    }

    fn field_ids(&self) -> &[FieldDefinitionId] {
        self.field_ids()
    }
}

pub fn find_field_in_class(
    sa: &Sema,
    object_type: SourceType,
    name: Name,
) -> Option<(FieldIndex, SourceType)> {
    if let SourceType::Class(cls_id, type_params) = object_type.clone() {
        let cls = sa.class(cls_id);

        for &field_id in cls.field_ids() {
            let field = sa.field(field_id);
            if field.name == Some(name) {
                return Some((
                    field.index,
                    specialize_for_element(sa, field.ty(), cls, &type_params),
                ));
            }
        }

        None
    } else {
        None
    }
}

pub struct Candidate {
    pub object_type: SourceType,
    pub container_type_params: SourceTypeArray,
    pub fct_id: FctDefinitionId,
}

#[derive(Copy, Clone, Debug)]
pub enum Visibility {
    Public,
    Module,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        match self {
            Visibility::Public => true,
            Visibility::Module => false,
        }
    }
}
