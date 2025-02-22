use std::cell::{OnceCell, RefCell};
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use std::sync::Arc;

use id_arena::Id;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;

use crate::sema::{
    module_path, Element, ElementAccess, ElementField, ElementId, ElementWithFields,
    ExtensionDefinitionId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition,
};
use crate::{specialize_for_element, ParsedType, SourceType, SourceTypeArray, Span};

pub type ClassDefinitionId = Id<ClassDefinition>;

#[derive(Debug)]
pub struct ClassDefinition {
    pub id: Option<ClassDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: Option<SourceFileId>,
    pub ast: Option<Arc<ast::Class>>,
    pub span: Option<Span>,
    pub name: Name,
    pub ty: OnceCell<SourceType>,
    pub is_internal: bool,
    pub internal_resolved: bool,
    pub visibility: Visibility,
    pub field_name_style: ast::FieldNameStyle,

    pub fields: Vec<Field>,

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
        ast: &Arc<ast::Class>,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
        fields: Vec<Field>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id: Some(file_id),
            ast: Some(ast.clone()),
            span: Some(ast.span),
            name,
            ty: OnceCell::new(),
            is_internal: modifiers.is_internal,
            internal_resolved: false,
            visibility: modifiers.visibility(),
            field_name_style: ast.field_name_style,

            fields,

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
        fields: Vec<Field>,
    ) -> ClassDefinition {
        ClassDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: None,
            span,
            name,
            ty: OnceCell::new(),
            is_internal: false,
            internal_resolved: false,
            visibility,
            field_name_style: ast::FieldNameStyle::Positional,

            fields,

            extensions: RefCell::new(Vec::new()),

            type_param_definition,

            is_array: false,
            is_str: false,
        }
    }

    pub fn id(&self) -> ClassDefinitionId {
        self.id.expect("missing id")
    }

    pub fn ast(&self) -> &Arc<ast::Class> {
        self.ast.as_ref().expect("ast expected")
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

    pub fn field_by_name(&self, name: Name) -> FieldId {
        for field in &self.fields {
            if field.name == Some(name) {
                return field.id;
            }
        }

        panic!("field not found!")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
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

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" classes don't have any outside visible fields.
        if self.is_internal {
            return false;
        }

        for field in &self.fields {
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

    fn field_name(&self, idx: usize) -> Option<Name> {
        self.fields[idx].name
    }

    fn fields_len(&self) -> usize {
        self.fields.len()
    }

    fn fields<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = ElementField> + 'a> {
        Box::new(self.fields.iter().map(|f| ElementField {
            id: f.id.to_usize(),
            name: f.name,
            ty: f.ty(),
        }))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(pub usize);

impl FieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldId {
    fn from(data: usize) -> FieldId {
        FieldId(data)
    }
}

#[derive(Debug)]
pub struct Field {
    pub id: FieldId,
    pub name: Option<Name>,
    pub parsed_ty: ParsedType,
    pub mutable: bool,
    pub visibility: Visibility,
}

impl Field {
    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }
}

impl Index<FieldId> for Vec<Field> {
    type Output = Field;

    fn index(&self, index: FieldId) -> &Field {
        &self[index.0]
    }
}

impl IndexMut<FieldId> for Vec<Field> {
    fn index_mut(&mut self, index: FieldId) -> &mut Field {
        &mut self[index.0]
    }
}

pub fn find_field_in_class(
    sa: &Sema,
    object_type: SourceType,
    name: Name,
) -> Option<(FieldId, SourceType)> {
    if let SourceType::Class(cls_id, type_params) = object_type.clone() {
        let cls = sa.class(cls_id);

        for field in &cls.fields {
            if field.name == Some(name) {
                return Some((
                    field.id,
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
