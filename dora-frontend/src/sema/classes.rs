use std::cell::{OnceCell, RefCell};
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use id_arena::Id;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, ExtensionDefinitionId, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId,
    Sema, SourceFileId, TypeParamDefinition,
};
use crate::{replace_type, AliasReplacement, ParsedType, SourceType, SourceTypeArray};

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

    pub fields: Vec<Field>,

    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,

    pub type_params: TypeParamDefinition,

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
        type_param_definition: TypeParamDefinition,
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

            fields,

            extensions: RefCell::new(Vec::new()),

            type_params: type_param_definition,

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
        type_param_definition: TypeParamDefinition,
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

            fields,

            extensions: RefCell::new(Vec::new()),

            type_params: type_param_definition,

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

    pub fn type_param_definition(&self) -> &TypeParamDefinition {
        &self.type_params
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("not initialized").clone()
    }

    pub fn field_by_name(&self, name: Name) -> FieldId {
        for field in &self.fields {
            if field.name == name {
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
    pub name: Name,
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
    class: SourceType,
    name: Name,
) -> Option<(SourceType, FieldId, SourceType)> {
    if class.cls_id().is_none() {
        return None;
    }

    let cls_id = class.cls_id().expect("no class");
    let cls = sa.class(cls_id);

    let type_list = class.type_params();

    for field in &cls.fields {
        if field.name == name {
            return Some((
                class,
                field.id,
                replace_type(
                    sa,
                    field.ty(),
                    Some(&type_list),
                    None,
                    AliasReplacement::None,
                ),
            ));
        }
    }

    None
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
