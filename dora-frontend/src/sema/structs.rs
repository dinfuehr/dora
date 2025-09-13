use std::cell::{OnceCell, RefCell};
use std::collections::hash_map::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use id_arena::Id;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, new_identity_type_params, Element, ElementAccess, ElementField, ElementId,
    ElementWithFields, ExtensionDefinitionId, FieldDefinition, FieldDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Visibility,
};
use crate::{SourceType, SourceTypeArray};

pub type StructDefinitionId = Id<StructDefinition>;

#[derive(Debug)]
pub struct StructDefinition {
    pub id: Option<StructDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Struct>,
    pub primitive_ty: Option<SourceType>,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub visibility: Visibility,
    pub is_internal: bool,
    pub internal_resolved: bool,
    pub span: Span,
    pub name: Name,
    pub fields: OnceCell<Vec<FieldDefinition>>,
    pub field_names: OnceCell<HashMap<Name, FieldDefinitionId>>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
    pub field_name_style: ast::FieldNameStyle,
}

impl StructDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Struct>,
        modifiers: ParsedModifierList,
        name: Name,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> StructDefinition {
        StructDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            primitive_ty: None,
            visibility: modifiers.visibility(),
            span: node.span,
            name,
            is_internal: modifiers.is_internal,
            internal_resolved: false,
            type_param_definition,
            fields: OnceCell::new(),
            field_names: OnceCell::new(),
            extensions: RefCell::new(Vec::new()),
            field_name_style: node.field_style,
        }
    }

    pub fn id(&self) -> StructDefinitionId {
        self.id.expect("missing id")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &Sema, type_params: &SourceTypeArray) -> String {
        let mut name = self.name(sa);

        if type_params.len() > 0 {
            let type_params = type_params
                .iter()
                .map(|p| p.name(sa))
                .collect::<Vec<_>>()
                .join(", ");

            name.push('[');
            name.push_str(&type_params);
            name.push(']');
        }

        name
    }

    pub fn ty(&self) -> SourceType {
        if let Some(ref primitive_ty) = self.primitive_ty {
            primitive_ty.clone()
        } else {
            SourceType::Struct(
                self.id(),
                new_identity_type_params(0, self.type_param_definition().type_param_count()),
            )
        }
    }

    pub fn field_names(&self) -> &HashMap<Name, FieldDefinitionId> {
        self.field_names.get().expect("missing field_names")
    }

    pub fn fields(&self) -> &[FieldDefinition] {
        self.fields.get().expect("missing fields")
    }

    pub fn field(&self, idx: FieldDefinitionId) -> &FieldDefinition {
        &self.fields()[idx.to_usize()]
    }

    pub fn field_at(&self, idx: usize) -> &FieldDefinition {
        &self.fields()[idx]
    }

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" structs don't have any outside visible fields.
        if self.is_internal {
            return false;
        }

        for field in self.fields() {
            if !field.visibility.is_public() {
                return false;
            }
        }

        true
    }
}

impl Element for StructDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Struct(self.id())
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

    fn visibility(&self) -> Visibility {
        self.visibility
    }
}

impl ElementAccess for StructDefinition {
    type Id = StructDefinitionId;

    fn by_id(sa: &Sema, id: Self::Id) -> &Self {
        sa.struct_(id)
    }

    fn id(&self) -> Self::Id {
        self.id.expect("missing id")
    }
}

impl ElementWithFields for StructDefinition {
    fn field_name_style(&self) -> ast::FieldNameStyle {
        self.field_name_style
    }

    fn fields_len(&self) -> usize {
        self.fields().len()
    }

    fn field_name(&self, idx: usize) -> Option<Name> {
        self.field_at(idx).name
    }

    fn fields<'a>(&'a self) -> Box<dyn DoubleEndedIterator<Item = ElementField> + 'a> {
        Box::new(self.fields().iter().map(|f| ElementField {
            id: f.id.to_usize(),
            name: f.name,
            ty: f.ty(),
        }))
    }
}
