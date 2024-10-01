use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use id_arena::Id;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, new_identity_type_params, ExtensionDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, Visibility,
};
use crate::{ParsedType, SourceType, SourceTypeArray};

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
    pub fields: Vec<StructDefinitionField>,
    pub field_names: HashMap<Name, StructDefinitionFieldId>,
    pub extensions: RefCell<Vec<ExtensionDefinitionId>>,
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
        fields: Vec<StructDefinitionField>,
    ) -> StructDefinition {
        let mut field_names = HashMap::new();

        for field in &fields {
            if field_names.contains_key(&field.name) {
                continue;
            }

            field_names.insert(field.name, field.id);
        }

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
            fields,
            field_names,
            extensions: RefCell::new(Vec::new()),
        }
    }

    pub fn id(&self) -> StructDefinitionId {
        self.id.expect("missing id")
    }

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
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
                new_identity_type_params(self.type_param_definition().type_param_count()),
            )
        }
    }

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" structs don't have any outside visible fields.
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StructDefinitionFieldId(pub usize);

impl From<usize> for StructDefinitionFieldId {
    fn from(data: usize) -> StructDefinitionFieldId {
        StructDefinitionFieldId(data)
    }
}

impl StructDefinitionFieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct StructDefinitionField {
    pub id: StructDefinitionFieldId,
    pub span: Span,
    pub name: Name,
    pub parsed_ty: ParsedType,
    pub visibility: Visibility,
}

impl StructDefinitionField {
    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }
}
