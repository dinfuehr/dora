use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::sync::Arc;

use id_arena::Id;
use once_cell::unsync::OnceCell;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    extension_matches, impl_matches, module_path, Candidate, ExtensionDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, TypeParamDefinition, TypeParamId,
    Visibility,
};
use crate::ty::{SourceType, SourceTypeArray};

pub type StructDefinitionId = Id<StructDefinition>;

#[derive(Debug)]
pub struct StructDefinition {
    pub id: Option<StructDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Struct>,
    pub primitive_ty: Option<SourceType>,
    pub type_params: OnceCell<TypeParamDefinition>,
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
            type_params: OnceCell::new(),
            fields,
            field_names,
            extensions: RefCell::new(Vec::new()),
        }
    }

    pub fn id(&self) -> StructDefinitionId {
        self.id.expect("missing id")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized")
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
            let type_params = (0..self.type_params().len())
                .into_iter()
                .map(|id| SourceType::TypeParam(TypeParamId(id)))
                .collect();
            SourceType::Struct(self.id(), SourceTypeArray::with(type_params))
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
    pub ty: OnceCell<SourceType>,
    pub visibility: Visibility,
}

impl StructDefinitionField {
    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("uninitialized").clone()
    }
}

pub fn find_methods_in_struct(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    for (_id, extension) in sa.extensions.iter() {
        if let Some(bindings) =
            extension_matches(sa, object_type.clone(), type_param_defs, extension.id())
        {
            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.borrow().get(&name) {
                return vec![Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings,
                    fct_id,
                }];
            }
        }
    }

    let mut candidates = Vec::new();

    for (_id, impl_) in sa.impls.iter() {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_.id()) {
            let table = if is_static {
                &impl_.static_names
            } else {
                &impl_.instance_names
            };

            let table = table.borrow();

            if let Some(&method_id) = table.get(&name) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings.clone(),
                    fct_id: method_id,
                });
            }
        }
    }

    candidates
}
