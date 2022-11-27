use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{
    extension_matches, impl_matches, module_path, Candidate, ExtensionDefinitionId,
    ImplDefinitionId, ModuleDefinitionId, PackageDefinitionId, SemAnalysis, SourceFileId,
    TypeParamDefinition, TypeParamId, Visibility,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueDefinitionId(u32);

impl ValueDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Id for ValueDefinition {
    type IdType = ValueDefinitionId;

    fn id_to_usize(id: ValueDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ValueDefinitionId {
        ValueDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut ValueDefinition, id: ValueDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ValueDefinition {
    pub id: Option<ValueDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Value>,
    pub primitive_ty: Option<SourceType>,
    pub type_params: Option<TypeParamDefinition>,
    pub visibility: Visibility,
    pub internal: bool,
    pub internal_resolved: bool,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<ValueDefinitionField>,
    pub field_names: HashMap<Name, ValueDefinitionFieldId>,
    pub impls: Vec<ImplDefinitionId>,
    pub extensions: Vec<ExtensionDefinitionId>,
}

impl ValueDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Value>,
    ) -> ValueDefinition {
        ValueDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            primitive_ty: None,
            visibility: Visibility::from_ast(node.visibility),
            pos: node.pos,
            name: node.name,
            internal: node.internal,
            internal_resolved: false,
            type_params: None,
            fields: Vec::new(),
            field_names: HashMap::new(),
            impls: Vec::new(),
            extensions: Vec::new(),
        }
    }

    pub fn id(&self) -> ValueDefinitionId {
        self.id.expect("missing id")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.as_ref().expect("uninitialized")
    }

    pub fn name(&self, sa: &SemAnalysis) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &SemAnalysis, type_params: &SourceTypeArray) -> String {
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
            SourceType::Value(self.id(), SourceTypeArray::with(type_params))
        }
    }

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" structs don't have any outside visible fields.
        if self.internal {
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
pub struct ValueDefinitionFieldId(pub usize);

impl From<usize> for ValueDefinitionFieldId {
    fn from(data: usize) -> ValueDefinitionFieldId {
        ValueDefinitionFieldId(data)
    }
}

impl ValueDefinitionFieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct ValueDefinitionField {
    pub id: ValueDefinitionFieldId,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub visibility: Visibility,
}

pub fn find_methods_in_value(
    sa: &SemAnalysis,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let struct_id = if object_type.is_primitive() {
        object_type
            .primitive_struct_id(sa)
            .expect("primitive expected")
    } else {
        object_type.value_id().expect("struct expected")
    };

    let struct_ = sa.values.idx(struct_id);
    let struct_ = struct_.read();

    for &extension_id in &struct_.extensions {
        if let Some(bindings) =
            extension_matches(sa, object_type.clone(), type_param_defs, extension_id)
        {
            let extension = sa.extensions[extension_id].read();

            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.get(&name) {
                return vec![Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings,
                    fct_id,
                }];
            }
        }
    }

    let mut candidates = Vec::new();

    for &impl_id in &struct_.impls {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_id) {
            let impl_ = sa.impls[impl_id].read();

            let table = if is_static {
                &impl_.static_names
            } else {
                &impl_.instance_names
            };

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
