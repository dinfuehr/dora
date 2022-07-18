use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{
    extension_matches, impl_matches, module_path, Candidate, ExtensionDefinitionId,
    ImplDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId, TypeParam, TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDefinitionId(u32);

impl StructDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Id for StructDefinition {
    type IdType = StructDefinitionId;

    fn id_to_usize(id: StructDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> StructDefinitionId {
        StructDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut StructDefinition, id: StructDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct StructDefinition {
    pub id: Option<StructDefinitionId>,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Struct>,
    pub primitive_ty: Option<SourceType>,
    pub module_id: ModuleDefinitionId,
    pub type_params: Vec<TypeParam>,
    pub is_pub: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructDefinitionField>,
    pub field_names: HashMap<Name, StructDefinitionFieldId>,
    pub impls: Vec<ImplDefinitionId>,
    pub extensions: Vec<ExtensionDefinitionId>,
}

impl StructDefinition {
    pub fn new(
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        node: &Arc<ast::Struct>,
    ) -> StructDefinition {
        let mut type_params = Vec::new();

        if let Some(ref ast_type_params) = node.type_params {
            for param in ast_type_params {
                type_params.push(TypeParam::new(param.name));
            }
        }

        StructDefinition {
            id: None,
            file_id,
            ast: node.clone(),
            module_id,
            primitive_ty: None,
            is_pub: node.is_pub,
            pos: node.pos,
            name: node.name,
            internal: node.internal,
            internal_resolved: false,
            type_params,
            fields: Vec::new(),
            field_names: HashMap::new(),
            impls: Vec::new(),
            extensions: Vec::new(),
        }
    }

    pub fn id(&self) -> StructDefinitionId {
        self.id.expect("missing id")
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
            let type_params = (0..self.type_params.len())
                .into_iter()
                .map(|id| SourceType::TypeParam(TypeParamId(id)))
                .collect();
            SourceType::Struct(self.id(), SourceTypeArray::with(type_params))
        }
    }

    pub fn all_fields_are_public(&self) -> bool {
        // "Internal" structs don't have any outside visible fields.
        if self.internal {
            return false;
        }

        for field in &self.fields {
            if !field.is_pub {
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
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub is_pub: bool,
}

pub fn find_methods_in_struct(
    sa: &SemAnalysis,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let struct_id = if object_type.is_primitive() {
        object_type
            .primitive_struct_id(sa)
            .expect("primitive expected")
    } else {
        object_type.struct_id().expect("struct expected")
    };

    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    for &extension_id in &xstruct.extensions {
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

    for &impl_id in &xstruct.impls {
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
