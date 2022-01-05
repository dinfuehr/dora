use parking_lot::RwLock;
use std::collections::hash_map::HashMap;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{
    accessible_from, extension_matches, impl_matches, namespace_path, Candidate, ExtensionId,
    FileId, ImplId, NamespaceId, SourceTypeArray, TypeParam, TypeParamDefinition, TypeParamId, VM,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(u32);

impl StructId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl GrowableVec<RwLock<StructDefinition>> {
    pub fn idx(&self, index: StructId) -> Arc<RwLock<StructDefinition>> {
        self.idx_usize(index.0 as usize)
    }
}

impl From<u32> for StructId {
    fn from(data: u32) -> StructId {
        StructId(data)
    }
}

#[derive(Debug)]
pub struct StructDefinition {
    pub id: StructId,
    pub file_id: FileId,
    pub ast: Arc<ast::Struct>,
    pub primitive_ty: Option<SourceType>,
    pub namespace_id: NamespaceId,
    pub type_params: Vec<TypeParam>,
    pub type_params2: TypeParamDefinition,
    pub is_pub: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructDefinitionField>,
    pub field_names: HashMap<Name, StructDefinitionFieldId>,
    pub specializations: RwLock<HashMap<SourceTypeArray, StructInstanceId>>,
    pub impls: Vec<ImplId>,
    pub extensions: Vec<ExtensionId>,
}

impl StructDefinition {
    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    pub fn name_with_params(&self, vm: &VM, type_params: &SourceTypeArray) -> String {
        let mut name = self.name(vm);

        if type_params.len() > 0 {
            let type_params = type_params
                .iter()
                .map(|p| p.name(vm))
                .collect::<Vec<_>>()
                .join(", ");

            name.push('[');
            name.push_str(&type_params);
            name.push(']');
        }

        name
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn ty(&self, vm: &VM) -> SourceType {
        if let Some(ref primitive_ty) = self.primitive_ty {
            primitive_ty.clone()
        } else {
            let type_params = (0..self.type_params.len())
                .into_iter()
                .map(|id| SourceType::TypeParam(TypeParamId(id)))
                .collect();
            let list_id = vm
                .source_type_arrays
                .lock()
                .insert(SourceTypeArray::with(type_params));
            SourceType::Struct(self.id, list_id)
        }
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInstanceId(usize);

impl From<usize> for StructInstanceId {
    fn from(data: usize) -> StructInstanceId {
        StructInstanceId(data)
    }
}

impl GrowableVec<StructInstance> {
    pub fn idx(&self, index: StructInstanceId) -> Arc<StructInstance> {
        self.idx_usize(index.0)
    }
}

pub struct StructInstance {
    pub fields: Vec<StructInstanceField>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

impl StructInstance {
    pub fn contains_references(&self) -> bool {
        !self.ref_fields.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct StructInstanceField {
    pub offset: i32,
    pub ty: SourceType,
}

pub fn struct_accessible_from(vm: &VM, struct_id: StructId, namespace_id: NamespaceId) -> bool {
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    accessible_from(vm, xstruct.namespace_id, xstruct.is_pub, namespace_id)
}

pub fn struct_field_accessible_from(
    vm: &VM,
    struct_id: StructId,
    field_id: StructDefinitionFieldId,
    namespace_id: NamespaceId,
) -> bool {
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    let field = &xstruct.fields[field_id.to_usize()];

    accessible_from(
        vm,
        xstruct.namespace_id,
        xstruct.is_pub && field.is_pub,
        namespace_id,
    )
}

pub fn find_methods_in_struct(
    vm: &VM,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    type_param_defs2: Option<&TypeParamDefinition>,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let struct_id = if object_type.is_primitive() {
        object_type
            .primitive_struct_id(vm)
            .expect("primitive expected")
    } else {
        object_type.struct_id().expect("struct expected")
    };

    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    for &extension_id in &xstruct.extensions {
        if let Some(bindings) = extension_matches(
            vm,
            object_type.clone(),
            type_param_defs,
            type_param_defs2,
            extension_id,
        ) {
            let extension = vm.extensions[extension_id].read();

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
        if let Some(bindings) = impl_matches(
            vm,
            object_type.clone(),
            type_param_defs,
            type_param_defs2,
            impl_id,
        ) {
            let ximpl = vm.impls[impl_id].read();

            let table = if is_static {
                &ximpl.static_names
            } else {
                &ximpl.instance_names
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
