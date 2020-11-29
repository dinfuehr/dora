use parking_lot::RwLock;
use std::collections::hash_map::HashMap;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::semck::specialize::replace_type_param;
use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{
    accessible_from, extension_matches, impl_matches, namespace_path, Candidate, ExtensionId,
    FileId, ImplId, NamespaceId, SourceTypeArray, TypeParam, TypeParamId, VM,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(u32);

impl GrowableVec<RwLock<StructData>> {
    pub fn idx(&self, index: StructId) -> Arc<RwLock<StructData>> {
        self.idx_usize(index.0 as usize)
    }
}

impl From<u32> for StructId {
    fn from(data: u32) -> StructId {
        StructId(data)
    }
}

#[derive(Debug)]
pub struct StructData {
    pub id: StructId,
    pub file_id: FileId,
    pub ast: Arc<ast::Struct>,
    pub namespace_id: NamespaceId,
    pub type_params: Vec<TypeParam>,
    pub is_pub: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructFieldData>,
    pub field_names: HashMap<Name, StructFieldId>,
    pub specializations: RwLock<HashMap<SourceTypeArray, StructDefId>>,
    pub impls: Vec<ImplId>,
    pub extensions: Vec<ExtensionId>,
}

impl StructData {
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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StructFieldId(pub usize);

impl From<usize> for StructFieldId {
    fn from(data: usize) -> StructFieldId {
        StructFieldId(data)
    }
}

impl StructFieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct StructFieldData {
    pub id: StructFieldId,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDefId(usize);

impl From<usize> for StructDefId {
    fn from(data: usize) -> StructDefId {
        StructDefId(data)
    }
}

impl GrowableVec<StructDef> {
    pub fn idx(&self, index: StructDefId) -> Arc<StructDef> {
        self.idx_usize(index.0)
    }
}

pub struct StructDef {
    pub fields: Vec<StructFieldDef>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

impl StructDef {
    pub fn contains_references(&self) -> bool {
        !self.ref_fields.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldDef {
    pub offset: i32,
    pub ty: SourceType,
}

pub fn struct_accessible_from(vm: &VM, struct_id: StructId, namespace_id: NamespaceId) -> bool {
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    accessible_from(vm, xstruct.namespace_id, xstruct.is_pub, namespace_id)
}

pub fn find_methods_in_struct(
    vm: &VM,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let struct_id = object_type.struct_id().unwrap();
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    for &extension_id in &xstruct.extensions {
        if let Some(bindings) =
            extension_matches(vm, object_type.clone(), type_param_defs, extension_id)
        {
            let extension = vm.extensions[extension_id].read();

            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.get(&name) {
                let ext_ty = extension.ty.clone();
                let type_params = object_type.type_params(vm);
                let ext_ty = replace_type_param(vm, ext_ty, &type_params, None);
                return vec![Candidate {
                    object_type: ext_ty,
                    container_type_params: bindings,
                    fct_id,
                }];
            }
        }
    }

    let mut candidates = Vec::new();

    for &impl_id in &xstruct.impls {
        if let Some(bindings) = impl_matches(vm, object_type.clone(), type_param_defs, impl_id) {
            let ximpl = vm.impls[impl_id].read();

            for &method in &ximpl.methods {
                let method = vm.fcts.idx(method);
                let method = method.read();

                if method.name == name && method.is_static == is_static {
                    let impl_ty = ximpl.ty.clone();
                    let type_params = object_type.type_params(vm);
                    let impl_ty = replace_type_param(vm, impl_ty, &type_params, None);
                    candidates.push(Candidate {
                        object_type: impl_ty,
                        container_type_params: bindings.clone(),
                        fct_id: method.id,
                    });
                }
            }
        }
    }

    candidates
}
