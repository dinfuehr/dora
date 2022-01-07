use parking_lot::RwLock;

use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::{SourceType, SourceTypeArray};
use crate::utils::GrowableVec;
use crate::vm::{
    extension_matches, impl_matches, namespace_path, Candidate, ClassInstanceId, ExtensionId,
    FileId, ImplId, NamespaceId, TypeParam, TypeParamDefinition, TypeParamId, VM,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDefinitionId(u32);

impl EnumDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for EnumDefinitionId {
    fn from(data: usize) -> EnumDefinitionId {
        EnumDefinitionId(data.try_into().unwrap())
    }
}

impl Index<EnumDefinitionId> for Vec<RwLock<EnumDefinition>> {
    type Output = RwLock<EnumDefinition>;

    fn index(&self, index: EnumDefinitionId) -> &RwLock<EnumDefinition> {
        &self[index.0 as usize]
    }
}

#[derive(Debug)]
pub struct EnumDefinition {
    pub id: EnumDefinitionId,
    pub file_id: FileId,
    pub namespace_id: NamespaceId,
    pub ast: Arc<ast::Enum>,
    pub pos: Position,
    pub name: Name,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub type_params2: TypeParamDefinition,
    pub variants: Vec<EnumVariant>,
    pub name_to_value: HashMap<Name, u32>,
    pub impls: Vec<ImplId>,
    pub extensions: Vec<ExtensionId>,
    pub specializations: RwLock<HashMap<SourceTypeArray, EnumInstanceId>>,
    pub simple_enumeration: bool,
}

impl EnumDefinition {
    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    pub fn name_with_params(&self, vm: &VM, type_list: &SourceTypeArray) -> String {
        let name = vm.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name_enum(vm, self))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub id: usize,
    pub name: Name,
    pub types: Vec<SourceType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumInstanceId(u32);

impl From<usize> for EnumInstanceId {
    fn from(data: usize) -> EnumInstanceId {
        EnumInstanceId(data as u32)
    }
}

impl GrowableVec<EnumInstance> {
    pub fn idx(&self, index: EnumInstanceId) -> Arc<EnumInstance> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Debug)]
pub struct EnumInstance {
    pub id: EnumInstanceId,
    pub enum_id: EnumDefinitionId,
    pub type_params: SourceTypeArray,
    pub layout: EnumLayout,
    pub variants: RwLock<Vec<Option<ClassInstanceId>>>,
}

impl EnumInstance {
    pub fn field_id(&self, xenum: &EnumDefinition, variant_id: usize, element: u32) -> u32 {
        let variant = &xenum.variants[variant_id];
        let mut units = 0;

        for ty in &variant.types[0..element as usize] {
            if ty.is_unit() {
                units += 1;
            }
        }

        1 + element - units
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EnumLayout {
    Int,
    Ptr,
    Tagged,
}

#[derive(Debug)]
pub struct EnumDefVariant {
    pub types: Vec<SourceType>,
}

pub fn find_methods_in_enum(
    vm: &VM,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    type_param_defs2: Option<&TypeParamDefinition>,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let enum_id = object_type.enum_id().unwrap();
    let xenum = vm.enums[enum_id].read();

    for &extension_id in &xenum.extensions {
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

    for &impl_id in &xenum.impls {
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
