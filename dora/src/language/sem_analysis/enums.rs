use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use parking_lot::RwLock;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{
    extension_matches, impl_matches, module_path, Candidate, ExtensionDefinitionId,
    ImplDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId, TypeParam,
    TypeParamDefinition, TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::Id;

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

impl Id for EnumDefinition {
    type IdType = EnumDefinitionId;

    fn id_to_usize(id: EnumDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> EnumDefinitionId {
        EnumDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut EnumDefinition, id: EnumDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct EnumDefinition {
    pub id: Option<EnumDefinitionId>,
    pub file_id: SourceFileId,
    pub module_id: ModuleDefinitionId,
    pub ast: Arc<ast::Enum>,
    pub pos: Position,
    pub name: Name,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub type_params2: TypeParamDefinition,
    pub variants: Vec<EnumVariant>,
    pub name_to_value: HashMap<Name, u32>,
    pub impls: Vec<ImplDefinitionId>,
    pub extensions: Vec<ExtensionDefinitionId>,
    pub simple_enumeration: bool,
}

impl EnumDefinition {
    pub fn new(
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        node: &Arc<ast::Enum>,
    ) -> EnumDefinition {
        let mut type_params = Vec::new();

        if let Some(ref ast_type_params) = node.type_params {
            for param in ast_type_params {
                type_params.push(TypeParam::new(param.name));
            }
        }

        EnumDefinition {
            id: None,
            file_id: file_id,
            module_id,
            ast: node.clone(),
            pos: node.pos,
            name: node.name,
            type_params,
            type_params2: TypeParamDefinition::new(),
            is_pub: node.is_pub,
            variants: Vec::new(),
            name_to_value: HashMap::new(),
            impls: Vec::new(),
            extensions: Vec::new(),
            simple_enumeration: false,
        }
    }

    pub fn id(&self) -> EnumDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn name(&self, sa: &SemAnalysis) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn name_with_params(&self, sa: &SemAnalysis, type_list: &SourceTypeArray) -> String {
        let name = sa.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name_enum(sa, self))
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

pub fn find_methods_in_enum(
    sa: &SemAnalysis,
    object_type: SourceType,
    type_param_defs: &[TypeParam],
    type_param_defs2: Option<&TypeParamDefinition>,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let enum_id = object_type.enum_id().unwrap();
    let enum_ = sa.enums.idx(enum_id);
    let enum_ = enum_.read();

    for &extension_id in &enum_.extensions {
        if let Some(bindings) = extension_matches(
            sa,
            object_type.clone(),
            type_param_defs,
            type_param_defs2,
            extension_id,
        ) {
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

    for &impl_id in &enum_.impls {
        if let Some(bindings) = impl_matches(
            sa,
            object_type.clone(),
            type_param_defs,
            type_param_defs2,
            impl_id,
        ) {
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
