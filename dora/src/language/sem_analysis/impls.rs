use parking_lot::RwLock;

use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{
    extension_matches_ty, FctDefinitionId, NamespaceDefinitionId, TraitDefinitionId, TypeParam,
    TypeParamDefinition, TypeParamId,
};
use crate::language::ty::{find_impl, SourceType, SourceTypeArray};
use crate::utils::Id;
use crate::vm::{FileId, VM};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplDefinitionId(u32);

impl From<u32> for ImplDefinitionId {
    fn from(data: u32) -> ImplDefinitionId {
        ImplDefinitionId(data)
    }
}

impl Id for ImplDefinition {
    type IdType = ImplDefinitionId;

    fn id_to_usize(id: ImplDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ImplDefinitionId {
        ImplDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut ImplDefinition, id: ImplDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ImplDefinition {
    pub id: Option<ImplDefinitionId>,
    pub file_id: FileId,
    pub ast: Arc<ast::Impl>,
    pub namespace_id: NamespaceDefinitionId,
    pub pos: Position,
    pub type_params: Vec<TypeParam>,
    pub trait_id: Option<TraitDefinitionId>,
    pub ty: SourceType,
    pub methods: Vec<FctDefinitionId>,
    pub instance_names: HashMap<Name, FctDefinitionId>,
    pub static_names: HashMap<Name, FctDefinitionId>,
    pub impl_for: HashMap<FctDefinitionId, FctDefinitionId>,
}

impl ImplDefinition {
    pub fn new(
        file_id: FileId,
        namespace_id: NamespaceDefinitionId,
        node: &Arc<ast::Impl>,
    ) -> ImplDefinition {
        let mut type_params = Vec::new();
        if let Some(ref ast_type_params) = node.type_params {
            for param in ast_type_params {
                type_params.push(TypeParam::new(param.name));
            }
        }

        ImplDefinition {
            id: None,
            file_id,
            ast: node.clone(),
            namespace_id,
            type_params,
            pos: node.pos,
            trait_id: None,
            ty: SourceType::Error,
            methods: Vec::new(),
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
            impl_for: HashMap::new(),
        }
    }

    pub fn id(&self) -> ImplDefinitionId {
        self.id.expect("id missing")
    }

    pub fn trait_id(&self) -> TraitDefinitionId {
        self.trait_id.expect("trait_id not initialized yet.")
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }
}

impl Index<ImplDefinitionId> for Vec<RwLock<ImplDefinition>> {
    type Output = RwLock<ImplDefinition>;

    fn index(&self, index: ImplDefinitionId) -> &RwLock<ImplDefinition> {
        &self[index.0 as usize]
    }
}

pub fn impl_matches(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    check_type_param_defs2: Option<&TypeParamDefinition>,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let ximpl = vm.impls[impl_id].read();
    extension_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        check_type_param_defs2,
        ximpl.ty.clone(),
        &ximpl.type_params,
    )
}

pub fn find_trait_impl(
    vm: &VM,
    fct_id: FctDefinitionId,
    trait_id: TraitDefinitionId,
    object_type: SourceType,
) -> FctDefinitionId {
    debug_assert!(!object_type.contains_type_param(vm));
    let impl_id = find_impl(vm, object_type, &[], trait_id)
        .expect("no impl found for generic trait method call");

    let ximpl = vm.impls[impl_id].read();
    assert_eq!(ximpl.trait_id(), trait_id);

    ximpl
        .impl_for
        .get(&fct_id)
        .cloned()
        .expect("no impl method found for generic trait call")
}
