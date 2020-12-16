use parking_lot::RwLock;

use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::{find_impl, SourceType, SourceTypeArray};
use crate::vm::{
    extension_matches_ty, FctId, FileId, NamespaceId, TraitId, TypeParam, TypeParamDefinition,
    TypeParamId, VM,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplId(u32);

impl From<u32> for ImplId {
    fn from(data: u32) -> ImplId {
        ImplId(data)
    }
}

#[derive(Debug)]
pub struct ImplData {
    pub id: ImplId,
    pub file_id: FileId,
    pub ast: Arc<ast::Impl>,
    pub namespace_id: NamespaceId,
    pub pos: Position,
    pub type_params: Vec<TypeParam>,
    pub trait_id: Option<TraitId>,
    pub ty: SourceType,
    pub methods: Vec<FctId>,
    pub instance_names: HashMap<Name, FctId>,
    pub static_names: HashMap<Name, FctId>,
    pub impl_for: HashMap<FctId, FctId>,
}

impl ImplData {
    pub fn trait_id(&self) -> TraitId {
        self.trait_id.expect("trait_id not initialized yet.")
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }
}

impl Index<ImplId> for Vec<RwLock<ImplData>> {
    type Output = RwLock<ImplData>;

    fn index(&self, index: ImplId) -> &RwLock<ImplData> {
        &self[index.0 as usize]
    }
}

pub fn impl_matches(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    check_type_param_defs2: Option<&TypeParamDefinition>,
    impl_id: ImplId,
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
    fct_id: FctId,
    trait_id: TraitId,
    object_type: SourceType,
) -> FctId {
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
