use parking_lot::RwLock;

use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    extension_matches_ty, ClassId, FctId, FileId, NamespaceId, TraitId, TypeParam, TypeParamId, VM,
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
}

impl ImplData {
    pub fn trait_id(&self) -> TraitId {
        self.trait_id.expect("trait_id not initialized yet.")
    }

    pub fn cls_id(&self, vm: &VM) -> ClassId {
        self.ty.cls_id(vm).expect("class_ty not initialized yet.")
    }

    pub fn find_implements(&self, vm: &VM, fct_id: FctId) -> Option<FctId> {
        for &mtd_id in &self.methods {
            let mtd = vm.fcts.idx(mtd_id);
            let mtd = mtd.read();

            if mtd.impl_for == Some(fct_id) {
                return Some(mtd_id);
            }
        }

        None
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
    impl_id: ImplId,
) -> Option<SourceTypeArray> {
    let ximpl = vm.impls[impl_id].read();
    extension_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        ximpl.ty.clone(),
        &ximpl.type_params,
    )
}
