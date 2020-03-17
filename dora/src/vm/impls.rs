use parking_lot::RwLock;

use std::ops::Index;

use dora_parser::lexer::position::Position;

use crate::ty::BuiltinType;
use crate::vm::{ClassId, FctId, FileId, TraitId, VM};

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
    pub file: FileId,
    pub pos: Position,
    pub trait_id: Option<TraitId>,
    pub class_ty: BuiltinType,
    pub methods: Vec<FctId>,
}

impl ImplData {
    pub fn trait_id(&self) -> TraitId {
        self.trait_id.expect("trait_id not initialized yet.")
    }

    pub fn cls_id(&self, vm: &VM) -> ClassId {
        self.class_ty
            .cls_id(vm)
            .expect("class_ty not initialized yet.")
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
}

impl Index<ImplId> for Vec<RwLock<ImplData>> {
    type Output = RwLock<ImplData>;

    fn index(&self, index: ImplId) -> &RwLock<ImplData> {
        &self[index.0 as usize]
    }
}
