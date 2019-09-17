use parking_lot::RwLock;
use std::sync::Arc;

use crate::field::Field;
use crate::ty::BuiltinType;
use crate::utils::GrowableVec;
use crate::vm::{FctId, TraitId, ImplId, TypeParam};

use dora_parser::lexer::position::Position;
use dora_parser::interner::Name;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ModuleId(usize);

impl ModuleId {
    pub fn max() -> ModuleId {
        ModuleId(usize::max_value())
    }
}

impl From<ModuleId> for usize {
    fn from(data: ModuleId) -> usize {
        data.0
    }
}

impl From<usize> for ModuleId {
    fn from(data: usize) -> ModuleId {
        ModuleId(data)
    }
}

impl GrowableVec<RwLock<Module>> {
    pub fn idx(&self, index: ModuleId) -> Arc<RwLock<Module>> {
        self.idx_usize(index.0)
    }
}

pub static DISPLAY_SIZE: usize = 6;

#[derive(Debug)]
pub struct Module {
    pub id: ModuleId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ModuleId>,
    pub internal: bool,
    pub internal_resolved: bool,
    pub has_constructor: bool,

    pub constructor: Option<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub virtual_fcts: Vec<FctId>,

    pub traits: Vec<TraitId>,
    pub impls: Vec<ImplId>,

    pub type_params: Vec<TypeParam>,
}
