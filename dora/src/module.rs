use parking_lot::RwLock;
use std::sync::Arc;

use crate::class::ClassId;
use crate::field::{Field, FieldDef};
use crate::size::InstanceSize;
use crate::ty::BuiltinType;
use crate::utils::GrowableVec;
use crate::vm::{FctId, FileId, TraitId, VM};
use crate::vtable::VTableBox;

use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

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
    pub file: FileId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ClassId>,
    pub internal: bool,
    pub internal_resolved: bool,
    pub has_constructor: bool,

    pub constructor: Option<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub virtual_fcts: Vec<FctId>,

    pub traits: Vec<TraitId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ModuleDefId(usize);

impl ModuleDefId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ModuleDefId {
    fn from(data: usize) -> ModuleDefId {
        ModuleDefId(data)
    }
}

impl GrowableVec<RwLock<ModuleDef>> {
    pub fn idx(&self, index: ModuleDefId) -> Arc<RwLock<ModuleDef>> {
        self.idx_usize(index.0)
    }
}

#[derive(Debug)]
pub struct ModuleDef {
    pub id: ModuleDefId,
    pub mod_id: Option<ModuleId>,
    pub parent_id: Option<ModuleDefId>,
    pub fields: Vec<FieldDef>,
    pub size: InstanceSize,
    pub ref_fields: Vec<i32>,
    pub vtable: Option<VTableBox>,
}

impl ModuleDef {
    pub fn name(&self, vm: &VM) -> String {
        if let Some(mod_id) = self.mod_id {
            let modu = vm.modules.idx(mod_id);
            let modu = modu.read();
            let name = vm.interner.str(modu.name);

            format!("{}", name)
        } else {
            "<Unknown>".into()
        }
    }
}
