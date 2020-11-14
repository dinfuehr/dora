use parking_lot::RwLock;
use std::sync::Arc;

use crate::gc::Address;
use crate::mem;
use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{accessible_from, namespace_path, FctId, FileId, NamespaceId, VM};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

impl GlobalId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u32> for GlobalId {
    fn from(data: u32) -> GlobalId {
        GlobalId(data)
    }
}

#[derive(Debug)]
pub struct GlobalData {
    pub id: GlobalId,
    pub file_id: FileId,
    pub ast: Arc<ast::Global>,
    pub pos: Position,
    pub namespace_id: NamespaceId,
    pub is_pub: bool,
    pub ty: SourceType,
    pub reassignable: bool,
    pub name: Name,
    pub initializer: Option<FctId>,
    pub address_init: Address,
    pub address_value: Address,
}

impl GlobalData {
    pub fn needs_initialization(&self) -> bool {
        self.initializer.is_some() && !self.is_initialized()
    }

    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    fn is_initialized(&self) -> bool {
        unsafe { *self.address_init.to_ptr::<bool>() }
    }
}

impl GrowableVec<RwLock<GlobalData>> {
    pub fn idx(&self, index: GlobalId) -> Arc<RwLock<GlobalData>> {
        self.idx_usize(index.0 as usize)
    }
}

pub fn init_global_addresses(vm: &VM) {
    let globals = vm.globals.lock();
    let mut size = 0;
    let mut offsets = Vec::with_capacity(globals.len());

    for glob in globals.iter() {
        let glob = glob.read();

        let initialized = size;
        size += SourceType::Bool.size(vm);

        let ty_size = glob.ty.size(vm);
        let ty_align = glob.ty.align(vm);

        let value = mem::align_i32(size, ty_align);
        offsets.push((initialized, value));
        size = value + ty_size;
    }

    let ptr = vm.gc.alloc_perm(size as usize);

    for (ind, glob) in globals.iter().enumerate() {
        let mut glob = glob.write();
        let (initialized, value) = offsets[ind];

        glob.address_init = ptr.offset(initialized as usize);
        glob.address_value = ptr.offset(value as usize);
    }
}

pub fn global_accessible_from(vm: &VM, global_id: GlobalId, namespace_id: NamespaceId) -> bool {
    let global = vm.globals.idx(global_id);
    let global = global.read();

    accessible_from(vm, global.namespace_id, global.is_pub, namespace_id)
}
