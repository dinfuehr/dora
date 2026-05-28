use crate::gc::{Address, Region};
use crate::os;
use dora_bytecode::GlobalId;

pub const UNINITIALIZED: u8 = 0;
pub const RUNNING: u8 = 1;
pub const INITIALIZED: u8 = 2;

pub struct GlobalVariableMemory {
    region: Region,
    variables: Vec<GlobalVariableLocation>,
    references: Vec<i32>,
    owned: bool,
}

impl GlobalVariableMemory {
    pub fn from_external(
        start: Address,
        end: Address,
        references: Vec<i32>,
    ) -> GlobalVariableMemory {
        let size = end.offset_from(start);
        GlobalVariableMemory {
            region: start.region_start(size),
            variables: Vec::new(),
            references,
            owned: false,
        }
    }

    pub fn address_value(&self, idx: GlobalId) -> Address {
        self.variables[idx.index()].address_value
    }

    pub fn address_init(&self, idx: GlobalId) -> Address {
        self.variables[idx.index()].address_init
    }

    pub fn start(&self) -> Address {
        self.region.start()
    }

    pub fn references(&self) -> &[i32] {
        &self.references
    }
}

impl Drop for GlobalVariableMemory {
    fn drop(&mut self) {
        if self.owned && self.region.start().is_non_null() {
            os::free(self.region.start(), self.region.size());
        }
    }
}

pub struct GlobalVariableLocation {
    address_init: Address,
    address_value: Address,
}
