use crate::driver::cmd::Args;
use crate::gc::{Address, Collector, GcReason, Region};
use crate::os;
use crate::vm::VM;

pub const REGION_SIZE: usize = 1 << 20;

pub struct RegionCollector {
    total: Region,
}

impl RegionCollector {
    pub fn new(_args: &Args) -> RegionCollector {
        let heap_size = 128 * REGION_SIZE;
        let reservation = os::reserve_align(heap_size, REGION_SIZE, false);

        RegionCollector {
            total: reservation.start.region_start(heap_size),
        }
    }
}

impl Collector for RegionCollector {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, _vm: &VM, _size: usize) -> Option<Region> {
        unimplemented!()
    }

    fn alloc(&self, _vm: &VM, _size: usize, _array_ref: bool) -> Address {
        unimplemented!()
    }

    fn collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn minor_collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn needs_write_barrier(&self) -> bool {
        unimplemented!()
    }

    fn card_table_offset(&self) -> usize {
        unimplemented!()
    }

    fn dump_summary(&self, _runtime: f32) {
        unimplemented!()
    }

    fn verify_ref(&self, _vm: &VM, _reference: Address) {
        unimplemented!()
    }
}
