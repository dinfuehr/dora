use ctxt::VM;
use driver::cmd::Args;
use gc::{Address, Collector, GcReason, Region};

mod old;

pub struct SweepSwiper;

impl SweepSwiper {
    pub fn new(_args: &Args) -> SweepSwiper {
        SweepSwiper
    }
}

impl Collector for SweepSwiper {
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