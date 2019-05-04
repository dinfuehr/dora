use ctxt::VM;
use driver::cmd::Args;
use gc::{Address, Collector, GcReason, Region};

pub struct SweepCollector;

impl SweepCollector {
    pub fn new(_args: &Args) -> SweepCollector {
        SweepCollector
    }
}

impl Collector for SweepCollector {
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
        unimplemented!();
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.collect(vm, reason);
    }

    fn dump_summary(&self, _runtime: f32) {
        unimplemented!();
    }
}
