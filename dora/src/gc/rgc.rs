use crate::driver::cmd::Args;
use crate::gc::{Address, Collector, GcReason, Region};
use crate::os::{self, Reservation};
use crate::vm::VM;

pub const PAGE_SIZE_BITS: usize = 20;
pub const PAGE_SIZE: usize = 1 << PAGE_SIZE_BITS;

pub struct RegionCollector {
    reservation: Reservation,
    regions: Vec<Page>,
}

impl RegionCollector {
    pub fn new(args: &Args) -> RegionCollector {
        let max_heap_size = align_page(args.max_heap_size());
        // let min_heap_size = align_region(args.min_heap_size());

        let reservation = os::reserve_align(max_heap_size, PAGE_SIZE, false);

        RegionCollector {
            reservation,
            regions: Vec::new(),
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

struct Page;

/// round the given value up to the nearest multiple of a generation
pub fn align_page(value: usize) -> usize {
    let align = PAGE_SIZE_BITS;
    // we know that region size is power of 2, hence
    // we can use shifts instead of expensive division
    ((value + (1 << align) - 1) >> align) << align
}
