use crate::driver::cmd::Args;
use crate::gc::{Address, Collector, GcReason, Region};
use crate::os::{self, Reservation};
use crate::vm::VM;

pub const PAGE_SIZE_BITS: usize = 20;
pub const PAGE_SIZE: usize = 1 << PAGE_SIZE_BITS;

pub struct PageCollector {
    reservation: Reservation,
    regions: Vec<Page>,
    number_pages: usize,
}

impl PageCollector {
    pub fn new(args: &Args) -> PageCollector {
        let max_heap_size = align_page(args.max_heap_size());
        // let min_heap_size = align_region(args.min_heap_size());

        let reservation = os::reserve_align(max_heap_size, PAGE_SIZE, false);
        let number_pages = max_heap_size / PAGE_SIZE;

        PageCollector {
            reservation,
            regions: Vec::new(),
            number_pages,
        }
    }
}

impl Collector for PageCollector {
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

struct Page {
    // Total memory region for page.
    start: Address,
    limit: Address,

    // Separator between used & free bytes in page.
    top: Address,

    // Current page state.
    state: PageState,

    // Number of live bytes after marking.
    live_bytes: usize,
}

enum PageState {
    FREE,
    USED,
}

/// round the given value up to the nearest multiple of a generation
pub fn align_page(value: usize) -> usize {
    let align = PAGE_SIZE_BITS;
    // we know that region size is power of 2, hence
    // we can use shifts instead of expensive division
    ((value + (1 << align) - 1) >> align) << align
}
