use parking_lot::Mutex;

use ctxt::VM;
use driver::cmd::Args;
use gc::{formatted_size, Address, Collector, GcReason, Region};
use os;

pub struct SweepCollector {
    heap: Region,
    alloc: Mutex<SweepAllocator>,
}

impl SweepCollector {
    pub fn new(args: &Args) -> SweepCollector {
        let heap_size = args.max_heap_size();
        let ptr = os::mmap(heap_size, os::Writable);

        if ptr.is_null() {
            panic!("could not allocate heap of size {} bytes", heap_size);
        }

        let heap_start = Address::from_ptr(ptr);
        let heap_end = heap_start.offset(heap_size);
        let heap = Region::new(heap_start, heap_end);

        if args.flag_gc_verbose {
            println!("GC: {} {}", heap, formatted_size(heap_size));
        }

        let alloc = SweepAllocator {
            top: heap_start,
            limit: heap_end,
        };

        SweepCollector {
            heap: heap,
            alloc: Mutex::new(alloc),
        }
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

impl Drop for SweepCollector {
    fn drop(&mut self) {
        os::munmap(self.heap.start.to_ptr(), self.heap.size());
    }
}

struct SweepAllocator {
    top: Address,
    limit: Address,
}
