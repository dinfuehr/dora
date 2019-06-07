use crate::ctxt::VM;
use crate::driver::cmd::Args;
use crate::gc::bump::BumpAllocator;
use crate::gc::{arena, Address, Collector, GcReason, Region};

pub struct ZeroCollector {
    start: Address,
    end: Address,
    alloc: BumpAllocator,
}

impl ZeroCollector {
    pub fn new(args: &Args) -> ZeroCollector {
        let heap_size: usize = args.max_heap_size();

        let start = arena::reserve(heap_size);
        let end = start.offset(heap_size);

        arena::commit(start, heap_size, false);

        ZeroCollector {
            start: start,
            end: end,
            alloc: BumpAllocator::new(start, end),
        }
    }
}

impl Collector for ZeroCollector {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, _ctxt: &VM, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        }
    }

    fn alloc(&self, _ctxt: &VM, size: usize, _array_ref: bool) -> Address {
        self.alloc.bump_alloc(size)
    }

    fn collect(&self, _: &VM, _: GcReason) {
        // do nothing
    }

    fn minor_collect(&self, _: &VM, _: GcReason) {
        // do nothing
    }

    fn dump_summary(&self, runtime: f32) {
        let mutator = runtime;
        let gc = 0.0f32;

        println!("GC stats: total={:.1}", runtime);
        println!("GC stats: mutator={:.1}", mutator);
        println!("GC stats: collection={:.1}", gc);

        println!(
            "GC summary: 0ms collection (0), {:.1}ms mutator, {:.1}ms total (100% mutator, 0% GC)",
            mutator, runtime,
        );
    }
}
