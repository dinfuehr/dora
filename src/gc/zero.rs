use ctxt::SemContext;
use driver::cmd::Args;
use gc::{arena, Address, Collector, Region};
use gc::bump::BumpAllocator;

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
    fn alloc_tlab_area(&self, _ctxt: &SemContext, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        }
    }

    fn alloc_normal(&self, _ctxt: &SemContext, size: usize, _array_ref: bool) -> Address {
        self.alloc.bump_alloc(size)
    }

    fn alloc_large(&self, _ctxt: &SemContext, size: usize, _array_ref: bool) -> Address {
        self.alloc.bump_alloc(size)
    }

    fn collect(&self, _: &SemContext) {
        // do nothing
    }

    fn minor_collect(&self, _: &SemContext) {
        // do nothing
    }
}
