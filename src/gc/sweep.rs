use parking_lot::Mutex;
use std::mem;

use ctxt::VM;
use driver::cmd::Args;
use gc::marking;
use gc::root::{get_rootset, Slot};
use gc::space::Space;
use gc::tlab;
use gc::{fill_region, formatted_size, Address, CollectionStats, Collector, GcReason, Region, K};
use os;
use safepoint;
use timer::Timer;

pub struct SweepCollector {
    heap: Region,
    alloc: Mutex<SweepAllocator>,
    stats: Mutex<CollectionStats>,
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

        SweepCollector {
            heap: heap,
            alloc: Mutex::new(SweepAllocator::new(heap)),
            stats: Mutex::new(CollectionStats::new()),
        }
    }
}

impl Collector for SweepCollector {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region> {
        let ptr = self.inner_alloc(vm, size);

        if ptr.is_non_null() {
            return Some(ptr.region_start(size));
        }

        self.collect(vm, GcReason::AllocationFailure);

        let ptr = self.inner_alloc(vm, size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn alloc(&self, vm: &VM, size: usize, _array_ref: bool) -> Address {
        let ptr = self.inner_alloc(vm, size);

        if ptr.is_non_null() {
            return ptr;
        }

        self.collect(vm, GcReason::AllocationFailure);
        self.inner_alloc(vm, size)
    }

    fn collect(&self, vm: &VM, reason: GcReason) {
        let mut timer = Timer::new(vm.args.flag_gc_stats);

        safepoint::stop_the_world(vm, |threads| {
            vm.perf_counters.stop();
            tlab::make_iterable_all(vm, threads);
            let rootset = get_rootset(vm, threads);
            self.mark_sweep(vm, &rootset, reason);
            vm.perf_counters.start();
        });

        if vm.args.flag_gc_stats {
            let duration = timer.stop();
            let mut stats = self.stats.lock();
            stats.add(duration);
        }
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.collect(vm, reason);
    }

    fn dump_summary(&self, runtime: f32) {
        let stats = self.stats.lock();
        let (mutator, gc) = stats.percentage(runtime);

        println!("GC stats: total={:.1}", runtime);
        println!("GC stats: mutator={:.1}", stats.mutator(runtime));
        println!("GC stats: collection={:.1}", stats.pause());

        println!("");
        println!("GC stats: collection-count={}", stats.collections());
        println!("GC stats: collection-pauses={}", stats.pauses());

        println!(
            "GC summary: {:.1}ms collection ({}), {:.1}ms mutator, {:.1}ms total ({}% mutator, {}% GC)",
            stats.pause(),
            stats.collections(),
            stats.mutator(runtime),
            runtime,
            mutator,
            gc,
        );
    }
}

impl Drop for SweepCollector {
    fn drop(&mut self) {
        os::munmap(self.heap.start.to_ptr(), self.heap.size());
    }
}

impl SweepCollector {
    fn inner_alloc(&self, vm: &VM, size: usize) -> Address {
        let mut alloc = self.alloc.lock();
        alloc.allocate(vm, size)
    }

    fn mark_sweep(&self, vm: &VM, rootset: &[Slot], reason: GcReason) {
        let start = self.heap.start;
        let top = self.alloc.lock().top;

        let mut collector = MarkSweep {
            vm: vm,
            heap: Region::new(start, top),
            perm_space: &vm.gc.perm_space,

            rootset: rootset,
            reason: reason,
            free_list: FreeList::new(),
        };

        collector.collect();

        let mut alloc = self.alloc.lock();
        alloc.free_list = mem::replace(&mut collector.free_list, FreeList::new());
    }
}

struct MarkSweep<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    perm_space: &'a Space,

    rootset: &'a [Slot],
    reason: GcReason,
    free_list: FreeList,
}

impl<'a, 'ast> MarkSweep<'a, 'ast> {
    fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    fn mark(&mut self) {
        marking::start(self.rootset, self.heap, self.perm_space.total());
    }

    fn sweep(&mut self) {
        let start = self.heap.start;
        let end = self.heap.end;

        let mut scan = start;
        let mut garbage_start = Address::null();

        while scan < end {
            let object = scan.to_mut_obj();

            if object.header().vtblptr().is_null() {
                scan = scan.add_ptr(1);
                continue;
            }

            let object_size = object.size();

            if object.header().is_marked_non_atomic() {
                self.free(garbage_start, scan);
                garbage_start = Address::null();
                object.header_mut().unmark_non_atomic();
            } else if garbage_start.is_non_null() {
                // more garbage, do nothing
            } else {
                // start garbage, last object was live
                garbage_start = scan;
            }

            scan = scan.offset(object_size);
        }

        self.free(garbage_start, end);
    }

    fn free(&mut self, start: Address, end: Address) {
        if start.is_null() {
            return;
        }

        fill_region(self.vm, start, end);

        let size = end.offset_from(start);
        self.free_list.add(start, size);
    }
}

struct SweepAllocator {
    top: Address,
    limit: Address,
    free_list: FreeList,
}

impl SweepAllocator {
    fn new(heap: Region) -> SweepAllocator {
        SweepAllocator {
            top: heap.start,
            limit: heap.end,
            free_list: FreeList::new(),
        }
    }

    fn allocate(&mut self, vm: &VM, size: usize) -> Address {
        let object = self.top;
        let next_top = object.offset(size);

        if next_top <= self.limit {
            self.top = next_top;
            return object;
        }

        let free_space = self.free_list.alloc(size);

        if free_space.is_non_null() {
            let object = free_space.addr();
            let free_size = free_space.size();
            assert!(size <= free_size);

            let free_start = object.offset(size);
            let free_end = object.offset(free_size);
            let new_free_size = free_end.offset_from(free_start);

            fill_region(vm, free_start, free_end);
            self.free_list.add(free_start, new_free_size);
            return object;
        }

        Address::null()
    }
}

pub const SIZE_CLASSES: usize = 6;

pub const SIZE_CLASS_SMALLEST: SizeClass = SizeClass(0);
pub const SIZE_SMALLEST: usize = 16;

pub const SIZE_CLASS_TINY: SizeClass = SizeClass(1);
pub const SIZE_TINY: usize = 32;

pub const SIZE_CLASS_SMALL: SizeClass = SizeClass(2);
pub const SIZE_SMALL: usize = 128;

pub const SIZE_CLASS_MEDIUM: SizeClass = SizeClass(3);
pub const SIZE_MEDIUM: usize = 2 * K;

pub const SIZE_CLASS_LARGE: SizeClass = SizeClass(4);
pub const SIZE_LARGE: usize = 8 * K;

pub const SIZE_CLASS_HUGE: SizeClass = SizeClass(5);
pub const SIZE_HUGE: usize = 32 * K;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SizeClass(usize);

pub const SIZES: [usize; SIZE_CLASSES] = [
    SIZE_SMALLEST,
    SIZE_TINY,
    SIZE_SMALL,
    SIZE_MEDIUM,
    SIZE_LARGE,
    SIZE_HUGE,
];

impl SizeClass {
    fn from_size(size: usize) -> SizeClass {
        assert!(size >= SIZE_SMALLEST);

        if size <= SIZE_SMALLEST {
            SIZE_CLASS_SMALLEST
        } else if size <= SIZE_TINY {
            SIZE_CLASS_TINY
        } else if size <= SIZE_SMALL {
            SIZE_CLASS_SMALL
        } else if size <= SIZE_MEDIUM {
            SIZE_CLASS_MEDIUM
        } else if size <= SIZE_LARGE {
            SIZE_CLASS_LARGE
        } else {
            SIZE_CLASS_HUGE
        }
    }

    fn idx(self) -> usize {
        self.0
    }

    fn size(self) -> usize {
        SIZES[self.0]
    }
}

struct FreeList {
    classes: Vec<FreeListClass>,
}

impl FreeList {
    fn new() -> FreeList {
        let mut classes = Vec::with_capacity(SIZE_CLASSES);

        for _ in 0..SIZE_CLASSES {
            classes.push(FreeListClass::new());
        }

        FreeList { classes: classes }
    }

    fn add(&mut self, addr: Address, size: usize) {
        if size < SIZE_SMALLEST {
            return;
        }

        debug_assert!(size >= SIZE_SMALLEST);
        let szclass = SizeClass::from_size(size);
        self.classes[szclass.idx()].add(FreeSpace(addr));
    }

    fn alloc(&mut self, size: usize) -> FreeSpace {
        let szclass = SizeClass::from_size(size).idx();
        let last = SIZE_CLASS_HUGE.idx();

        for class in szclass..last {
            let result = self.classes[class].first();

            if result.addr().is_non_null() {
                return result;
            }
        }

        self.classes[SIZE_CLASS_HUGE.idx()].find(size)
    }
}

struct FreeListClass(Vec<FreeSpace>);

impl FreeListClass {
    fn new() -> FreeListClass {
        FreeListClass(Vec::new())
    }

    fn add(&mut self, addr: FreeSpace) {
        self.0.push(addr);
    }

    fn first(&mut self) -> FreeSpace {
        if let Some(space) = self.0.pop() {
            space
        } else {
            FreeSpace::null()
        }
    }

    fn find(&mut self, minimum_size: usize) -> FreeSpace {
        let len = self.0.len();
        for idx in 0..len {
            let curr = self.0[idx];
            if curr.size() >= minimum_size {
                self.0.remove(idx);
                return curr;
            }
        }

        FreeSpace::null()
    }
}

#[derive(Copy, Clone)]
struct FreeSpace(Address);

impl FreeSpace {
    #[inline(always)]
    fn null() -> FreeSpace {
        FreeSpace(Address::null())
    }

    #[inline(always)]
    fn is_null(self) -> bool {
        self.addr().is_null()
    }

    #[inline(always)]
    fn is_non_null(self) -> bool {
        self.addr().is_non_null()
    }

    #[inline(always)]
    fn addr(self) -> Address {
        self.0
    }

    #[inline(always)]
    fn size(self) -> usize {
        let obj = self.addr().to_obj();
        obj.size() as usize
    }
}
