use parking_lot::Mutex;

use crate::gc::freelist::FreeList;
use crate::gc::marking;
use crate::gc::root::{determine_strong_roots, Slot};
use crate::gc::tlab;
use crate::gc::{
    default_readonly_space_config, formatted_size, iterate_weak_roots, setup_free_space, Address,
    CollectionStats, Collector, GcReason, Region, Space,
};
use crate::os;
use crate::timer::Timer;
use crate::vm::{VmFlags, VM};

pub struct SweepCollector {
    heap: Region,
    alloc: Mutex<SweepAllocator>,
    stats: Mutex<CollectionStats>,
    readonly: Space,
}

impl SweepCollector {
    pub fn new(args: &VmFlags) -> SweepCollector {
        let heap_size = args.max_heap_size();
        let heap_start = os::commit(heap_size, false);

        if heap_start.is_null() {
            panic!("could not allocate heap of size {} bytes", heap_size);
        }

        let heap_end = heap_start.offset(heap_size);
        let heap = Region::new(heap_start, heap_end);

        if args.gc_verbose {
            println!("GC: {} {}", heap, formatted_size(heap_size));
        }

        let readonly_space = Space::new(default_readonly_space_config(args), "perm");

        SweepCollector {
            heap,
            alloc: Mutex::new(SweepAllocator::new(heap)),
            stats: Mutex::new(CollectionStats::new()),
            readonly: readonly_space,
        }
    }
}

impl Collector for SweepCollector {
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region> {
        self.inner_alloc(vm, size)
            .map(|address| address.region_start(size))
    }

    fn alloc_object(&self, vm: &VM, size: usize) -> Option<Address> {
        self.inner_alloc(vm, size)
    }

    fn alloc_readonly(&self, _vm: &VM, size: usize) -> Address {
        self.readonly.alloc(size)
    }

    fn collect_garbage(
        &self,
        vm: &VM,
        threads: &[std::sync::Arc<crate::threads::DoraThread>],
        reason: GcReason,
        _size: usize,
    ) {
        let mut timer = Timer::new(vm.flags.gc_stats);

        tlab::make_iterable_all(vm, threads);
        let rootset = determine_strong_roots(vm, threads);
        self.mark_sweep(vm, &rootset, reason);

        if vm.flags.gc_stats {
            let duration = timer.stop();
            let mut stats = self.stats.lock();
            stats.add(duration);
        }
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
        os::free(self.heap.start, self.heap.size());
    }
}

impl SweepCollector {
    fn inner_alloc(&self, vm: &VM, size: usize) -> Option<Address> {
        let mut alloc = self.alloc.lock();
        alloc.allocate(vm, size)
    }

    fn mark_sweep(&self, vm: &VM, rootset: &[Slot], reason: GcReason) {
        let start = self.heap.start;
        let top = self.alloc.lock().top;

        let mut collector = MarkSweep {
            vm,
            heap: Region::new(start, top),
            readonly_space: &self.readonly,

            rootset,
            reason,
            free_list: FreeList::new(),
        };

        collector.collect(vm);

        let mut alloc = self.alloc.lock();
        alloc.free_list = collector.free_list;
    }
}

struct MarkSweep<'a> {
    vm: &'a VM,
    heap: Region,
    readonly_space: &'a Space,

    rootset: &'a [Slot],
    reason: GcReason,
    free_list: FreeList,
}

impl<'a> MarkSweep<'a> {
    fn collect(&mut self, vm: &VM) {
        self.mark();
        self.iterate_weak_refs();

        self.sweep(vm);
    }

    fn mark(&mut self) {
        marking::start(
            self.vm,
            self.rootset,
            self.heap,
            self.readonly_space.total(),
        );
    }

    fn iterate_weak_refs(&mut self) {
        iterate_weak_roots(self.vm, |current_address| {
            let obj = current_address.to_obj();

            if obj.header().is_marked() {
                Some(current_address)
            } else {
                None
            }
        });
    }

    fn sweep(&mut self, vm: &VM) {
        let start = self.heap.start;
        let end = self.heap.end;
        let meta_space_start = vm.meta_space_start();

        let mut scan = start;
        let mut garbage_start = Address::null();

        while scan < end {
            let object = scan.to_obj();
            let object_size = object.size(meta_space_start);

            if object.header().is_marked() {
                self.add_freelist(garbage_start, scan);
                garbage_start = Address::null();
                object.header().clear_mark();
            } else if garbage_start.is_non_null() {
                // more garbage, do nothing
            } else {
                // start garbage, last object was live
                garbage_start = scan;
            }

            scan = scan.offset(object_size);
        }

        assert!(scan == end);
        self.add_freelist(garbage_start, end);
    }

    fn add_freelist(&mut self, start: Address, end: Address) {
        if start.is_null() {
            return;
        }

        let size = end.offset_from(start);
        self.free_list.add(self.vm, start, size);
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

    fn allocate(&mut self, vm: &VM, size: usize) -> Option<Address> {
        let object = self.top;
        let next_top = object.offset(size);

        if next_top <= self.limit {
            self.top = next_top;
            return Some(object);
        }

        let free_space = self.free_list.alloc(vm, size);

        if free_space.is_non_null() {
            let object = free_space.addr();
            let free_size = free_space.size(vm.meta_space_start());
            assert!(size <= free_size);

            let free_start = object.offset(size);
            let free_end = object.offset(free_size);
            let new_free_size = free_end.offset_from(free_start);

            setup_free_space(vm, free_start, free_end, Address::null());
            self.free_list.add(vm, free_start, new_free_size);
            return Some(object);
        }

        None
    }
}
