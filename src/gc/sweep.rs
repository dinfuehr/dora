use parking_lot::Mutex;

use ctxt::VM;
use driver::cmd::Args;
use gc::marking;
use gc::root::{get_rootset, Slot};
use gc::space::Space;
use gc::tlab;
use gc::{formatted_size, Address, CollectionStats, Collector, GcReason, Region};
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

        let alloc = SweepAllocator {
            top: heap_start,
            limit: heap_end,
        };

        SweepCollector {
            heap: heap,
            alloc: Mutex::new(alloc),
            stats: Mutex::new(CollectionStats::new()),
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
    fn mark_sweep(&self, vm: &VM, rootset: &[Slot], reason: GcReason) {
        let start = self.heap.start;
        let top = self.alloc.lock().top;

        let mut collector = MarkSweep {
            vm: vm,
            heap: Region::new(start, top),
            perm_space: &vm.gc.perm_space,

            rootset: rootset,
            reason: reason,
        };

        collector.collect();
    }
}

struct MarkSweep<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    perm_space: &'a Space,

    rootset: &'a [Slot],
    reason: GcReason,
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

        while scan < end {
            let object = scan.to_mut_obj();

            if object.header().vtblptr().is_null() {
                scan = scan.add_ptr(1);
                continue;
            }

            let object_size = object.size();

            if object.header().is_marked_non_atomic() {
                object.header_mut().unmark_non_atomic();
            } else {
                unimplemented!();
            }

            scan = scan.offset(object_size);
        }
    }
}

struct SweepAllocator {
    top: Address,
    limit: Address,
}
