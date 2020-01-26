use parking_lot::Mutex;

use crate::driver::cmd::Args;
use crate::gc::bump::BumpAllocator;
use crate::gc::marking;
use crate::gc::root::{get_rootset, Slot};
use crate::gc::space::Space;
use crate::gc::tlab;
use crate::gc::{formatted_size, Address, CollectionStats, Collector, GcReason, Region};
use crate::object::Obj;
use crate::os;
use crate::safepoint;
use crate::timer::Timer;
use crate::vm::VM;

pub struct MarkCompactCollector {
    heap: Region,
    alloc: BumpAllocator,
    stats: Mutex<CollectionStats>,
}

impl MarkCompactCollector {
    pub fn new(args: &Args) -> MarkCompactCollector {
        let heap_size = args.max_heap_size();
        let heap_start = os::commit(heap_size, false);

        if heap_start.is_null() {
            panic!("could not allocate heap of size {} bytes", heap_size);
        }

        let heap_end = heap_start.offset(heap_size);
        let heap = Region::new(heap_start, heap_end);

        if args.flag_gc_verbose {
            println!("GC: {} {}", heap, formatted_size(heap_size));
        }

        MarkCompactCollector {
            heap,
            alloc: BumpAllocator::new(heap_start, heap_end),
            stats: Mutex::new(CollectionStats::new()),
        }
    }
}

impl Collector for MarkCompactCollector {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return Some(ptr.region_start(size));
        }

        self.collect(vm, GcReason::AllocationFailure);

        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        }
    }

    fn alloc(&self, vm: &VM, size: usize, _array_ref: bool) -> Address {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        self.collect(vm, GcReason::AllocationFailure);
        self.alloc.bump_alloc(size)
    }

    fn collect(&self, vm: &VM, reason: GcReason) {
        let mut timer = Timer::new(vm.args.flag_gc_stats);

        safepoint::stop_the_world(vm, |threads| {
            tlab::make_iterable_all(vm, threads);
            let rootset = get_rootset(vm, threads);
            self.mark_compact(vm, &rootset, reason);
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

impl Drop for MarkCompactCollector {
    fn drop(&mut self) {
        os::free(self.heap.start, self.heap.size());
    }
}

impl MarkCompactCollector {
    fn mark_compact(&self, vm: &VM, rootset: &[Slot], reason: GcReason) {
        let mut mark_compact = MarkCompact {
            vm,
            heap: self.heap,
            perm_space: &vm.gc.perm_space,

            init_top: self.alloc.top(),
            top: self.heap.start,

            rootset,
            reason,
        };

        mark_compact.collect();

        self.alloc.reset(mark_compact.top, self.heap.end);
    }
}

struct MarkCompact<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    perm_space: &'a Space,
    init_top: Address,
    top: Address,

    rootset: &'a [Slot],
    reason: GcReason,
}

impl<'a, 'ast> MarkCompact<'a, 'ast> {
    fn collect(&mut self) {
        self.mark_live();
        self.compute_forward();
        self.update_references();
        self.relocate();
    }

    fn mark_live(&mut self) {
        marking::start(self.rootset, self.heap, self.perm_space.total());
    }

    fn compute_forward(&mut self) {
        self.walk_heap(|mc, object, _addr, object_size| {
            if object.header().is_marked_non_atomic() {
                let fwd = mc.allocate(object_size);
                object.header_mut().set_fwdptr_non_atomic(fwd);
            }
        });
    }

    fn allocate(&mut self, object_size: usize) -> Address {
        let addr = self.top;
        let next = self.top.offset(object_size);

        if next <= self.heap.end {
            self.top = next;
            return addr;
        }

        panic!("FAIL: Not enough space for objects.");
    }

    fn update_references(&mut self) {
        self.walk_heap(|mc, object, _addr, _object_size| {
            if object.header().is_marked_non_atomic() {
                object.visit_reference_fields(|field| {
                    mc.forward_reference(field);
                });
            }
        });

        for root in self.rootset {
            self.forward_reference(*root);
        }
    }

    fn forward_reference(&mut self, slot: Slot) {
        let object_addr = slot.get();

        if self.heap.contains(object_addr) {
            debug_assert!(object_addr.to_obj().header().is_marked_non_atomic());
            let fwd_addr = object_addr.to_obj().header().fwdptr_non_atomic();
            debug_assert!(self.heap.contains(fwd_addr));
            slot.set(fwd_addr);
        } else {
            debug_assert!(object_addr.is_null() || self.perm_space.contains(object_addr));
        }
    }

    fn relocate(&mut self) {
        self.walk_heap(|mc, object, address, object_size| {
            if object.header().is_marked_non_atomic() {
                // get new location
                let dest = object.header().fwdptr_non_atomic();
                debug_assert!(mc.heap.contains(dest));

                // determine location after relocated object
                let next_dest = dest.offset(object_size);
                debug_assert!(mc.heap.valid_top(next_dest));

                if address != dest {
                    object.copy_to(dest, object_size);
                }

                // unmark object for next collection
                let dest_obj = dest.to_mut_obj();
                dest_obj.header_mut().unmark_non_atomic();
            }
        });
    }

    fn walk_heap<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut MarkCompact, &mut Obj, Address, usize),
    {
        let start = self.heap.start;
        let end = self.init_top;

        let mut scan = start;

        while scan < end {
            let object = scan.to_mut_obj();

            if object.header().vtblptr().is_null() {
                scan = scan.add_ptr(1);
                continue;
            }

            let object_size = object.size();

            fct(self, object, scan, object_size);

            scan = scan.offset(object_size);
        }
    }
}
