use parking_lot::Mutex;

use ctxt::VM;
use driver::cmd::Args;
use gc::bump::BumpAllocator;
use gc::root::{get_rootset, Slot};
use gc::space::Space;
use gc::tlab;
use gc::{formatted_size, Address, CollectionStats, Collector, GcReason, Region};
use object::Obj;
use os;
use safepoint;
use timer::Timer;

pub struct MarkCompactCollector {
    heap: Region,
    alloc: BumpAllocator,
    stats: Mutex<CollectionStats>,
}

impl MarkCompactCollector {
    pub fn new(args: &Args) -> MarkCompactCollector {
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

        MarkCompactCollector {
            heap: heap,
            alloc: BumpAllocator::new(heap_start, heap_end),
            stats: Mutex::new(CollectionStats::new()),
        }
    }
}

impl Collector for MarkCompactCollector {
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return Some(ptr.region_start(size));
        }

        self.collect(vm, GcReason::AllocationFailure);

        let ptr = self.alloc.bump_alloc(size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn alloc_normal(&self, vm: &VM, size: usize, _array_ref: bool) -> Address {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        self.collect(vm, GcReason::AllocationFailure);
        self.alloc.bump_alloc(size)
    }

    fn alloc_large(&self, vm: &VM, size: usize, array_ref: bool) -> Address {
        self.alloc_normal(vm, size, array_ref)
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

        println!(
            "GC summary: {:.1}ms collection ({}), {:.1}ms runtime ({}% mutator, {}% GC)",
            stats.pause(),
            stats.collections(),
            runtime,
            mutator,
            gc,
        );
    }
}

impl Drop for MarkCompactCollector {
    fn drop(&mut self) {
        os::munmap(self.heap.start.to_ptr(), self.heap.size());
    }
}

impl MarkCompactCollector {
    fn mark_compact(&self, vm: &VM, rootset: &[Slot], reason: GcReason) {
        let mut mark_compact = MarkCompact {
            vm: vm,
            heap: self.heap,
            perm_space: &vm.gc.perm_space,

            init_top: self.alloc.top(),
            top: self.heap.start,

            rootset: rootset,
            reason: reason,
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
        let mut marking_stack: Vec<Address> = Vec::new();

        for root in self.rootset {
            let root_ptr = root.get();

            if self.heap.contains(root_ptr) {
                let root_obj = root_ptr.to_mut_obj();

                if !root_obj.header().is_marked_non_atomic() {
                    marking_stack.push(root_ptr);
                    root_obj.header_mut().mark_non_atomic();
                }
            } else {
                debug_assert!(root_ptr.is_null() || self.perm_space.contains(root_ptr));
            }
        }

        while marking_stack.len() > 0 {
            let object_addr = marking_stack.pop().expect("stack already empty");
            let object = object_addr.to_mut_obj();

            object.visit_reference_fields(|field| {
                let field_addr = field.get();

                if self.heap.contains(field_addr) {
                    let field_obj = field_addr.to_mut_obj();

                    if !field_obj.header().is_marked_non_atomic() {
                        marking_stack.push(field_addr);
                        field_obj.header_mut().mark_non_atomic();
                    }
                } else {
                    debug_assert!(field_addr.is_null() || self.perm_space.contains(field_addr));
                }
            });
        }
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
