use parking_lot::Mutex;

use std::sync::Arc;

use crate::gc::bump::BumpAllocator;
use crate::gc::tlab;
use crate::gc::{
    formatted_size, iterate_strong_roots, iterate_weak_roots, Address, CollectionStats, Collector,
    GcReason, Region,
};
use crate::mem;
use crate::object::{Obj, VtblptrWordKind};
use crate::os::{self, MemoryPermission};
use crate::safepoint;
use crate::threads::DoraThread;
use crate::timer::Timer;
use crate::vm::{Flags, VM};

pub struct CopyCollector {
    total: Region,
    separator: Address,

    alloc: BumpAllocator,
    stats: Mutex<CollectionStats>,
}

impl CopyCollector {
    pub fn new(args: &Flags) -> CopyCollector {
        let alignment = 2 * os::page_size();
        let heap_size = mem::align_usize(args.max_heap_size(), alignment);
        let heap_start = os::commit(heap_size, false);

        if heap_start.is_null() {
            panic!("could not allocate semi space of size {} bytes", heap_size);
        }

        let heap = heap_start.region_start(heap_size);

        let semi_size = heap_size / 2;
        let separator = heap_start.offset(semi_size);

        if args.gc_verbose {
            println!("GC: {}; semi size: {}", heap, formatted_size(semi_size),);
        }

        CopyCollector {
            total: heap,
            separator,
            alloc: BumpAllocator::new(heap_start, separator),
            stats: Mutex::new(CollectionStats::new()),
        }
    }
}

impl Collector for CopyCollector {
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

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
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
        let mut timer = Timer::new(vm.flags.gc_stats);

        safepoint::stop_the_world(vm, |threads| {
            tlab::make_iterable_all(vm, threads);
            self.copy_collect(vm, threads, reason);
        });

        if vm.flags.gc_stats {
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

impl Drop for CopyCollector {
    fn drop(&mut self) {
        os::free(self.total.start, self.total.size());
    }
}

impl CopyCollector {
    fn copy_collect(&self, vm: &VM, threads: &[Arc<DoraThread>], reason: GcReason) {
        let timer = Timer::new(vm.flags.gc_verbose);

        // enable writing into to-space again (for debug builds)
        if cfg!(debug_assertions) {
            let to_space = self.to_space();
            os::protect(to_space.start, to_space.size(), MemoryPermission::ReadWrite);
        }

        // empty to-space
        let to_space = self.to_space();
        let from_space = self.from_space();

        // determine size of heap before collection
        let old_size = self.alloc.top().offset_from(from_space.start);

        let mut top = to_space.start;
        let mut scan = top;

        iterate_strong_roots(vm, threads, |root| {
            let root_ptr = root.get();

            if from_space.contains(root_ptr) {
                root.set(self.copy(root_ptr, &mut top));
            }
        });

        while scan < top {
            let object: &Obj = scan.to_obj();

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if from_space.contains(field_ptr) {
                    field.set(self.copy(field_ptr, &mut top));
                }
            });

            scan = scan.offset(object.size());
        }

        self.iterate_weak_roots(vm);

        // disable access in current from-space
        // makes sure that no pointer into from-space is left (in debug-builds)
        if cfg!(debug_assertions) {
            os::protect(from_space.start, from_space.size(), MemoryPermission::None);
        }

        self.alloc.reset(top, to_space.end);

        timer.stop_with(|time_pause| {
            let new_size = top.offset_from(to_space.start);
            let garbage = old_size - new_size;
            let garbage_ratio = if old_size == 0 {
                0f64
            } else {
                (garbage as f64 / old_size as f64) * 100f64
            };

            println!(
                "Copy GC: {:.1} ms, {}->{} size, {}/{:.0}% garbage, ({})",
                time_pause,
                formatted_size(old_size),
                formatted_size(new_size),
                formatted_size(garbage),
                garbage_ratio,
                reason
            );
        });
    }

    fn iterate_weak_roots(&self, vm: &VM) {
        iterate_weak_roots(vm, |current_address| {
            debug_assert!(self.from_space().contains(current_address));
            let obj = current_address.to_obj();

            if let VtblptrWordKind::Fwdptr(new_address) = obj.header().vtblptr() {
                debug_assert!(self.to_space().contains(new_address));
                Some(new_address)
            } else {
                None
            }
        })
    }

    fn copy(&self, obj_addr: Address, top: &mut Address) -> Address {
        let obj = obj_addr.to_obj();

        if let VtblptrWordKind::Fwdptr(fwd) = obj.header().vtblptr() {
            return fwd;
        }

        let addr = *top;
        let obj_size = obj.size();

        obj.copy_to(addr, obj_size);
        *top = top.offset(obj_size);

        obj.header().install_fwdptr(addr);

        addr
    }

    pub fn from_space(&self) -> Region {
        if self.alloc.limit() == self.separator {
            Region::new(self.total.start, self.separator)
        } else {
            Region::new(self.separator, self.total.end)
        }
    }

    pub fn to_space(&self) -> Region {
        if self.alloc.limit() == self.separator {
            Region::new(self.separator, self.total.end)
        } else {
            Region::new(self.total.start, self.separator)
        }
    }
}
