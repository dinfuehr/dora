use parking_lot::Mutex;

use std::sync::Arc;

use crate::gc::bump::BumpAllocator;
use crate::gc::tlab;
use crate::gc::{
    Address, CollectionStats, Collector, GcReason, Region, Space, default_readonly_space_config,
    formatted_size, iterate_strong_roots, iterate_weak_roots,
};
use crate::mem;
use crate::mirror::{Object, VtblptrWordKind};
use crate::os::{self, MemoryPermission};
use crate::threads::DoraThread;
use crate::timer::Timer;
use crate::vm::{VM, VmFlags};

pub struct CopyCollector {
    total: Region,
    separator: Address,

    alloc: BumpAllocator,
    stats: Mutex<CollectionStats>,
    readonly: Space,
}

impl CopyCollector {
    pub fn new(args: &VmFlags) -> CopyCollector {
        let alignment = 2 * os::page_size();
        let heap_size = mem::align_usize_up(args.max_heap_size(), alignment);
        let heap_start = os::commit(heap_size, false);

        if heap_start.is_null() {
            panic!("could not allocate semi space of size {} bytes", heap_size);
        }

        let heap = heap_start.region_start(heap_size);

        let semi_size = heap_size / 2;
        let separator = heap_start.offset(semi_size);

        let readonly_space = Space::new(default_readonly_space_config(args), "perm");

        CopyCollector {
            total: heap,
            separator,
            alloc: BumpAllocator::new(heap_start, separator),
            stats: Mutex::new(CollectionStats::new()),
            readonly: readonly_space,
        }
    }
}

impl Collector for CopyCollector {
    fn alloc_tlab_area(&self, _vm: &VM, size: usize) -> Option<Region> {
        self.alloc
            .bump_alloc(size)
            .map(|address| address.region_start(size))
    }

    fn alloc_object(&self, _vm: &VM, size: usize) -> Option<Address> {
        self.alloc.bump_alloc(size)
    }

    fn alloc_readonly(&self, _vm: &VM, size: usize) -> Address {
        self.readonly.alloc(size)
    }

    fn collect_garbage(
        &self,
        vm: &VM,
        threads: &[Arc<DoraThread>],
        reason: GcReason,
        _size: usize,
    ) {
        let mut timer = Timer::new(vm.flags.gc_stats);

        tlab::make_iterable_all(vm, threads);
        self.copy_collect(vm, threads, reason);

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
                root.set(self.copy(vm, root_ptr, &mut top));
            } else if root_ptr.is_non_null() {
                assert!(self.readonly.contains(root_ptr));
            }
        });

        while scan < top {
            let object: &Object = scan.to_obj();

            object.visit_reference_fields(vm.meta_space_start(), |field| {
                let field_ptr = field.get();

                if from_space.contains(field_ptr) {
                    field.set(self.copy(vm, field_ptr, &mut top));
                } else if field_ptr.is_non_null() {
                    assert!(self.readonly.contains(field_ptr));
                }
            });

            scan = scan.offset(object.size(vm.meta_space_start()));
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

            if let VtblptrWordKind::Fwdptr(new_address) =
                obj.header().vtblptr_or_fwdptr(vm.meta_space_start())
            {
                debug_assert!(self.to_space().contains(new_address));
                Some(new_address)
            } else {
                None
            }
        })
    }

    fn copy(&self, vm: &VM, obj_addr: Address, top: &mut Address) -> Address {
        let obj = obj_addr.to_obj();

        if let VtblptrWordKind::Fwdptr(fwd) = obj.header().vtblptr_or_fwdptr(vm.meta_space_start())
        {
            return fwd;
        }

        let addr = *top;
        let obj_size = obj.size(vm.meta_space_start());

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
