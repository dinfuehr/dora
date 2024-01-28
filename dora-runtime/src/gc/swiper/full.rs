use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;

use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::{walk_region, Swiper, PAGE_SIZE};
use crate::gc::swiper::{BasePage, LargeSpace, OldGen, RegularPage, YoungGen};
use crate::gc::{iterate_weak_roots, Slot};
use crate::gc::{Address, GcReason, Region};
use crate::threads::DoraThread;
use crate::vm::VM;

pub struct FullCollector<'a> {
    vm: &'a VM,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    threads: &'a [Arc<DoraThread>],
    swiper: &'a Swiper,

    pages: Vec<RegularPage>,
    metaspace_start: Address,
    live_bytes: Vec<AtomicUsize>,

    reason: GcReason,

    min_heap_size: usize,
    max_heap_size: usize,

    phases: FullCollectorPhases,
}

impl<'a> FullCollector<'a> {
    pub fn new(
        vm: &'a VM,
        swiper: &'a Swiper,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        rootset: &'a [Slot],
        threads: &'a [Arc<DoraThread>],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> FullCollector<'a> {
        FullCollector {
            vm,
            swiper,
            young,
            old,
            pages: Vec::new(),
            large_space,
            rootset,
            threads,
            metaspace_start: vm.meta_space_start(),
            live_bytes: Vec::new(),

            reason,

            min_heap_size,
            max_heap_size,

            phases: FullCollectorPhases::new(),
        }
    }

    pub fn phases(&self) -> FullCollectorPhases {
        self.phases.clone()
    }

    pub fn collect(&mut self) {
        self.phases.complete = measure(self.vm, || {
            self.swiper.sweeper.sweep_in_allocation_to_end(self.vm);
        });

        self.phases.marking = measure(self.vm, || {
            self.mark_live();
        });

        if self.vm.flags.gc_verify {
            verify_marking(self.vm, self.young, self.old, self.large_space);
        }

        self.old.promote_pages(self.vm, self.young);

        self.phases.sweep = measure(self.vm, || {
            self.sweep();
        });

        self.young.reset_after_full_gc(self.vm);

        self.swiper.remset.write().clear();
    }

    fn mark_live(&mut self) {
        self.live_bytes = marking(self.vm, self.swiper, self.rootset);

        iterate_weak_roots(self.vm, |object_address| {
            let obj = object_address.to_obj();

            if obj.header().is_marked() {
                Some(object_address)
            } else {
                None
            }
        });
    }

    fn sweep(&mut self) {
        self.old.clear_freelist();

        let mut pages_to_sweep = Vec::new();
        let heap_start = self.swiper.heap.start_address();

        for page in self.old.pages() {
            let page_id = page.address().offset_from(heap_start) / PAGE_SIZE;
            let live_bytes = self.live_bytes[page_id].load(Ordering::Relaxed);

            if live_bytes > 0 {
                pages_to_sweep.push(page);
            } else {
                self.old.free_page(self.vm, page);
            }
        }

        self.swiper.heap.merge_free_regions();
        self.swiper.sweeper.start(pages_to_sweep, self.vm);

        if self.vm.flags.gc_verify {
            self.swiper.sweeper.join();
        }

        self.large_space.remove_pages(&self.swiper.heap, |page| {
            let object = page.object_address().to_obj();

            if object.header().is_marked() {
                assert!(!object.header().is_remembered());

                // unmark object for next collection
                object.header().clear_mark();

                // keep object
                false
            } else {
                // Drop  unmarked large object.
                true
            }
        });
    }
}

pub fn marking(vm: &VM, swiper: &Swiper, rootset: &[Slot]) -> Vec<AtomicUsize> {
    let mut marking_stack: Vec<Address> = Vec::new();
    let meta_space_start = vm.meta_space_start();
    let pages = swiper.heap.pages();
    let heap_start = swiper.heap.start_address();
    let mut live_bytes: Vec<AtomicUsize> = Vec::with_capacity(pages);

    for _ in 0..pages {
        live_bytes.push(AtomicUsize::new(0));
    }

    for root in rootset {
        let object = root.get();

        if object.is_null() {
            continue;
        }

        let root_obj = object.to_obj();

        if root_obj.header().try_mark() {
            marking_stack.push(object);
        }
    }

    while let Some(address) = marking_stack.pop() {
        debug_assert!(!BasePage::from_address(address).is_readonly());
        let object = address.to_obj();

        let size = object.size(meta_space_start);

        let page_id = address.offset_from(heap_start) / PAGE_SIZE;
        live_bytes[page_id].fetch_add(size, Ordering::Relaxed);

        object.visit_reference_fields(meta_space_start, |field| {
            let referenced = field.get();

            if referenced.is_null() {
                return;
            }

            let field_obj = referenced.to_obj();

            if field_obj.header().try_mark() {
                marking_stack.push(referenced);
            }
        });
    }

    live_bytes
}

pub fn verify_marking(vm: &VM, young: &YoungGen, old: &OldGen, large: &LargeSpace) {
    for page in old.pages() {
        verify_marking_region(vm, page.object_area());
    }

    for page in young.to_pages() {
        verify_marking_region(vm, page.object_area());
    }

    large.iterate_pages(|page| {
        verify_marking_object(vm, page.object_address());
    });
}

fn verify_marking_region(vm: &VM, region: Region) {
    walk_region(vm, region, |_obj, obj_address, _size| {
        verify_marking_object(vm, obj_address);
    });
}

fn verify_marking_object(vm: &VM, obj_address: Address) {
    let obj = obj_address.to_obj();

    if obj.header().is_marked() {
        obj.visit_reference_fields(vm.meta_space_start(), |field| {
            let object_addr = field.get();

            if object_addr.is_null() {
                return;
            }

            assert!(object_addr.to_obj().header().is_marked());
        });
    }
}

fn measure<F>(vm: &VM, fct: F) -> f32
where
    F: FnOnce(),
{
    if vm.flags.gc_stats {
        let start = Instant::now();
        fct();
        let duration = start.elapsed();
        duration.as_secs_f32() * 1000f32
    } else {
        fct();
        0.0f32
    }
}
