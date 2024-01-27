use std::sync::Arc;
use std::time::Instant;

use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::{walk_region, Swiper};
use crate::gc::swiper::{BasePage, LargeSpace, OldGen, RegularPage, YoungGen};
use crate::gc::{iterate_weak_roots, Slot};
use crate::gc::{Address, GcReason, Region};
use crate::threads::DoraThread;
use crate::vm::VM;

use super::get_swiper;

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
        marking(self.vm, self.rootset);

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

        self.swiper.sweeper.reset(self.old.pages());

        let mut threadpool = self.swiper.threadpool.lock();

        threadpool.scoped(|tp| {
            for _ in 0..4 {
                tp.execute(|| {
                    while let Some(page) = self.swiper.sweeper.next_page() {
                        sweep_page(self.vm, page);
                    }
                });
            }
        });

        self.large_space.remove_pages(&self.swiper.heap, |page| {
            let object = page.object_address().to_obj();

            if object.header().is_marked() {
                // unmark object for next collection
                object.header().clear_mark();
                object.header().clear_remembered();

                // keep object
                false
            } else {
                // Drop  unmarked large object.
                true
            }
        });
    }
}

fn sweep_page(vm: &VM, page: RegularPage) -> bool {
    let swiper = get_swiper(vm);
    let old = &swiper.old;
    let (live, free_regions) = sweep_page_for_free_memory(vm, page);

    if live == 0 {
        old.free_page(vm, page);
        swiper.heap.merge_free_regions();
        true
    } else {
        old.add_to_free_list(vm, free_regions);
        false
    }
}

fn sweep_page_for_free_memory(vm: &VM, page: RegularPage) -> (usize, Vec<Region>) {
    let region = page.object_area();
    let mut scan = region.start;
    let mut free_start = region.start;
    let mut live = 0;
    let mut free_regions = Vec::new();
    let metaspace_start = vm.meta_space_start();

    while scan < region.end {
        let object = scan.to_obj();

        if object.is_filler(vm) {
            scan = scan.offset(object.size(metaspace_start));
        } else {
            let object_size = object.size(metaspace_start);
            let object_end = scan.offset(object_size);

            if object.header().is_marked() {
                handle_free_region(&mut free_regions, free_start, scan);
                object.header().clear_mark();
                object.header().clear_remembered();
                free_start = object_end;
                live += object_size;
            }

            scan = object_end
        }
    }

    handle_free_region(&mut free_regions, free_start, scan);
    assert_eq!(scan, region.end);

    (live, free_regions)
}

fn handle_free_region(free_regions: &mut Vec<Region>, start: Address, end: Address) {
    assert!(start <= end);

    if start == end {
        return;
    }

    free_regions.push(Region::new(start, end));
}

pub fn marking(vm: &VM, rootset: &[Slot]) {
    let mut marking_stack: Vec<Address> = Vec::new();
    let meta_space_start = vm.meta_space_start();

    for root in rootset {
        let object = root.get();

        if object.is_null() {
            continue;
        }

        let root_obj = object.to_obj();

        if !root_obj.header().is_marked() {
            marking_stack.push(object);
            root_obj.header().mark();
        }
    }

    while let Some(address) = marking_stack.pop() {
        debug_assert!(!BasePage::from_address(address).is_readonly());
        let object = address.to_obj();

        object.visit_reference_fields(meta_space_start, |field| {
            let referenced = field.get();

            if referenced.is_null() {
                return;
            }

            let field_obj = referenced.to_obj();

            if !field_obj.header().is_marked() {
                marking_stack.push(referenced);
                field_obj.header().mark();
            }
        });
    }
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
