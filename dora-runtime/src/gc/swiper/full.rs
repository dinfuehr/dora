use std::sync::Arc;
use std::time::Instant;

use parking_lot::MutexGuard;

use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::old::OldGenProtected;
use crate::gc::swiper::{walk_region, ReadOnlySpace, Swiper};
use crate::gc::swiper::{BasePage, LargeSpace, OldGen, RegularPage, YoungGen};
use crate::gc::{fill_region_with, iterate_strong_roots, iterate_weak_roots, Slot};
use crate::gc::{Address, GcReason, Region};
use crate::object::{Obj, VtblptrWordKind};
use crate::stdlib;
use crate::threads::DoraThread;
use crate::vm::{Trap, VM};

pub struct FullCollector<'a> {
    vm: &'a VM,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    threads: &'a [Arc<DoraThread>],
    readonly_space: &'a ReadOnlySpace,
    swiper: &'a Swiper,

    top: Address,
    current_limit: Address,
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
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        readonly_space: &'a ReadOnlySpace,
        rootset: &'a [Slot],
        threads: &'a [Arc<DoraThread>],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> FullCollector<'a> {
        let old_total = old.total();

        FullCollector {
            vm,
            swiper,
            heap,
            young,
            old,
            old_protected: old.protected(),
            pages: Vec::new(),
            large_space,
            rootset,
            threads,
            readonly_space,
            metaspace_start: vm.meta_space_start(),

            top: old_total.start(),
            current_limit: old_total.start(),

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
            verify_marking(
                self.vm,
                self.young,
                &*self.old_protected,
                self.large_space,
                self.heap,
            );
        }

        self.phases.sweep = measure(self.vm, || {
            self.sweep();
        });

        self.phases.evacuate = measure(self.vm, || {
            self.evacuate();
        });

        self.phases.update_refs = measure(self.vm, || {
            self.update_references();
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

    fn update_references(&mut self) {
        let meta_space_start = self.vm.meta_space_start();

        for page in self.old_protected.pages() {
            walk_region(self.vm, page.object_area(), |object, _addr, _size| {
                object.visit_reference_fields(meta_space_start, |field| {
                    self.forward_reference(field);
                });
            });
        }

        iterate_strong_roots(self.vm, self.threads, |slot| {
            self.forward_reference(slot);
        });

        iterate_weak_roots(self.vm, |object_address| {
            self.forward_object(object_address).or(Some(object_address))
        });

        self.large_space.iterate_pages(|page| {
            let object = page.object_address().to_obj();

            object.visit_reference_fields(meta_space_start, |field| {
                self.forward_reference(field);
            });
        });
    }

    fn sweep(&mut self) {
        self.old_protected.clear_freelist();

        for page in self.old_protected.pages() {
            let (live, free_regions) = self.sweep_page(page);

            if live == 0 {
                self.old_protected.free_page(page);
            } else {
                for free_region in free_regions {
                    fill_region_with(self.vm, free_region.start, free_region.end, true);

                    self.old_protected.add_to_freelist(
                        self.vm,
                        free_region.start,
                        free_region.size(),
                    );
                }
            }
        }

        self.large_space.remove_pages(|page| {
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

    fn sweep_page(&mut self, page: RegularPage) -> (usize, Vec<Region>) {
        let region = page.object_area();
        let mut scan = region.start;
        let mut free_start = region.start;
        let mut live = 0;
        let mut free_regions = Vec::new();

        while scan < region.end {
            let object = scan.to_obj();

            if object.is_filler(self.vm) {
                scan = scan.offset(object.size(self.metaspace_start));
            } else {
                let object_size = object.size(self.metaspace_start);
                let object_end = scan.offset(object_size);

                if object.header().is_marked() {
                    self.handle_free_region(&mut free_regions, free_start, scan);
                    object.header().clear_mark();
                    object.header().clear_remembered();
                    free_start = object_end;
                    live += object_size;
                }

                scan = object_end
            }
        }

        self.handle_free_region(&mut free_regions, free_start, scan);
        assert_eq!(scan, region.end);

        (live, free_regions)
    }

    fn handle_free_region(&mut self, free_regions: &mut Vec<Region>, start: Address, end: Address) {
        assert!(start <= end);

        if start == end {
            return;
        }

        free_regions.push(Region::new(start, end));
    }

    fn evacuate(&mut self) {
        for page in self.young.to_pages() {
            self.evacuate_page(page);
        }
    }

    fn evacuate_page(&mut self, page: RegularPage) {
        walk_region(self.vm, page.object_area(), |object, _address, size| {
            if !object.header().is_marked() {
                return;
            }

            if let Some(new_region) = self
                .old_protected
                .allocate(self.vm, self.old, true, size, size)
            {
                let new_address = new_region.start();

                object.copy_to(new_address, size);
                object.header().install_fwdptr(new_address);

                // Clear metadata word.
                let new_obj = new_address.to_obj();
                new_obj.header().clear_mark();
                new_obj.header().set_metadata_raw(false, false);
            } else {
                stdlib::trap(Trap::OOM.int());
            }
        });
    }

    fn forward_reference(&mut self, slot: Slot) {
        let object_address = slot.get();

        if object_address.is_null() {
            return;
        }

        if let Some(forwarded) = self.forward_object(object_address) {
            debug_assert!(self.heap.contains(forwarded));
            slot.set(forwarded);
        }
    }

    fn forward_object(&mut self, object_address: Address) -> Option<Address> {
        if self.heap.contains(object_address) {
            let object = object_address.to_obj();
            let vtblptr = object
                .header()
                .vtblptr_or_fwdptr(self.vm.meta_space_start());

            if let VtblptrWordKind::Fwdptr(address) = vtblptr {
                Some(address)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn walk_old_and_young<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &Obj, Address, usize),
    {
        for page in self.old_protected.pages() {
            walk_region(self.vm, page.object_area(), |obj, addr, size| {
                fct(self, obj, addr, size);
            });
        }

        for page in self.young.to_pages() {
            walk_region(self.vm, page.object_area(), |obj, addr, size| {
                fct(self, obj, addr, size);
            });
        }
    }
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

pub fn verify_marking(
    vm: &VM,
    young: &YoungGen,
    old_protected: &OldGenProtected,
    large: &LargeSpace,
    heap: Region,
) {
    for page in old_protected.pages() {
        verify_marking_region(vm, page.object_area(), heap);
    }

    for page in young.to_pages() {
        verify_marking_region(vm, page.object_area(), heap);
    }

    large.iterate_pages(|page| {
        verify_marking_object(vm, page.object_address(), heap);
    });
}

fn verify_marking_region(vm: &VM, region: Region, heap: Region) {
    walk_region(vm, region, |_obj, obj_address, _size| {
        verify_marking_object(vm, obj_address, heap);
    });
}

fn verify_marking_object(vm: &VM, obj_address: Address, heap: Region) {
    let obj = obj_address.to_obj();

    if obj.header().is_marked() {
        obj.visit_reference_fields(vm.meta_space_start(), |field| {
            let object_addr = field.get();

            if heap.contains(object_addr) {
                assert!(object_addr.to_obj().header().is_marked());
            }
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
