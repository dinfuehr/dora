use std::sync::Arc;

use parking_lot::MutexGuard;

use crate::gc::space::Space;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected, Page};
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{forward_full, walk_region};
use crate::gc::{fill_region, iterate_strong_roots, iterate_weak_roots, marking, Slot};
use crate::gc::{Address, GcReason, Region};
use crate::object::Obj;
use crate::stdlib;
use crate::threads::DoraThread;
use crate::timer::Timer;
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
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    readonly_space: &'a Space,

    top: Address,
    current_limit: Address,
    pages: Vec<Page>,

    reason: GcReason,

    min_heap_size: usize,
    max_heap_size: usize,

    phases: FullCollectorPhases,
}

impl<'a> FullCollector<'a> {
    pub fn new(
        vm: &'a VM,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        readonly_space: &'a Space,
        rootset: &'a [Slot],
        threads: &'a [Arc<DoraThread>],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> FullCollector<'a> {
        let old_total = old.total();

        FullCollector {
            vm,
            heap,
            young,
            old,
            old_protected: old.protected(),
            pages: Vec::new(),
            large_space,
            rootset,
            threads,
            card_table,
            crossing_map,
            readonly_space,

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
        let dev_verbose = self.vm.flags.gc_dev_verbose;
        let stats = self.vm.flags.gc_stats;

        let mut timer = Timer::new(stats);

        if dev_verbose {
            println!("Full GC: Start");
        }

        self.mark_live();

        if stats {
            let duration = timer.stop();
            self.phases.marking = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 1 (marking)");
        }

        if self.vm.flags.gc_verify {
            verify_marking(
                self.young,
                &*self.old_protected,
                self.large_space,
                self.heap,
            );

            if stats {
                timer.stop();
            }

            if dev_verbose {
                println!("Full GC: Phase 1b (verify marking)");
            }
        }

        self.compute_forward();

        if stats {
            let duration = timer.stop();
            self.phases.compute_forward = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 2 (compute forward)");
        }

        self.update_references();

        if stats {
            let duration = timer.stop();
            self.phases.update_refs = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }

        self.relocate();

        if stats {
            let duration = timer.stop();
            self.phases.relocate = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.reset_cards();

        if stats {
            let duration = timer.stop();
            self.phases.reset_cards = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 5 (reset cards)");
        }

        self.young.clear();
        self.young.protect_from();

        let pages = std::mem::replace(&mut self.pages, Vec::new());
        self.old_protected
            .reset_after_gc(pages, self.top, self.current_limit);
    }

    fn mark_live(&mut self) {
        marking::start(self.rootset, self.heap, self.readonly_space.total());
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young(|full, object, _address, object_size| {
            if object.header().is_marked_non_atomic() {
                let fwd = full.allocate(object_size);
                object.header_mut().set_fwdptr_non_atomic(fwd);
            }
        });

        if !self.fits_into_heap() {
            stdlib::trap(Trap::OOM.int());
        }

        self.old_protected.commit_pages(&self.pages);
    }

    fn fits_into_heap(&mut self) -> bool {
        let young_size = self.young.committed_size();
        let old_size = self.top.align_page().offset_from(self.old.total_start());
        let large_size = self.large_space.committed_size();

        (young_size + old_size + large_size) <= self.max_heap_size
    }

    fn update_references(&mut self) {
        self.walk_old_and_young(|full, object, _address, _| {
            if object.header().is_marked_non_atomic() {
                object.visit_reference_fields(|field| {
                    full.forward_reference(field);
                });
            }
        });

        iterate_strong_roots(self.vm, self.threads, |slot| {
            self.forward_reference(slot);
        });

        iterate_weak_roots(self.vm, |current_address| {
            forward_full(
                current_address,
                self.heap,
                self.readonly_space.total(),
                self.large_space.total(),
            )
        });

        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_mut_obj();

            // reset cards for object, also do this for dead objects
            // to reset card entries to clean.
            self.card_table.reset_addr(object_start);

            if object.header().is_marked_non_atomic() {
                object.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });

                // unmark object for next collection
                object.header_mut().unmark_non_atomic();

                // keep object
                false
            } else {
                // object is unmarked -> free it
                true
            }
        });
    }

    fn relocate(&mut self) {
        self.crossing_map.set_first_object(0.into(), 0);
        let mut previous_end = self.old.total_start();

        self.walk_old_and_young(|full, object, address, object_size| {
            if object.header().is_marked_non_atomic() {
                // find new location
                let dest = object.header().fwdptr_non_atomic();

                if previous_end != dest {
                    let page = Page::from_address(dest);
                    assert_eq!(dest, page.object_area_start());

                    // Fill tail of current page.
                    fill_region(self.vm, previous_end, page.start());
                    full.old.update_crossing(previous_end, page.start());

                    page.initialize_header();
                }

                previous_end = dest.offset(object_size);

                // determine location after relocated object
                let next_dest = dest.offset(object_size);

                if address != dest {
                    object.copy_to(dest, object_size);
                }

                // unmark object for next collection
                let dest_obj = dest.to_mut_obj();
                dest_obj.header_mut().unmark_non_atomic();

                full.old.update_crossing(dest, next_dest);
            }
        });

        fill_region(self.vm, self.top, self.current_limit);
        self.old.update_crossing(self.top, self.current_limit);
    }

    fn reset_cards(&mut self) {
        self.card_table
            .reset_region(self.old.total_start(), self.old_protected.top);
    }

    fn forward_reference(&mut self, slot: Slot) {
        let object_addr = slot.get();

        if self.heap.contains(object_addr) {
            debug_assert!(object_addr.to_obj().header().is_marked_non_atomic());

            if self.large_space.contains(object_addr) {
                // large objects do not move in memory
                return;
            }

            let fwd_addr = object_addr.to_obj().header().fwdptr_non_atomic();
            debug_assert!(self.heap.contains(fwd_addr));
            slot.set(fwd_addr);
        } else {
            debug_assert!(object_addr.is_null() || self.readonly_space.contains(object_addr));
        }
    }

    fn walk_old_and_young<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
    {
        let pages = self.old_protected.pages.clone();
        let mut last = self.old.total_start();

        for page in pages {
            assert_eq!(last, page.start());
            walk_region(page.object_area(), |obj, addr, size| {
                fct(self, obj, addr, size);
            });
            last = page.end();
        }

        // This is a bit strange at first: from-space might not be empty,
        // after too many survivors in the minor GC of the young gen.
        let used_region = self.young.from_active();
        walk_region(used_region, |obj, addr, size| {
            fct(self, obj, addr, size);
        });

        let used_region = self.young.to_active();
        walk_region(used_region, |obj, addr, size| {
            fct(self, obj, addr, size);
        });
    }

    fn allocate(&mut self, object_size: usize) -> Address {
        let addr = self.top;
        let next = self.top.offset(object_size);

        if next <= self.current_limit {
            self.top = next;
            return addr;
        }

        let page = Page::new(self.current_limit);

        if page.end() <= self.old.total().end() {
            self.pages.push(page);

            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();

            let addr = self.top;
            self.top = self.top.offset(object_size);

            addr
        } else {
            panic!("FAIL: Not enough space for objects in old generation.");
        }
    }
}

pub fn verify_marking(
    young: &YoungGen,
    old_protected: &OldGenProtected,
    large: &LargeSpace,
    heap: Region,
) {
    for page in &old_protected.pages {
        verify_marking_region(page.object_area(), heap);
    }

    let from = young.from_active();
    verify_marking_region(from, heap);

    let to = young.to_active();
    verify_marking_region(to, heap);

    large.visit_objects(|obj_address| {
        verify_marking_object(obj_address, heap);
    });
}

fn verify_marking_region(region: Region, heap: Region) {
    walk_region(region, |_obj, obj_address, _size| {
        verify_marking_object(obj_address, heap);
    });
}

fn verify_marking_object(obj_address: Address, heap: Region) {
    let obj = obj_address.to_mut_obj();

    if obj.header().is_marked_non_atomic() {
        obj.visit_reference_fields(|field| {
            let object_addr = field.get();

            if heap.contains(object_addr) {
                assert!(object_addr.to_obj().header().is_marked_non_atomic());
            }
        });
    }
}
