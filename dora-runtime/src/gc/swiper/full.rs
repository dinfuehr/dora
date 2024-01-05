use std::collections::HashSet;
use std::sync::Arc;

use parking_lot::MutexGuard;

use crate::gc::space::Space;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected, Page};
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{forward_full, walk_region, INITIAL_METADATA_OLD};
use crate::gc::{fill_region, iterate_strong_roots, iterate_weak_roots, marking, Slot};
use crate::gc::{Address, GcReason, Region};
use crate::object::{Obj, MARK_BIT};
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
    use_evacuation: bool,
    young_evacuated_pages: Vec<(Page, usize)>,
    old_evacuated_pages: Vec<(Page, usize)>,

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
            use_evacuation: true,
            young_evacuated_pages: Vec::new(),
            old_evacuated_pages: Vec::new(),

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
                self.vm,
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

        if !self.use_evacuation {
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
        } else {
            self.evacuate();
            self.update_references_after_evacuation();
            self.clear_markbits();
            self.free_pages();
        }

        self.reset_cards();

        if stats {
            let duration = timer.stop();
            self.phases.reset_cards = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 5 (reset cards)");
        }

        self.young.reset_after_full_gc(self.vm);

        if !self.use_evacuation {
            let pages = std::mem::replace(&mut self.pages, Vec::new());
            self.old_protected
                .reset_after_gc(pages, self.top, self.current_limit);
        }
    }

    fn mark_live(&mut self) {
        marking::start(self.rootset, self.heap, self.readonly_space.total());

        iterate_weak_roots(self.vm, |object_address| {
            if self.heap.contains(object_address) {
                let obj = object_address.to_obj();

                if obj.header().is_marked() {
                    Some(object_address)
                } else {
                    None
                }
            } else {
                assert!(self.readonly_space.contains(object_address));
                Some(object_address)
            }
        });

        for page in self.young.pages() {
            let live = self.compute_live_on_page(page);

            if live > 0 {
                self.young_evacuated_pages.push((page, live));
            }
        }

        let mut old_evacuated_pages = Vec::new();

        for page in self.old_protected.pages() {
            let live = self.compute_live_on_page(page);

            if live == 0 {
                self.old_protected.free_page(page);
            } else {
                old_evacuated_pages.push((page, live));
            }
        }

        old_evacuated_pages.sort_by(|a, b| a.1.cmp(&b.1));
        old_evacuated_pages.truncate(10);

        let _ = std::mem::replace(&mut self.old_evacuated_pages, old_evacuated_pages);
    }

    fn compute_live_on_page(&self, page: Page) -> usize {
        let mut live = 0;
        walk_region(self.vm, page.object_area(), |obj, _addr, size| {
            if obj.header().is_marked() {
                live += size;
            }
        });
        live
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young(|full, object, _address, object_size| {
            if object.header().is_marked() {
                let fwd = full.allocate(object_size);
                object.header().set_metadata_fwdptr(fwd);
            }
        });

        if !self.fits_into_heap() {
            stdlib::trap(Trap::OOM.int());
        }

        self.old_protected.commit_pages(&self.pages);
    }

    fn fits_into_heap(&mut self) -> bool {
        let young_size = self.young.committed_size();
        let old_size = self.top.align_page_up().offset_from(self.old.total_start());
        let large_size = self.large_space.committed_size();

        (young_size + old_size + large_size) <= self.max_heap_size
    }

    fn update_references(&mut self) {
        self.walk_old_and_young(|full, object, _address, _| {
            if object.header().is_marked() {
                object.visit_reference_fields(|field| {
                    full.forward_reference(field);
                });
            }
        });

        iterate_strong_roots(self.vm, self.threads, |slot| {
            self.forward_reference(slot);
        });

        iterate_weak_roots(self.vm, |object_address| {
            forward_full(object_address, self.heap, self.readonly_space.total())
        });

        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_obj();

            // reset cards for object, also do this for dead objects
            // to reset card entries to clean.
            self.card_table.reset_addr(object_start);

            if object.header().is_marked() {
                object.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });

                // unmark object for next collection
                object.header().unmark();

                // keep object
                false
            } else {
                // object is unmarked -> free it
                true
            }
        });
    }

    fn update_references_after_evacuation(&mut self) {
        let old_evacuated_set: HashSet<Page> =
            HashSet::from_iter(self.old_evacuated_pages.iter().map(|pair| pair.0));

        for page in self.old_protected.pages() {
            if old_evacuated_set.contains(&page) {
                continue;
            }

            walk_region(self.vm, page.object_area(), |object, _addr, _size| {
                if object.header().is_marked() {
                    object.visit_reference_fields(|field| {
                        self.forward_reference(field);
                    });
                }
            });
        }

        iterate_strong_roots(self.vm, self.threads, |slot| {
            self.forward_reference(slot);
        });

        iterate_weak_roots(self.vm, |object_address| {
            forward_full(object_address, self.heap, self.readonly_space.total())
        });

        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_obj();

            // reset cards for object, also do this for dead objects
            // to reset card entries to clean.
            self.card_table.reset_addr(object_start);

            if object.header().is_marked() {
                object.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });

                // keep object
                false
            } else {
                // object is unmarked -> free it
                true
            }
        });
    }

    fn clear_markbits(&mut self) {
        let old_evacuated_set: HashSet<Page> =
            HashSet::from_iter(self.old_evacuated_pages.iter().map(|pair| pair.0));

        for page in self.old_protected.pages() {
            if old_evacuated_set.contains(&page) {
                continue;
            }

            walk_region(self.vm, page.object_area(), |object, _addr, _size| {
                if object.header().is_marked() {
                    object.header().unmark();
                }
            });
        }

        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_obj();
            assert!(object.header().is_marked());

            // unmark object for next collection
            object.header().unmark();

            // keep object
            false
        });
    }

    fn relocate(&mut self) {
        self.crossing_map.set_first_object(0.into(), 0);
        let mut previous_end = self.old.total_start();

        self.walk_old_and_young(|full, object, address, object_size| {
            if object.header().is_marked() {
                // find new location
                let dest = object.header().metadata_fwdptr();

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

                // Clear metadata word.
                let dest_obj = dest.to_obj();
                dest_obj.header().set_metadata_raw(INITIAL_METADATA_OLD);

                full.old.update_crossing(dest, next_dest);
            }
        });

        fill_region(self.vm, self.top, self.current_limit);
        self.old.update_crossing(self.top, self.current_limit);
    }

    fn evacuate(&mut self) {
        self.old_protected.fill_alloc_page();

        for (page, _) in self.young_evacuated_pages.clone() {
            self.evacuate_page(page);
        }

        for (page, _) in self.old_evacuated_pages.clone() {
            self.evacuate_page(page);
        }
    }

    fn evacuate_page(&mut self, page: Page) {
        walk_region(self.vm, page.object_area(), |object, _address, size| {
            if !object.header().is_marked() {
                return;
            }

            if let Some(new_address) = self.old_protected.allocate(self.vm, self.old, size) {
                let object_end = new_address.offset(size);

                object.header().set_metadata_fwdptr(new_address);
                object.copy_to(new_address, size);

                // Clear metadata word.
                let new_obj = new_address.to_obj();
                new_obj
                    .header()
                    .set_metadata_raw(INITIAL_METADATA_OLD | MARK_BIT);

                self.old.update_crossing(new_address, object_end);
            } else {
                stdlib::trap(Trap::OOM.int());
            }
        });
    }

    fn free_pages(&mut self) {
        for (page, _) in self.old_evacuated_pages.clone() {
            self.old_protected.free_page(page);
        }
    }

    fn reset_cards(&mut self) {
        for page in self.old_protected.pages() {
            self.card_table.reset_page(page);
        }
    }

    fn forward_reference(&mut self, slot: Slot) {
        let object_address = slot.get();

        if object_address.is_null() {
            return;
        }

        if self.heap.contains(object_address) {
            let object = object_address.to_obj();
            debug_assert!(object.header().is_marked());

            let fwd_addr = object.header().metadata_fwdptr();

            if fwd_addr.is_non_null() {
                debug_assert!(self.heap.contains(fwd_addr));
                slot.set(fwd_addr);
            }
        } else {
            debug_assert!(self.readonly_space.contains(object_address));
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

        for page in self.young.pages() {
            walk_region(self.vm, page.object_area(), |obj, addr, size| {
                fct(self, obj, addr, size);
            });
        }
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
    vm: &VM,
    young: &YoungGen,
    old_protected: &OldGenProtected,
    large: &LargeSpace,
    heap: Region,
) {
    for page in old_protected.pages() {
        verify_marking_region(vm, page.object_area(), heap);
    }

    for page in young.pages() {
        verify_marking_region(vm, page.object_area(), heap);
    }

    large.visit_objects(|obj_address| {
        verify_marking_object(obj_address, heap);
    });
}

fn verify_marking_region(vm: &VM, region: Region, heap: Region) {
    walk_region(vm, region, |_obj, obj_address, _size| {
        verify_marking_object(obj_address, heap);
    });
}

fn verify_marking_object(obj_address: Address, heap: Region) {
    let obj = obj_address.to_obj();

    if obj.header().is_marked() {
        obj.visit_reference_fields(|field| {
            let object_addr = field.get();

            if heap.contains(object_addr) {
                assert!(object_addr.to_obj().header().is_marked());
            }
        });
    }
}
