use parking_lot::MutexGuard;
use std::cmp;

use crate::ctxt::VM;
use crate::gc::marking;
use crate::gc::root::Slot;
use crate::gc::space::Space;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected};
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{walk_region, walk_region_and_skip_garbage};
use crate::gc::{Address, GcReason, Region};
use crate::object::Obj;
use crate::os::signal::Trap;
use crate::stdlib;
use crate::timer::Timer;

pub struct FullCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    old_top: Address,
    old_limit: Address,
    old_committed: Region,
    init_old_top: Vec<Address>,

    reason: GcReason,

    min_heap_size: usize,
    max_heap_size: usize,

    phases: FullCollectorPhases,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        vm: &'a VM<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        perm_space: &'a Space,
        rootset: &'a [Slot],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> FullCollector<'a, 'ast> {
        let old_total = old.total();

        FullCollector {
            vm: vm,
            heap: heap,
            young: young,
            old: old,
            old_protected: old.protected(),
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            old_top: old_total.start,
            old_limit: old_total.end,
            old_committed: Default::default(),
            init_old_top: Vec::new(),

            reason: reason,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            phases: FullCollectorPhases::new(),
        }
    }

    pub fn phases(&self) -> FullCollectorPhases {
        self.phases.clone()
    }

    pub fn collect(&mut self) {
        let dev_verbose = self.vm.args.flag_gc_dev_verbose;
        let stats = self.vm.args.flag_gc_stats;
        self.init_old_top = self.old_protected.regions.iter().map(|r| r.top()).collect();

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

        if self.vm.args.flag_gc_verify {
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

        self.old_protected.update_single_region(self.old_top);
    }

    fn mark_live(&mut self) {
        marking::start(self.rootset, self.heap, self.perm_space.total());
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young_and_skip_garbage(|full, object, _address, object_size| {
            if object.header().is_marked_non_atomic() {
                let fwd = full.allocate(object_size);
                object.header_mut().set_fwdptr_non_atomic(fwd);
                true
            } else {
                false
            }
        });

        if !self.fits_into_heap() {
            stdlib::trap(Trap::OOM.int());
        }

        self.old_protected.commit_single_region(self.old_top);
        self.old_committed = Region::new(self.old.total_start(), self.old_top);
    }

    fn fits_into_heap(&mut self) -> bool {
        let (eden_size, semi_size) = self.young.committed_size();
        let young_size = eden_size + semi_size;
        let old_size = self.old_top.align_gen().offset_from(self.old.total_start());
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

        for root in self.rootset {
            self.forward_reference(*root);
        }

        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_mut_obj();

            // reset cards for object, also do this for dead objects
            // to reset card entries to clean.
            if object.is_array_ref() {
                let object_end = object_start.offset(object.size());
                self.card_table.reset_region(object_start, object_end);
            } else {
                self.card_table.reset_addr(object_start);
            }

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

        self.walk_old_and_young(|full, object, address, object_size| {
            if object.header().is_marked_non_atomic() {
                // get new location
                let dest = object.header().fwdptr_non_atomic();
                debug_assert!(full.old_committed.contains(dest));

                // determine location after relocated object
                let next_dest = dest.offset(object_size);
                debug_assert!(full.old_committed.valid_top(next_dest));

                if address != dest {
                    object.copy_to(dest, object_size);
                }

                // unmark object for next collection
                let dest_obj = dest.to_mut_obj();
                dest_obj.header_mut().unmark_non_atomic();

                full.old
                    .update_crossing(dest, next_dest, dest_obj.is_array_ref());
            }
        });
    }

    fn reset_cards(&mut self) {
        let regions = self
            .old_protected
            .regions
            .iter()
            .map(|r| (r.start(), r.top()))
            .collect::<Vec<_>>();

        for ((start, top), init_top) in regions.into_iter().zip(&self.init_old_top) {
            let top = cmp::max(top, *init_top);
            self.card_table.reset_region(start, top);
        }
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
            debug_assert!(object_addr.is_null() || self.perm_space.contains(object_addr));
        }
    }

    fn walk_old_and_young<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
    {
        let old_regions = self
            .old_protected
            .regions
            .iter()
            .map(|r| r.active_region())
            .collect::<Vec<_>>();

        for old_region in old_regions {
            walk_region(old_region, |obj, addr, size| {
                fct(self, obj, addr, size);
            });
        }

        let used_region = self.young.eden_active();
        walk_region(used_region, |obj, addr, size| {
            fct(self, obj, addr, size);
        });

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

    fn walk_old_and_young_and_skip_garbage<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize) -> bool,
    {
        let old_regions = self
            .old_protected
            .regions
            .iter()
            .map(|r| r.active_region())
            .collect::<Vec<_>>();

        let vm = self.vm;

        for old_region in old_regions {
            walk_region_and_skip_garbage(vm, old_region, |obj, addr, size| {
                fct(self, obj, addr, size)
            });
        }

        let used_region = self.young.eden_active();
        walk_region_and_skip_garbage(vm, used_region, |obj, addr, size| {
            fct(self, obj, addr, size)
        });

        // This is a bit strange at first: from-space might not be empty,
        // after too many survivors in the minor GC of the young gen.
        let used_region = self.young.from_active();
        walk_region_and_skip_garbage(vm, used_region, |obj, addr, size| {
            fct(self, obj, addr, size)
        });

        let used_region = self.young.to_active();
        walk_region_and_skip_garbage(vm, used_region, |obj, addr, size| {
            fct(self, obj, addr, size)
        });
    }

    fn allocate(&mut self, object_size: usize) -> Address {
        let addr = self.old_top;
        let next = self.old_top.offset(object_size);

        if next <= self.old_limit {
            self.old_top = next;
            return addr;
        }

        panic!("FAIL: Not enough space for objects in old generation.");
    }
}

pub fn verify_marking(
    young: &YoungGen,
    old_protected: &OldGenProtected,
    large: &LargeSpace,
    heap: Region,
) {
    for region in &old_protected.regions {
        let active = region.active_region();
        verify_marking_region(active, heap);
    }

    let eden = young.eden_active();
    verify_marking_region(eden, heap);

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
