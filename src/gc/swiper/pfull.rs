use scoped_threadpool::Pool;
use std::cmp;

use ctxt::VM;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::marking;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::{align_gen, Address, GcReason, Region};
use object::Obj;

pub struct ParallelFullCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    old_top: Address,
    old_limit: Address,
    init_old_top: Address,

    reason: GcReason,
    threadpool: &'a mut Pool,

    min_heap_size: usize,
    max_heap_size: usize,
}

impl<'a, 'ast> ParallelFullCollector<'a, 'ast> {
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
        threadpool: &'a mut Pool,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> ParallelFullCollector<'a, 'ast> {
        let old_total = old.total();

        ParallelFullCollector {
            vm: vm,
            heap: heap,
            young: young,
            old: old,
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            old_top: old_total.start,
            old_limit: old_total.end,
            init_old_top: Address::null(),

            reason: reason,
            threadpool: threadpool,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,
        }
    }

    pub fn collect(&mut self) {
        let dev_verbose = self.vm.args.flag_gc_dev_verbose;
        self.init_old_top = self.old.active().end;

        if dev_verbose {
            println!(
                "Full GC: init: eden={} from={} old={}",
                self.young.eden_active(),
                self.young.from_active(),
                self.old.active()
            );
            println!("Full GC: Phase 1 (marking)");
        }

        self.mark_live();

        if dev_verbose {
            println!("Full GC: Phase 2 (forward)");
        }

        self.compute_forward();

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }

        self.update_references();

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.relocate();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects)");
        }

        self.update_large_objects();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects) finished.");
            println!(
                "Full GC: final: eden={} from={} old={}",
                self.young.eden_active(),
                self.young.from_active(),
                self.old.active()
            );
        }

        self.reset_cards();

        let old_start = self.old.total().start;
        let old_size = align_gen(self.old_top.offset_from(old_start));
        self.old.set_committed_size(old_size);
    }

    fn mark_live(&mut self) {
        marking::start(
            self.rootset,
            self.heap.clone(),
            self.perm_space.total(),
            self.threadpool,
        );
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young(|full, object, _address, object_size| {
            if object.header().is_marked_non_atomic() {
                let fwd = full.allocate(object_size);
                object.header_mut().set_fwdptr_non_atomic(fwd);
            }
        });

        let old_start = self.old.total().start;
        let init_old_size = self.init_old_top.offset_from(old_start);
        let new_old_size = self.old_top.offset_from(old_start);

        let old_size = align_gen(cmp::max(init_old_size, new_old_size));
        self.old.set_committed_size(old_size);
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

        self.large_space.visit_objects(|object_start| {
            let object = object_start.to_mut_obj();

            if object.header().is_marked_non_atomic() {
                object.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });
            }
        });
    }

    fn relocate(&mut self) {
        self.crossing_map.set_first_object(0.into(), 0);

        self.walk_old_and_young(|full, object, address, object_size| {
            if object.header().is_marked_non_atomic() {
                // get new location
                let dest = object.header().fwdptr_non_atomic();
                debug_assert!(full.old.committed().contains(dest));

                // determine location after relocated object
                let next_dest = dest.offset(object_size);
                debug_assert!(full.old.committed().valid_top(next_dest));

                if address != dest {
                    object.copy_to(dest, object_size);
                }

                // unmark object for next collection
                let dest_obj = dest.to_mut_obj();
                dest_obj.header_mut().unmark_non_atomic();

                if on_different_cards(dest, next_dest) {
                    full.old
                        .update_crossing(dest, next_dest, dest_obj.is_array_ref());
                }
            }
        });

        self.young.clear();
        self.young.protect_to();

        assert!(self.old.valid_top(self.old_top));
        self.old.update_top(self.old_top);
    }

    fn update_large_objects(&mut self) {
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

            if !object.header().is_marked_non_atomic() {
                // object is unmarked -> free it
                return false;
            }

            // unmark object for next collection
            object.header_mut().unmark_non_atomic();

            // keep object
            true
        });
    }

    fn reset_cards(&mut self) {
        let old_total = self.old.total();
        let start = old_total.start;
        let end = cmp::max(self.old_top, self.init_old_top);

        debug_assert!(old_total.valid_top(self.old_top));
        debug_assert!(old_total.valid_top(self.init_old_top));

        self.card_table.reset_region(start, end);
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
        F: FnMut(&mut ParallelFullCollector, &mut Obj, Address, usize),
    {
        let used_region = self.old.active();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        let used_region = self.young.eden_active();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        let used_region = self.young.from_active();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        // This is a bit strange at first: to-space might not be empty,
        // after too many survivors in the minor GC of the young gen.
        let used_region = self.young.to_active();
        self.walk_region(used_region.start, used_region.end, &mut fct);
    }

    fn walk_region<F>(&mut self, start: Address, end: Address, fct: &mut F)
    where
        F: FnMut(&mut ParallelFullCollector, &mut Obj, Address, usize),
    {
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
