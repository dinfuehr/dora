use std::cmp;

use ctxt::SemContext;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::{formatted_size, Address, GcReason, Region};
use object::Obj;
use timer::Timer;

pub struct FullCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    fwd: Address,
    fwd_end: Address,
    old_top: Address,

    reason: GcReason,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        perm_space: &'a Space,
        rootset: &'a [Slot],
        reason: GcReason,
    ) -> FullCollector<'a, 'ast> {
        let old_committed = old.committed();

        FullCollector {
            ctxt: ctxt,
            heap: heap,
            young: young,
            old: old,
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            fwd: old_committed.start,
            fwd_end: old_committed.end,
            old_top: Address::null(),

            reason: reason,
        }
    }

    pub fn collect(&mut self) {
        let active = self.ctxt.args.flag_gc_verbose;
        let dev_verbose = self.ctxt.args.flag_gc_dev_verbose;
        let mut timer = Timer::new(active);
        let init_size = self.heap_size();
        self.old_top = self.old.active().end;

        if dev_verbose {
            println!(
                "Full GC: init: eden={} from={} old={}",
                self.young.eden_active(),
                self.young.from_active(),
                self.old.active()
            );
            println!("Full GC: Phase 1 (marking)");
        }
        let time_mark = Timer::ms(active, || {
            self.mark_live();
        });

        if dev_verbose {
            println!("Full GC: Phase 2 (forward)");
        }
        let time_forward = Timer::ms(active, || {
            self.compute_forward();
        });

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }
        let time_updateref = Timer::ms(active, || {
            self.update_references();
        });

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }
        let time_relocate = Timer::ms(active, || {
            self.relocate();
        });

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects)");
        }
        let time_large = Timer::ms(active, || {
            self.update_large_objects();
        });

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

        timer.stop_with(|time_pause| {
            let new_size = self.heap_size();
            let garbage = init_size - new_size;
            let garbage_ratio = if init_size == 0 {
                0f64
            } else {
                (garbage as f64 / init_size as f64) * 100f64
            };

            println!(
                "GC: Full GC ({:.1} ms, {}->{} size, {}/{:.0}% garbage); \
                 mark={:.1}ms forward={:.1}ms updateref={:.1}ms relocate={:.1}ms large={:.1}ms; ({})",
                time_pause,
                formatted_size(init_size),
                formatted_size(new_size),
                formatted_size(garbage),
                garbage_ratio,
                time_mark,
                time_forward,
                time_updateref,
                time_relocate,
                time_large,
                self.reason,
            );
        });
    }

    fn heap_size(&self) -> usize {
        self.young.active_size() + self.old.active_size()
    }

    fn mark_live(&mut self) {
        let mut marking_stack: Vec<Address> = Vec::new();

        for root in self.rootset {
            let root_ptr = root.get();

            if self.heap.contains(root_ptr) {
                let root_obj = root_ptr.to_mut_obj();

                if !root_obj.header().is_marked() {
                    marking_stack.push(root_ptr);
                    root_obj.header_mut().mark();
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

                    if !field_obj.header().is_marked() {
                        marking_stack.push(field_addr);
                        field_obj.header_mut().mark();
                    }
                } else {
                    debug_assert!(field_addr.is_null() || self.perm_space.contains(field_addr));
                }
            });
        }
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young(|full, object, _address, object_size| {
            if object.header().is_marked() {
                let fwd = full.allocate(object_size);
                object.header_mut().set_fwdptr(fwd);
            }
        });
    }

    fn update_references(&mut self) {
        self.walk_old_and_young(|full, object, _address, _| {
            if object.header().is_marked() {
                object.visit_reference_fields(|field| {
                    full.forward_reference(field);
                });
            }
        });

        for root in self.rootset {
            self.forward_reference(*root);
        }
    }

    fn relocate(&mut self) {
        self.crossing_map.set_first_object(0.into(), 0);

        self.walk_old_and_young(|full, object, _address, object_size| {
            if object.header().is_marked() {
                // get new location
                let dest = object.header().fwdptr();
                debug_assert!(full.old.committed().contains(dest));

                // determine location after relocated object
                let next_dest = dest.offset(object_size);
                debug_assert!(full.old.committed().valid_top(next_dest));

                object.copy_to(dest, object_size);

                // unmark object for next collection
                dest.to_mut_obj().header_mut().unmark();

                if on_different_cards(dest, next_dest) {
                    full.old
                        .update_crossing(dest, next_dest, object.is_array_ref());
                }
            }
        });

        self.young.clear();

        assert!(self.old.valid_top(self.fwd));
        self.old.update_free(self.fwd);
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

            if !object.header().is_marked() {
                // object is unmarked -> free it
                return false;
            }

            // unmark object for next collection
            object.header_mut().unmark();

            object.visit_reference_fields(|field| {
                self.forward_reference(field);
            });

            // keep object
            true
        });
    }

    fn reset_cards(&mut self) {
        let start = self.old.total().start;
        let end = cmp::max(self.fwd, self.old_top);
        self.card_table.reset_region(start, end);
    }

    fn forward_reference(&mut self, slot: Slot) {
        let object_addr = slot.get();

        if self.heap.contains(object_addr) && !self.large_space.contains(object_addr) {
            debug_assert!(object_addr.to_obj().header().is_marked());
            let fwd_addr = object_addr.to_obj().header().fwdptr();
            debug_assert!(self.heap.contains(fwd_addr));
            slot.set(fwd_addr);
        } else {
            debug_assert!(
                object_addr.is_null()
                    || self.perm_space.contains(object_addr)
                    || self.large_space.contains(object_addr)
            );
        }
    }

    fn walk_old_and_young<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
    {
        let used_region = self.old.active();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        let used_region = self.young.eden_active();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        let used_region = self.young.from_active();
        self.walk_region(used_region.start, used_region.end, &mut fct);
    }

    fn walk_region<F>(&mut self, start: Address, end: Address, fct: &mut F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
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
        let addr = self.fwd;
        let next = self.fwd.offset(object_size);

        if next <= self.fwd_end {
            self.fwd = next;
            return addr;
        }

        panic!("FAIL: Not enough space for objects in old generation.");
    }
}
