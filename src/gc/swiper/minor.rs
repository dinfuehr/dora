use parking_lot::MutexGuard;
use std::cmp;

use ctxt::VM;
use gc::root::Slot;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::old::{OldGen, OldGenProtected};
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::swiper::{CardIdx, CARD_SIZE};
use gc::{Address, GcReason, Region};
use mem;
use object::Obj;

pub struct MinorCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,

    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    large: &'a LargeSpace,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    rootset: &'a [Slot],
    reason: GcReason,

    young_top: Address,
    young_limit: Address,
    init_old_top: Vec<Address>,

    promotion_failed: bool,
    promoted_size: usize,

    from_active: Region,
    eden_active: Region,

    min_heap_size: usize,
    max_heap_size: usize,

    config: &'a SharedHeapConfig,
}

impl<'a, 'ast: 'a> MinorCollector<'a, 'ast> {
    pub fn new(
        vm: &'a VM<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        reason: GcReason,
        min_heap_size: usize,
        max_heap_size: usize,
        config: &'a SharedHeapConfig,
    ) -> MinorCollector<'a, 'ast> {
        MinorCollector {
            vm: vm,
            young: young,
            old: old,
            old_protected: old.protected(),
            large: large,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,

            young_top: Address::null(),
            young_limit: Address::null(),
            init_old_top: Vec::new(),

            promotion_failed: false,
            promoted_size: 0,

            from_active: young.from_active(),
            eden_active: young.eden_active(),

            reason: reason,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            config: config,
        }
    }

    pub fn collect(&mut self) -> bool {
        let to_committed = self.young.to_committed();
        self.young_top = to_committed.start;
        self.young_limit = to_committed.end;

        self.init_old_top = self.old_protected.regions.iter().map(|r| r.top()).collect();
        self.young.unprotect_to();

        let dev_verbose = self.vm.args.flag_gc_dev_verbose;

        if dev_verbose {
            println!("Minor GC: Phase 1 (roots)");
        }

        self.visit_roots();

        if dev_verbose {
            println!("Minor GC: Phase 2 (dirty cards)");
        }

        self.visit_dirty_cards();

        if dev_verbose {
            println!("Minor GC: Phase 3 (traverse)");
        }

        self.trace_gray_objects();

        if dev_verbose {
            println!("Minor GC: Phase 3 (traverse) finished");
        }

        if self.promotion_failed {
            // oh no: promotion failed, we need a subsequent full GC
            self.remove_forwarding_pointers();
            self.young.swap_semi_and_keep_to_space(self.young_top);

            return true;
        }

        self.young.clear_eden();
        self.young.swap_semi(self.young_top);
        self.young.protect_to();

        assert!(self.young.eden_active().size() == 0);
        assert!(self.young.to_active().size() == 0);

        let mut config = self.config.lock();
        config.minor_promoted = self.promoted_size;
        config.minor_copied = self.young.from_active().size();

        self.promotion_failed
    }

    fn visit_roots(&mut self) {
        // detect all references from roots into young generation
        for &root in self.rootset {
            let root_ptr = root.get();

            if self.young.contains(root_ptr) {
                root.set(self.copy(root_ptr));
            }
        }
    }

    fn visit_dirty_cards(&mut self) {
        self.visit_dirty_cards_in_old();
        self.visit_dirty_cards_in_large();
    }

    fn visit_dirty_cards_in_large(&mut self) {
        self.large.visit_objects(|addr| {
            let object = addr.to_mut_obj();

            if object.is_array_ref() {
                self.visit_large_object_array(object, addr);
            } else {
                self.visit_large_object(object, addr);
            }
        })
    }

    fn visit_large_object_array(&mut self, object: &mut Obj, object_start: Address) {
        let object_end = object_start.offset(object.size() as usize);
        let (start_card_idx, end_card_idx) = self.card_table.card_indices(object_start, object_end);

        for card_idx in start_card_idx..=end_card_idx {
            let card_idx = card_idx.into();

            if self.card_table.get(card_idx).is_clean() {
                continue;
            }

            let card_start = self.card_table.to_address(card_idx);
            let card_end = card_start.offset(CARD_SIZE);
            let end = cmp::min(card_end, object_end);

            let mut ref_to_young_gen = false;

            if card_idx.to_usize() == start_card_idx {
                self.copy_range(object_start, end, &mut ref_to_young_gen);
            } else {
                // all but the first card are full with references
                let refs = end.offset_from(card_start) / mem::ptr_width_usize();
                self.copy_refs(card_start, refs, &mut ref_to_young_gen);
            }

            self.clean_card_if_no_young_refs(card_idx, ref_to_young_gen);
        }
    }

    fn visit_large_object(&mut self, object: &mut Obj, object_start: Address) {
        let card_idx = self.card_table.card_idx(object_start);
        let mut ref_to_young_gen = false;

        if self.card_table.get(card_idx).is_clean() {
            return;
        }

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(field_ptr) {
                let copied_addr = self.copy(field_ptr);
                field.set(copied_addr);

                if self.young.contains(copied_addr) {
                    ref_to_young_gen = true;
                }
            }
        });

        self.clean_card_if_no_young_refs(card_idx, ref_to_young_gen);
    }

    fn trace_gray_objects(&mut self) {
        let mut young_scan = self.young.to_committed().start;
        let mut old_scan = self.init_old_top.clone();

        let mut work_done = true;

        // visit all fields in gray (=copied) objects
        // there can be gray objects in old & young gen
        while work_done {
            work_done = false;

            while young_scan < self.young_top {
                young_scan = self.trace_young_object(young_scan);
                work_done = true;

            }

            for (id, scan) in old_scan.iter_mut().enumerate() {
                while *scan < self.old_protected.regions[id].top() {
                    *scan = self.trace_old_object(*scan);
                    work_done = true;
                }
            }
        }

        assert!(young_scan == self.young_top);
    }

    fn trace_young_object(&mut self, addr: Address) -> Address {
        let object = addr.to_mut_obj();

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(field_ptr) {
                field.set(self.copy(field_ptr));
            }
        });

        addr.offset(object.size())
    }

    fn trace_old_object(&mut self, object_start: Address) -> Address {
        let object = object_start.to_mut_obj();

        if object.is_array_ref() {
            let mut ref_to_young_gen = false;
            let mut last = object_start;

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if on_different_cards(last, field.address()) && ref_to_young_gen {
                    let card_idx = self.card_table.card_idx(last);
                    self.card_table.set(card_idx, CardEntry::Dirty);
                    ref_to_young_gen = false;
                }

                if self.young.contains(field_ptr) {
                    let copied_addr = self.copy(field_ptr);
                    field.set(copied_addr);

                    if self.young.contains(copied_addr) {
                        ref_to_young_gen = true;
                    }
                }

                last = field.address();
            });

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(last);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        } else {
            let mut ref_to_young_gen = false;

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if self.young.contains(field_ptr) {
                    let copied_addr = self.copy(field_ptr);
                    field.set(copied_addr);

                    if self.young.contains(copied_addr) {
                        ref_to_young_gen = true;
                    }
                }
            });

            if ref_to_young_gen {
                let card_idx = self.card_table.card_idx(object_start);
                self.card_table.set(card_idx, CardEntry::Dirty);
            }
        }

        object_start.offset(object.size())
    }

    fn visit_dirty_cards_in_old(&mut self) {
        let old_regions = self.old_protected.regions.iter().map(|r| r.start()).collect::<Vec<_>>();

        for (id, old_region_start) in old_regions.into_iter().enumerate() {
            let init_old_top = self.init_old_top[id];
            self.visit_dirty_cards_in_old_region(old_region_start, init_old_top);
        }
    }

    // copy all references from old- into young-generation.
    fn visit_dirty_cards_in_old_region(&mut self, start: Address, end: Address) {
        self.card_table
            .visit_dirty_in_old(start, end, |card_idx| {
                let crossing_entry = self.crossing_map.get(card_idx);
                let card_start = self.card_table.to_address(card_idx);

                match crossing_entry {
                    CrossingEntry::NoRefs => panic!("card dirty without any refs"),
                    CrossingEntry::LeadingRefs(refs) => {
                        let mut ref_to_young_gen = false;

                        // copy references at start of card
                        let first_object =
                            self.copy_refs(card_start, refs as usize, &mut ref_to_young_gen);

                        // copy all objects from this card
                        self.copy_old_card(card_idx, first_object, end, ref_to_young_gen);
                    }

                    CrossingEntry::FirstObject(offset) => {
                        let ptr = card_start.offset(offset as usize * mem::ptr_width_usize());

                        // copy all objects from this card
                        self.copy_old_card(card_idx, ptr, end, false);
                    }

                    CrossingEntry::ArrayStart(offset) => {
                        assert!(offset == 1);
                        let ptr =
                            card_start.to_usize() - (offset as usize * mem::ptr_width_usize());

                        // copy all objects from this card
                        self.copy_old_card(card_idx, ptr.into(), end, false);
                    }
                }
            });
    }

    fn copy_refs(&mut self, mut ptr: Address, refs: usize, ref_to_young_gen: &mut bool) -> Address {
        for _ in 0..refs {
            let slot = Slot::at(ptr);
            let dir_ptr = slot.get();

            if self.young.contains(dir_ptr) {
                let copied_obj = self.copy(dir_ptr);
                slot.set(copied_obj);

                if self.young.contains(copied_obj) {
                    *ref_to_young_gen = true;
                }
            }

            ptr = ptr.offset(mem::ptr_width_usize());
        }

        ptr
    }

    fn copy_old_card(&mut self, card: CardIdx, ptr: Address, end: Address, mut ref_to_young_gen: bool) {
        let card_start = self.card_table.to_address(card);
        let card_end = card_start.offset(CARD_SIZE);
        let end = cmp::min(card_end, end);

        self.copy_range(ptr, end, &mut ref_to_young_gen);
        self.clean_card_if_no_young_refs(card, ref_to_young_gen);
    }

    fn copy_range(
        &mut self,
        mut ptr: Address,
        end: Address,
        ref_to_young_gen: &mut bool,
    ) -> Address {
        while ptr < end {
            let object = ptr.to_mut_obj();

            if object.header().vtblptr().is_null() {
                ptr = ptr.add_ptr(1);
                continue;
            }

            object.visit_reference_fields_within(end, |field| {
                let field_ptr = field.get();

                if self.young.contains(field_ptr) {
                    let copied_obj = self.copy(field_ptr);
                    field.set(copied_obj);

                    // determine if copied object is still in young generation
                    if self.young.contains(copied_obj) {
                        *ref_to_young_gen = true;
                    }
                }
            });

            ptr = ptr.offset(object.size());
        }

        end
    }

    fn clean_card_if_no_young_refs(&mut self, card_idx: CardIdx, ref_to_young_gen: bool) {
        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card_idx, CardEntry::Clean);
        }
    }

    fn copy(&mut self, obj_addr: Address) -> Address {
        let obj = obj_addr.to_mut_obj();

        if let Some(fwd) = obj.header().vtblptr_forwarded() {
            return fwd;
        }

        // As soon as promotion of an object failed, objects are not copied anymore.
        if self.promotion_failed {
            return obj_addr;
        }

        let obj_size = obj.size();
        debug_assert!(
            self.from_active.contains(obj_addr) || self.eden_active.contains(obj_addr),
            "copy objects only from from-space."
        );

        let copy_addr = self.young_top;
        let next_young_top = copy_addr.offset(obj_size);

        // if object is old enough we copy it into the old generation
        if self.young.should_be_promoted(obj_addr) || next_young_top > self.young_limit {
            return self.promote_object(obj, obj_size);
        }

        assert!(next_young_top <= self.young_limit);

        self.young_top = next_young_top;
        debug_assert!(self.young.to_committed().valid_top(self.young_top));

        obj.copy_to(copy_addr, obj_size);
        obj.header_mut().vtblptr_forward(copy_addr);

        copy_addr
    }

    fn promote_object(&mut self, obj: &mut Obj, obj_size: usize) -> Address {
        let copy_addr = self.alloc_old(obj_size, obj.is_array_ref());

        // if there isn't enough space in old gen keep it in the
        // young generation for now. A full collection will be forced later and
        // cleans this up.
        if copy_addr.is_null() {
            self.promotion_failed = true;
            return obj.address();
        }

        obj.copy_to(copy_addr, obj_size);
        self.promoted_size += obj_size;

        obj.header_mut().vtblptr_forward(copy_addr);

        copy_addr
    }

    fn alloc_old(&mut self, size: usize, array_ref: bool) -> Address {
        let obj_start = self.old_protected.alloc(self.config, size);

        if obj_start.is_non_null() {
            let obj_end = obj_start.offset(size);
            self.old.update_crossing(obj_start, obj_end, array_ref);
            return obj_start;
        }

        Address::null()
    }

    fn remove_forwarding_pointers(&mut self) {
        let region = self.eden_active.clone();
        self.remove_forwarding_pointers_in_region(region);

        let region = self.from_active.clone();
        self.remove_forwarding_pointers_in_region(region);
    }

    fn remove_forwarding_pointers_in_region(&mut self, region: Region) {
        let mut scan = region.start;

        while scan < region.end {
            let obj = scan.to_mut_obj();

            if obj.header().vtblptr().is_null() {
                scan = scan.add_ptr(1);
                continue;
            }

            obj.header_mut().vtblptr_repair();
            scan = scan.offset(obj.size());
        }

        assert!(scan == region.end);
    }
}
