use std::cmp;

use ctxt::SemContext;

use gc::root::IndirectObj;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::in_kilo;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::{CardIdx, Region, CARD_SIZE};
use gc::Address;

use mem;
use object::Obj;
use timer::{in_ms, Timer};

pub struct MinorCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    large: &'a LargeSpace,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    free: Address,
    promotion_failed: bool,
    promoted_size: usize,
    young_from: Region,
}

impl<'a, 'ast> MinorCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        large: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [IndirectObj],
    ) -> MinorCollector<'a, 'ast> {
        MinorCollector {
            ctxt: ctxt,
            young: young,
            old: old,
            large: large,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            free: Address::null(),
            promotion_failed: false,
            promoted_size: 0,
            young_from: young.from_space(),
        }
    }

    pub fn collect(&mut self) {
        let mut timer = Timer::new(self.ctxt.args.flag_gc_verbose);
        let init_size = self.heap_size();
        let young_init_size = self.young.used_region().size();
        self.free = self.young.to_space().start;

        self.young.unprotect_to_space();

        self.visit_roots();
        self.copy_dirty_cards();
        // self.visit_large_objects();
        self.visit_copied_objects();
        self.young.swap_spaces(self.free);

        self.young.protect_to_space();

        timer.stop_with(|dur| {
            let new_size = self.heap_size();
            let young_new_size = self.young.used_region().size();
            let garbage = young_init_size - young_new_size - self.promoted_size;
            let garbage_ratio = (garbage as f64 / young_init_size as f64) * 100f64;

            println!(
                "GC: Minor GC ({:.2} ms, {:.1}K->{:.1}K, young {:.1}K->{:.1}K, \
                 {:.1}K promoted, {:.1}K/{:.0}% garbage)",
                in_ms(dur),
                in_kilo(init_size),
                in_kilo(new_size),
                in_kilo(young_init_size),
                in_kilo(young_new_size),
                in_kilo(self.promoted_size),
                in_kilo(garbage),
                garbage_ratio
            );
        });
    }

    fn heap_size(&self) -> usize {
        self.young.used_region().size() + self.old.used_region().size()
    }

    fn visit_roots(&mut self) {
        // detect all references from roots into young generation
        for &root in self.rootset {
            let root_ptr = root.get();

            if self.young.contains(Address::from_ptr(root_ptr)) {
                root.set(self.copy(root_ptr));
            }
        }
    }

    fn visit_large_objects(&mut self) {
        self.large.visit_objects(|addr| {
            let object = unsafe { &mut *addr.to_mut_ptr::<Obj>() };

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

        for card_idx in start_card_idx..end_card_idx {
            let card_idx = card_idx.into();

            if self.card_table.get(card_idx).is_clean() {
                continue;
            }

            let card_start = self.card_table.to_address(card_idx);
            let card_end = card_start.offset(CARD_SIZE);
            let end = cmp::min(card_end, object_end);

            if card_idx.to_usize() == start_card_idx {
                self.copy_card(card_idx, object_start, end, false);

            } else {
                // all but the first card are full with references
                let refs = end.offset_from(card_start) / mem::ptr_width_usize();
                let mut ref_to_young_gen = false;

                self.copy_refs(card_start, refs, &mut ref_to_young_gen);
                self.update_card(card_idx, ref_to_young_gen);
            }
        }
    }

    fn visit_large_object(&mut self, object: &mut Obj, object_start: Address) {
        if !self.card_table.is_dirty(object_start) { return; }

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(Address::from_ptr(field_ptr)) {
                field.set(self.copy(field_ptr));
            }
        });
    }

    fn visit_copied_objects(&mut self) {
        let mut scan = self.young.to_space().start;

        // visit all fields in copied objects
        while scan < self.free {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if self.young.contains(Address::from_ptr(field_ptr)) {
                    field.set(self.copy(field_ptr));
                }
            });

            scan = scan.offset(object.size());
        }
    }

    // copy all references from old- into young-generation.
    fn copy_dirty_cards(&mut self) {
        self.card_table.visit_dirty_in_old(self.old.free(), |card| {
            let crossing_entry = self.crossing_map.get(card);
            let card_start = self.card_table.to_address(card);
            let card_end = card_start.offset(CARD_SIZE);

            match crossing_entry {
                CrossingEntry::NoRefs => panic!("card dirty without any refs"),
                CrossingEntry::LeadingRefs(refs) => {
                    let mut ref_to_young_gen = false;

                    // copy references at start of card
                    let first_obect = self.copy_refs(card_start, refs as usize, &mut ref_to_young_gen);

                    // copy all objects from this card
                    self.copy_card(card, first_obect, card_end, ref_to_young_gen);
                }

                CrossingEntry::FirstObject(offset) => {
                    let ptr = card_start.offset(offset as usize * mem::ptr_width_usize());

                    // copy all objects from this card
                    self.copy_card(card, ptr, card_end, false);
                }

                CrossingEntry::ArrayStart(offset) => {
                    let ptr = card_start.to_usize() - (offset as usize * mem::ptr_width_usize());

                    // copy all objects from this card
                    self.copy_card(card, ptr.into(), card_end, false);
                }
            }
        });
    }

    fn copy_refs(&mut self, mut ptr: Address, refs: usize, ref_to_young_gen: &mut bool) -> Address {
        for _ in 0..refs {
            let ind_ptr = IndirectObj::from_address(ptr);
            let dir_ptr = ind_ptr.get();

            if self.young_from.contains(Address::from_ptr(dir_ptr)) {
                let copied_obj = self.copy(dir_ptr);
                ind_ptr.set(copied_obj);

                if self.young.contains(Address::from_ptr(copied_obj)) {
                    *ref_to_young_gen = true;
                }
            }

            ptr = ptr.offset(mem::ptr_width_usize());
        }

        ptr
    }

    fn copy_card(
        &mut self,
        card: CardIdx,
        mut ptr: Address,
        card_end: Address,
        mut ref_to_young_gen: bool,
    ) {
        let old_end: Address = self.old.free();
        let mut end = cmp::min(card_end, old_end);

        loop {
            while ptr < end {
                let object = unsafe { &mut *ptr.to_mut_ptr::<Obj>() };

                object.visit_reference_fields_within(end, |field| {
                    let field_ptr = field.get();

                    if self.young_from.contains(Address::from_ptr(field_ptr)) {
                        let copied_obj = self.copy(field_ptr);
                        field.set(copied_obj);

                        // determine if copied object is still in young generation
                        if self.young.contains(Address::from_ptr(copied_obj)) {
                            ref_to_young_gen = true;
                        }
                    }
                });

                ptr = ptr.offset(object.size());
            }

            // if we are in the last card of the old generation, promoted objects
            // will increase `end` towards `card_end`. Those newly promoted objects
            // need also be handled.
            if end == card_end {
                break;
            }

            let old_end = self.old.free();
            let next_end = cmp::min(card_end, old_end);

            if end != next_end {
                end = next_end;
            } else {
                break;
            }
        }

        self.update_card(card, ref_to_young_gen);
    }

    fn update_card(&mut self, card_idx: CardIdx, ref_to_young_gen: bool) {
        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card_idx, CardEntry::Clean);
        }
    }

    fn copy(&mut self, obj: *mut Obj) -> *mut Obj {
        let obj_addr = Address::from_ptr(obj);
        let obj = unsafe { &mut *obj };

        if let Some(fwd) = obj.header().forwarded() {
            return fwd.to_mut_ptr();
        }

        let obj_size = obj.size();
        debug_assert!(
            self.young_from.contains(obj_addr),
            "copy objects only from from-space."
        );

        // if object is old enough we copy it into the old generation
        if self.young.should_be_promoted(obj_addr) {
            let copy_addr = self.old.alloc(obj_size, obj.is_array_ref());

            // if there isn't enough space in old gen keep it in the
            // young generation for now
            if copy_addr.is_null() {
                self.promotion_failed = true;
            } else {
                self.promote_object(obj, copy_addr, obj_size);
                return copy_addr.to_mut_ptr();
            }
        }

        let copy_addr = self.free;
        self.free = copy_addr.offset(obj_size);

        obj.copy_to(copy_addr, obj_size);
        obj.header_mut().forward_to(copy_addr);

        copy_addr.to_mut_ptr()
    }

    fn promote_object(&mut self, obj: &mut Obj, copy_addr: Address, obj_size: usize) {
        obj.copy_to(copy_addr, obj_size);
        self.promoted_size += obj_size;

        // Promoted object can have references to the young generation.
        // Set the card table entry to dirty if this is the case.
        self.handle_promoted_object(copy_addr);

        obj.header_mut().forward_to(copy_addr);
    }

    fn handle_promoted_object(&mut self, addr: Address) {
        let card_idx = self.card_table.card_idx(addr);

        // card is already dirty, nothing left to do.
        if self.card_table.get(card_idx).is_dirty() {
            return;
        }

        let object: &mut Obj = unsafe { &mut *addr.to_mut_ptr() };
        let mut old_to_young_ref = false;

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if self.young.contains(Address::from_ptr(field_ptr)) {
                old_to_young_ref = true;
            }
        });

        if old_to_young_ref {
            self.card_table.set(card_idx, CardEntry::Dirty);
        }
    }

    pub fn promotion_failed(&self) -> bool {
        self.promotion_failed
    }
}
