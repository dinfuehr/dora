use std::cmp;

use ctxt::SemContext;

use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::{Region, CARD_SIZE};
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{Card, CrossingEntry, CrossingMap};
use gc::swiper::size_format;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;

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
        self.visit_copied_objects();
        self.young.swap_spaces(self.free);

        self.young.protect_to_space();

        timer.stop_with(|dur| {
            let new_size = self.heap_size();
            let young_new_size = self.young.used_region().size();
            let garbage = young_init_size - young_new_size - self.promoted_size;
            let garbage_ratio = (garbage as f64 / young_init_size as f64) * 100f64;

            println!(
                "GC: Minor GC ({:.2} ms, {}->{}, young {}->{}, \
                 {} promoted, {}/{:.0}% garbage)",
                in_ms(dur),
                size_format(init_size),
                size_format(new_size),
                size_format(young_init_size),
                size_format(young_new_size),
                size_format(self.promoted_size),
                size_format(garbage),
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
        self.card_table.visit_dirty(|card| {
            let crossing_entry = self.crossing_map.get(card);
            let card_start = self.card_table.to_address(card);
            let card_end = card_start.offset(CARD_SIZE);

            match crossing_entry {
                CrossingEntry::NoRefs => panic!("card dirty without any refs"),
                CrossingEntry::LeadingRefs(refs) => {
                    let mut ref_to_young_gen = false;
                    let refs_end = card_start.pointer_offset(refs as usize);

                    self.visit_references(card_start, refs_end, &mut ref_to_young_gen);

                    // copy all objects from this card
                    self.copy_card(card, refs_end, card_end, true, ref_to_young_gen);
                }

                CrossingEntry::FirstObjectOffset(offset) => {
                    let ptr = card_start.offset(offset as usize * mem::ptr_width_usize());

                    // copy all objects from this card
                    self.copy_card(card, ptr, card_end, true, false);
                }

                CrossingEntry::ArrayStart(offset) => {
                    let ptr = card_start.to_usize() - (offset as usize * mem::ptr_width_usize());

                    // copy all objects from this card
                    self.copy_card(card, ptr.into(), card_end, true, false);
                }
            }
        });

        self.large.visit(|obj| {
            let obj_start = Address::from_ptr(obj as *const _);
            let obj_end = obj_start.offset(obj.size());
            let card = self.card_table.card(obj_start);

            if obj.is_obj_array() {
                if self.card_table.get(card).is_dirty() {
                    let first_card_end = self.card_table.to_address(card).offset(CARD_SIZE);
                    debug_assert!(first_card_end < obj_end);
                    self.copy_card(card, obj_start, first_card_end, false, false);
                }

                let card_end = self.card_table.card(obj_end);

                let real_card_end = if obj_end == self.card_table.to_address(card_end) {
                    card_end.to_usize()
                } else {
                    card_end.to_usize() + 1
                };

                for c in card.to_usize() + 1..real_card_end {
                    if self.card_table.get(c.into()).is_clean() {
                        continue;
                    }

                    let card_start = self.card_table.to_address(c.into());
                    let card_end_address = card_start.offset(CARD_SIZE);
                    let end = cmp::min(card_end_address, obj_end);
                    let mut ref_to_young_gen = false;

                    self.visit_references(card_start, end, &mut ref_to_young_gen);

                    if !ref_to_young_gen {
                        self.card_table.set(card, CardEntry::Clean);
                    }
                }
            } else if self.card_table.get(card).is_dirty() {
                let mut ref_to_young_gen = false;
                self.copy_object(obj_start, obj_end, &mut ref_to_young_gen);

                if !ref_to_young_gen {
                    self.card_table.set(card, CardEntry::Clean);
                }
            }

            true
        });
    }

    fn visit_references(&mut self, start: Address, end: Address, ref_to_young_gen: &mut bool) {
        for ref_addr in (start.to_usize()..end.to_usize()).step_by(mem::ptr_width() as usize) {
            let ind_ptr = IndirectObj::from_address(ref_addr.into());
            let dir_ptr = ind_ptr.get();

            if self.young_from.contains(Address::from_ptr(dir_ptr)) {
                let copied_obj = self.copy(dir_ptr);
                ind_ptr.set(copied_obj);

                if self.young.contains(Address::from_ptr(copied_obj)) {
                    *ref_to_young_gen = true;
                }
            }
        }
    }

    fn copy_card(
        &mut self,
        card: Card,
        mut ptr: Address,
        card_end: Address,
        in_old: bool,
        mut ref_to_young_gen: bool,
    ) {
        let old_end: Address = if in_old { self.old.free() } else { card_end };
        let mut end = cmp::min(card_end, old_end);

        loop {
            while ptr < end {
                ptr = self.copy_object(ptr, end, &mut ref_to_young_gen);
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

        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card, CardEntry::Clean);
        }
    }

    fn copy_object(&mut self, addr: Address, end: Address, ref_to_young_gen: &mut bool) -> Address {
        let object = unsafe { &mut *addr.to_mut_ptr::<Obj>() };

        object.visit_reference_fields_within(end, |field| {
            let field_ptr = field.get();

            if self.young_from.contains(Address::from_ptr(field_ptr)) {
                let copied_obj = self.copy(field_ptr);
                field.set(copied_obj);

                // determine if copied object is still in young generation
                if self.young.contains(Address::from_ptr(copied_obj)) {
                    *ref_to_young_gen = true;
                }
            }
        });

        addr.offset(object.size())
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
            let copy_addr = Address::from_ptr(self.old.alloc(obj_size, obj.is_obj_array()));

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
        if obj.is_obj_array() {
            self.handle_promoted_object_array(copy_addr);
        } else {
            self.handle_promoted_object(copy_addr);
        }

        obj.header_mut().forward_to(copy_addr);
    }

    fn handle_promoted_object_array(&mut self, addr: Address) {
        let object: &mut Obj = unsafe { &mut *addr.to_mut_ptr() };
        let mut old_to_young_ref = false;
        let mut last_field = addr;

        object.visit_reference_fields(|field| {
            let field_ptr = field.get();

            if on_different_cards(last_field, field.to_address()) && old_to_young_ref {
                let card = self.card_table.card(last_field);
                self.card_table.set_young_refs(card, true);
                old_to_young_ref = false;
            }

            if self.young.contains(Address::from_ptr(field_ptr)) {
                old_to_young_ref = true;
            }

            last_field = field.to_address();
        });

        if old_to_young_ref {
            let card = self.card_table.card(last_field);
            self.card_table.set(card, CardEntry::Dirty);
        }
    }

    fn handle_promoted_object(&mut self, addr: Address) {
        let card = self.card_table.card(addr);

        // card is already dirty, nothing left to do.
        if self.card_table.get(card).is_dirty() {
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
            self.card_table.set(card, CardEntry::Dirty);
        }
    }

    pub fn promotion_failed(&self) -> bool {
        self.promotion_failed
    }
}
