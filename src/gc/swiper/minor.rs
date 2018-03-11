use std::cmp;

use ctxt::SemContext;

use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::CARD_SIZE;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{Card, CrossingEntry, CrossingMap};
use gc::swiper::in_kilo;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;

use mem;
use object::Obj;
use timer::{in_ms, Timer};

pub struct MinorCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    young: &'a YoungGen,
    old: &'a OldGen,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    free: Address,
    promotion_failed: bool,
    promoted_size: usize,
}

impl<'a, 'ast> MinorCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [IndirectObj],
    ) -> MinorCollector<'a, 'ast> {
        MinorCollector {
            ctxt: ctxt,
            young: young,
            old: old,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            free: Address::null(),
            promotion_failed: false,
            promoted_size: 0,
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

            println!("GC: Minor GC ({:.2} ms, {:.1}K->{:.1}K, young {:.1}K->{:.1}K, \
                      {:.1}K promoted, {:.1}K/{:.0}% garbage)",
                in_ms(dur), in_kilo(init_size), in_kilo(new_size),
                in_kilo(young_init_size), in_kilo(young_new_size),
                in_kilo(self.promoted_size),
                in_kilo(garbage), garbage_ratio);
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
            let card_start = self.old.address_from_card(card);
            let card_end = card_start.offset(CARD_SIZE);

            match crossing_entry {
                CrossingEntry::NoRefs => panic!("card dirty without any refs"),
                CrossingEntry::LeadingRefs(refs) => {
                    let mut ptr = card_start;

                    for _ in 0..refs {
                        let ind_ptr = IndirectObj::from_address(ptr);
                        let dir_ptr = ind_ptr.get();

                        if self.young.from_space().contains(Address::from_ptr(dir_ptr)) {
                            ind_ptr.set(self.copy(dir_ptr));
                        }

                        ptr = ptr.offset(mem::ptr_width() as usize);
                    }

                    // copy all objects from this card
                    self.copy_card(card, ptr, card_end);
                }

                CrossingEntry::FirstObjectOffset(offset) => {
                    let ptr = card_start.offset(offset as usize * mem::ptr_width_usize());

                    // copy all objects from this card
                    self.copy_card(card, ptr, card_end);
                }
            }
        });
    }

    fn copy_card(&mut self, card: Card, mut ptr: Address, card_end: Address) {
        let old_end: Address = self.old.free();
        let mut end = cmp::min(card_end, old_end);
        let mut ref_to_young_gen = false;

        loop {
            while ptr < end {
                let object = unsafe { &mut *ptr.to_mut_ptr::<Obj>() };

                object.visit_reference_fields(|field| {
                    let field_ptr = field.get();

                    if self.young.from_space().contains(Address::from_ptr(field_ptr)) {
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

        // if there are no references to the young generation in this card,
        // set the card to clean.
        if !ref_to_young_gen {
            self.card_table.set(card, CardEntry::Clean);
        }
    }

    fn copy(&mut self, obj: *mut Obj) -> *mut Obj {
        let obj_addr = Address::from_ptr(obj);
        let obj = unsafe { &mut *obj };

        if let Some(fwd) = obj.header().forwarded() {
            return fwd.to_mut_ptr();
        }

        let obj_size = obj.size();
        debug_assert!(self.young.from_space().contains(obj_addr), "copy objects only from from-space.");

        // if object is old enough we copy it into the old generation
        if self.young.should_be_promoted(obj_addr) {
            let copy_addr = Address::from_ptr(self.old.alloc(obj_size, obj.is_array_ref()));

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
        let card = self.old.card_from_address(addr);

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
