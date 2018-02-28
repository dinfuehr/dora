use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::old::OldGen;
use gc::swiper::Region;
use gc::swiper::young::YoungGen;

use mem;
use object::Obj;

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [IndirectObj],

    refs_to_young_gen: usize,
    in_old: bool,

    old_region: Region,
    young_region: Region,
}

impl<'a> Verifier<'a> {
    pub fn new(
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [IndirectObj],
    ) -> Verifier<'a> {
        Verifier {
            young: young,
            old: old,
            card_table: card_table,
            crossing_map: crossing_map,
            rootset: rootset,

            refs_to_young_gen: 0,
            in_old: false,

            young_region: young.used_region(),
            old_region: old.used_region(),
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_young();
        self.verify_old();
    }

    fn verify_young(&mut self) {
        let region = self.young_region.clone();
        self.verify_objects(region, "young gen");
    }

    fn verify_old(&mut self) {
        let region = self.old_region.clone();
        self.in_old = true;
        self.verify_objects(region, "old gen");
        self.in_old = false;
    }

    fn verify_roots(&mut self) {
        for root in self.rootset {
            let root_ptr = root.get();
            self.verify_reference(root_ptr, root.to_address(), Address::null(), "root set");
        }
    }

    fn verify_objects(&mut self, region: Region, name: &str) {
        let mut curr = region.start;
        self.refs_to_young_gen = 0;

        if self.in_old {
            // we should start at card start
            assert!(self.old.is_card_aligned(curr));
            self.verify_crossing(curr);
        }

        while curr < region.end {
            let object = unsafe { &mut *curr.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();
                self.verify_reference(child_ptr, child.to_address(), curr, name);
            });

            let next = curr.offset(object.size());

            if self.in_old && on_different_cards(curr, next) {
                self.verify_card(curr);
                self.verify_crossing(next);
            }

            curr = next;
        }

        assert!(curr == region.end, "object doesn't end at region end");

        if self.in_old && !start_of_card(curr) {
            self.verify_card(curr);
        }
    }

    fn verify_card(&mut self, curr: Address) {
        let curr_card = self.old.card_from_address(curr);

        let card_entry = self.card_table.get(curr_card);
        let expected_card_entry = if self.refs_to_young_gen > 0 {
            CardEntry::Dirty
        } else {
            CardEntry::Clean
        };

        if card_entry != expected_card_entry {
            let card_text = match card_entry {
                CardEntry::Dirty => "dirty",
                CardEntry::Clean => "clean",
            };

            println!(
                "CARD: {} is marked {} but has {} reference(s).",
                curr_card.to_usize(),
                card_text,
                self.refs_to_young_gen
            );

            panic!("card table entry wrong.");
        }

        assert!(card_entry == expected_card_entry);

        self.refs_to_young_gen = 0;
    }

    fn verify_crossing(&mut self, addr: Address) {
        let card = self.old.card_from_address(addr);
        let card_start = self.old.address_from_card(card);
        let offset = addr.offset_from(card_start);
        let offset_words = offset / (mem::ptr_width() as usize);

        let crossing = self.crossing_map.get(card);
        assert!(crossing == CrossingEntry::FirstObjectOffset(offset_words as u8));
    }

    fn verify_reference(
        &mut self,
        obj: *mut Obj,
        ref_addr: Address,
        obj_addr: Address,
        name: &str,
    ) {
        let addr = Address::from_ptr(obj);

        if obj.is_null() {
            return;
        }

        if self.old_region.contains(addr) || self.young_region.contains(addr) {
            let object = unsafe { &mut *obj };

            // Verify that the address is the start of an object,
            // for this access its size.
            // To make sure this isn't optimized out by the compiler,
            // make sure that the size doesn't equal 1.
            assert!(object.size() != 1, "object size shouldn't be 1");

            if self.young_region.contains(addr) {
                self.refs_to_young_gen += 1;
            }

            return;
        }

        println!(
            "YNG: {:x}-{:x}; active: {:x}-{:x}",
            self.young.total.start.to_usize(),
            self.young.total.end.to_usize(),
            self.young_region.start.to_usize(),
            self.young_region.end.to_usize()
        );
        println!(
            "OLD: {:x}-{:x}; active: {:x}-{:x}",
            self.old.total.start.to_usize(),
            self.old.total.end.to_usize(),
            self.old_region.start.to_usize(),
            self.old_region.end.to_usize()
        );
        println!(
            "found invalid reference to {:x} in {} (at {:x}, in object {:x}).",
            addr.to_usize(),
            name,
            ref_addr.to_usize(),
            obj_addr.to_usize()
        );

        if self.young.contains(addr) && !self.young_region.contains(addr) {
            println!("reference points into young generation but not into the active semi-space.");
        }

        panic!("reference neither pointing into young nor old generation.");
    }
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

fn start_of_card(addr: Address) -> bool {
    (addr.to_usize() & (CARD_SIZE - 1)) == addr.to_usize()
}
