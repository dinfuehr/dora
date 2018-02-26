use gc::Address;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::crossing::CrossingMap;
use gc::swiper::old::OldGen;
use gc::swiper::Region;
use gc::swiper::young::YoungGen;

use object::Obj;

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,
    card: &'a CardTable,
    crossing: &'a CrossingMap,

    refs_to_young_gen: usize,
    in_old: bool,

    old_region: Region,
    young_region: Region,
}

impl<'a> Verifier<'a> {
    pub fn new(
        young: &'a YoungGen,
        old: &'a OldGen,
        card: &'a CardTable,
        crossing: &'a CrossingMap,
    ) -> Verifier<'a> {
        Verifier {
            young: young,
            old: old,
            card: card,
            crossing: crossing,

            refs_to_young_gen: 0,
            in_old: false,

            young_region: young.used_region(),
            old_region: old.used_region(),
        }
    }

    pub fn verify(&mut self) {
        self.verify_young();
        self.verify_old();
    }

    fn verify_young(&mut self) {
        let region = self.young_region.clone();
        self.verify_objects(region);
    }

    fn verify_old(&mut self) {
        let region = self.old_region.clone();
        self.in_old = true;
        self.verify_objects(region);
        self.in_old = false;
    }

    fn verify_objects(&mut self, region: Region) {
        let mut curr = region.start;
        self.refs_to_young_gen = 0;

        while curr < region.end {
            let object = unsafe { &mut *curr.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();
                self.verify_reference(child_ptr);
            });

            let next = curr.offset(object.size());

            if self.in_old && on_different_cards(curr, next) {
                self.verify_card(curr);
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

        let card_entry = self.card.get(curr_card);
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

    fn verify_reference(&mut self, obj: *mut Obj) {
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
        println!("found reference: {:x}", addr.to_usize());

        panic!("reference neither pointing into young nor old generation.");
    }
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

fn start_of_card(addr: Address) -> bool {
    (addr.to_usize() & (CARD_SIZE-1)) == addr.to_usize()
}