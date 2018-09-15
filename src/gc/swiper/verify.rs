use gc::root::IndirectObj;
use gc::space::Space;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::Region;
use gc::swiper::CARD_SIZE;
use gc::swiper::on_different_cards;
use gc::Address;

use mem;
use object::{offset_of_array_data, Obj};

#[derive(Copy, Clone)]
pub enum VerifierPhase {
    PreMinor,
    PostMinor,
    PreFull,
    PostFull,
}

impl VerifierPhase {
    fn is_pre(self) -> bool {
        match self {
            VerifierPhase::PreMinor => true,
            VerifierPhase::PostMinor => false,
            VerifierPhase::PreFull => true,
            VerifierPhase::PostFull => false,
        }
    }
}

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [IndirectObj],
    large: &'a LargeSpace,
    perm_space: &'a Space,

    refs_to_young_gen: usize,
    in_old: bool,
    in_large: bool,

    old_region: Region,
    young_region: Region,

    phase: VerifierPhase,
}

impl<'a> Verifier<'a> {
    pub fn new(
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [IndirectObj],
        large: &'a LargeSpace,
        perm_space: &'a Space,
        phase: VerifierPhase,
    ) -> Verifier<'a> {
        Verifier {
            young: young,
            old: old,
            card_table: card_table,
            crossing_map: crossing_map,
            rootset: rootset,
            perm_space: perm_space,
            large: large,

            refs_to_young_gen: 0,
            in_old: false,
            in_large: false,

            young_region: young.used_region(),
            old_region: old.used_region(),

            phase: phase,
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_young();
        self.verify_old();
        self.verify_large();
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

    fn verify_large(&mut self) {
        self.in_large = true;
        self.large.visit_objects(|addr| {
            let object = unsafe { &mut *addr.to_mut_ptr::<Obj>() };
            let region = Region::new(addr, addr.offset(object.size()));
            self.verify_objects(region, "large space");
        });
        self.in_large = false;
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
            assert!(self.card_table.is_aligned(curr));
            self.verify_crossing(curr, curr, false);
        }

        while curr < region.end {
            let object = unsafe { &mut *curr.to_mut_ptr::<Obj>() };

            let next = if object.is_array_ref() {
                self.verify_array_ref(object, curr, name)
            } else {
                self.verify_object(object, curr, name)
            };

            curr = next;
        }

        assert!(curr == region.end, "object doesn't end at region end");

        if (self.in_old || self.in_large) && !self.card_table.is_aligned(curr) {
            self.verify_card(curr);
        }
    }

    fn verify_array_ref(&mut self, object: &mut Obj, mut curr: Address, name: &str) -> Address {
        let object_address = curr;

        object.visit_reference_fields(|child| {
            let child_ptr = child.get();

            if (self.in_old || self.in_large) && on_different_cards(curr, child.to_address()) {
                self.verify_card(curr);
                curr = child.to_address();
            }

            self.verify_reference(child_ptr, child.to_address(), object_address, name);
        });

        let next = object_address.offset(object.size());

        if self.in_old && on_different_cards(object_address, next) {
            self.verify_crossing(object_address, next, true);
        }

        next
    }

    fn verify_object(&mut self, object: &mut Obj, curr: Address, name: &str) -> Address {
        object.visit_reference_fields(|child| {
            let child_ptr = child.get();
            self.verify_reference(child_ptr, child.to_address(), curr, name);
        });

        let next = curr.offset(object.size());

        if (self.in_old || self.in_large) && on_different_cards(curr, next) {
            self.verify_card(curr);
            if self.in_old {
                self.verify_crossing(curr, next, false);
            }
        }

        next
    }

    fn verify_card(&mut self, curr: Address) {
        let curr_card = self.card_table.card_idx(curr);

        let card_entry = self.card_table.get(curr_card);
        let expected_card_entry = if self.refs_to_young_gen > 0 {
            CardEntry::Dirty
        } else {
            CardEntry::Clean
        };

        // In the verify-phase before the collection the card's dirty-entry isn't
        // guaranteed to be exact. It could be `dirty` although this card doesn't
        // actually contain any references into the young generation. But it is never
        // clean when there are actual references into the young generation.
        if self.phase.is_pre() && expected_card_entry == CardEntry::Clean {
            self.refs_to_young_gen = 0;
            return;
        }

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

    fn verify_crossing(&mut self, old: Address, new: Address, array_ref: bool) {
        let new_card_idx = self.card_table.card_idx(new);
        let old_card_idx = self.card_table.card_idx(old);

        if new_card_idx == old_card_idx {
            return;
        }

        let new_card_start = self.card_table.to_address(new_card_idx);
        let old_card_end = self.card_table.to_address(old_card_idx).offset(CARD_SIZE);

        let offset = new.offset_from(new_card_start);
        let offset_words = (offset / mem::ptr_width_usize()) as u8;

        let crossing_middle;
        let loop_start;

        if array_ref {
            let refs_per_card = (CARD_SIZE / mem::ptr_width_usize()) as u8;

            crossing_middle = CrossingEntry::LeadingRefs(refs_per_card);

            if old.offset(offset_of_array_data() as usize) > old_card_end {
                let old_next = old_card_idx.to_usize() + 1;
                let crossing = self.crossing_map.get(old_next.into());
                let diff_words = old_card_end.offset_from(old) / mem::ptr_width_usize();
                assert!(crossing == CrossingEntry::ArrayStart(diff_words as u8));

                loop_start = old_card_idx.to_usize() + 2;
            } else {
                loop_start = old_card_idx.to_usize() + 1;
            }

            if new_card_idx.to_usize() >= loop_start {
                let crossing = self.crossing_map.get(new_card_idx);
                let expected = CrossingEntry::LeadingRefs(offset_words);
                assert!(crossing == expected, "array crossing at end not correct.");
            }
        } else {
            crossing_middle = CrossingEntry::NoRefs;
            loop_start = old_card_idx.to_usize() + 1;

            let crossing = self.crossing_map.get(new_card_idx);
            let expected = CrossingEntry::FirstObject(offset_words);
            assert!(crossing == expected, "crossing at end not correct.");
        }

        for c in loop_start..new_card_idx.to_usize() {
            assert!(
                self.crossing_map.get(c.into()) == crossing_middle,
                "middle crossing not correct."
            );
        }
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

        if self.old_region.contains(addr)
            || self.young_region.contains(addr)
            || self.perm_space.contains(addr)
            || self.large.contains(addr)
        {
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
            "YNG: {}-{}; active: {}-{}",
            self.young.total.start,
            self.young.total.end,
            self.young_region.start,
            self.young_region.end
        );
        println!(
            "OLD: {}-{}; active: {}-{}",
            self.old.total.start, self.old.total.end, self.old_region.start, self.old_region.end
        );
        println!(
            "found invalid reference to {} in {} (at {}, in object {}).",
            addr, name, ref_addr, obj_addr
        );

        if self.young.contains(addr) && !self.young_region.contains(addr) {
            println!("reference points into young generation but not into the active semi-space.");
        }

        panic!("reference neither pointing into young nor old generation.");
    }
}
