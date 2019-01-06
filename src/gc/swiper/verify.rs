use ctxt::get_vm;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::swiper::CARD_SIZE;
use gc::{Address, Region};

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

    fn is_post_minor(self) -> bool {
        match self {
            VerifierPhase::PostMinor => true,
            _ => false,
        }
    }

    fn is_pre_full(self) -> bool {
        match self {
            VerifierPhase::PreFull => true,
            _ => false,
        }
    }
}

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    large: &'a LargeSpace,
    perm_space: &'a Space,

    refs_to_young_gen: usize,
    in_old: bool,
    in_large: bool,

    old_active: Region,
    young_total: Region,
    eden_active: Region,
    from_active: Region,
    to_active: Region,
    reserved_area: Region,

    phase: VerifierPhase,
}

impl<'a> Verifier<'a> {
    pub fn new(
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        large: &'a LargeSpace,
        perm_space: &'a Space,
        reserved_area: Region,
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

            eden_active: young.eden_active(),
            from_active: young.from_active(),
            to_active: young.to_active(),
            old_active: old.active(),
            young_total: young.total(),
            reserved_area: reserved_area,

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
        let region = self.young.eden_active();
        self.verify_objects(region, "young gen (eden)");

        let region = self.from_active.clone();
        self.verify_objects(region, "young gen (from)");

        let region = self.to_active.clone();
        if !self.phase.is_post_minor() && !self.phase.is_pre_full() {
            assert!(region.size() == 0, "to-space should be empty.");
        }
        self.verify_objects(region, "young gen (to)");
    }

    fn verify_old(&mut self) {
        let region = self.old_active.clone();
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
            self.verify_reference(*root, Address::null(), "root set");
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

            if object.header().vtblptr().is_null() {
                curr = curr.add_ptr(1);
                continue;
            }

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

        object.visit_reference_fields(|element| {
            if (self.in_old || self.in_large) && on_different_cards(curr, element.address()) {
                self.verify_card(curr);
                curr = element.address();
            }

            self.verify_reference(element, object_address, name);
        });

        let next = object_address.offset(object.size());

        if self.in_old && on_different_cards(object_address, next) {
            self.verify_crossing(object_address, next, true);
        }

        next
    }

    fn verify_object(&mut self, object: &mut Obj, curr: Address, name: &str) -> Address {
        object.visit_reference_fields(|child| {
            self.verify_reference(child, curr, name);
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
        // actually contain any references into the young generation. But it should never
        // be clean when there are actual references into the young generation.
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
                let expected = if offset_words > 0 {
                    CrossingEntry::LeadingRefs(offset_words)
                } else {
                    CrossingEntry::FirstObject(0)
                };
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

    fn verify_reference(&mut self, slot: Slot, container_obj: Address, name: &str) {
        let reference = slot.get();

        if reference.is_null() {
            return;
        }

        if self.old_active.contains(reference)
            || self.eden_active.contains(reference)
            || self.from_active.contains(reference)
            || self.perm_space.contains(reference)
            || self.large.contains(reference)
            || (self.to_active.contains(reference)
                && (self.phase.is_post_minor() || self.phase.is_pre_full()))
        {
            let object = reference.to_obj();

            // Verify that the address is the start of an object,
            // for this access its size.
            // To make sure this isn't optimized out by the compiler,
            // make sure that the size doesn't equal 1.
            assert!(object.size() != 1, "object size shouldn't be 1");

            if self.young_total.contains(reference) {
                self.refs_to_young_gen += 1;
            }

            return;
        }

        let perm_region = self.perm_space.used_region();

        println!(
            "PRM: {}; active: {} (size 0x{:x})",
            self.perm_space.total(),
            perm_region,
            perm_region.size(),
        );
        println!(
            "EDN: {}; active: {} (size 0x{:x})",
            self.young.eden_total(),
            self.eden_active,
            self.eden_active.size(),
        );
        println!(
            "FRM: {}; active: {} (size 0x{:x})",
            self.young.from_total(),
            self.from_active,
            self.from_active.size(),
        );
        println!(
            " TO: {}; active: {} (size 0x{:x})",
            self.young.to_total(),
            self.to_active,
            self.to_active.size(),
        );
        println!(
            "OLD: {}; active: {} (size 0x{:x})",
            self.old.total(),
            self.old_active,
            self.old_active.size(),
        );
        println!(
            "LRG: {}-{}",
            self.large.total().start,
            self.large.total().end
        );
        println!(
            "TTL: {}-{}",
            self.reserved_area.start, self.reserved_area.end
        );
        println!(
            "found invalid reference to {} in {} (at {}, in object {}).",
            reference,
            name,
            slot.address(),
            container_obj
        );

        if self.young.contains(reference)
            && !self.from_active.contains(reference)
            && !self.eden_active.contains(reference)
        {
            println!("reference points into young generation but not into the active semi-space.");
        }

        println!("try print object size and class:");

        let object = reference.to_obj();
        println!("\tsize {}", object.size());
        let cls = object.header().vtbl().class();
        println!("\tclass {}", cls.name(get_vm()));

        panic!("reference neither pointing into young nor old generation.");
    }
}
