use parking_lot::MutexGuard;
use std::fmt;

use crate::gc::root::Slot;
use crate::gc::space::Space;
use crate::gc::swiper::card::{CardEntry, CardTable};
use crate::gc::swiper::crossing::{CrossingEntry, CrossingMap};
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected};
use crate::gc::swiper::on_different_cards;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{Page, CARD_SIZE};
use crate::gc::{Address, Region};

use crate::mem;
use crate::vm::VM;

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

    fn is_post_full(self) -> bool {
        match self {
            VerifierPhase::PostFull => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn is_pre_full(self) -> bool {
        match self {
            VerifierPhase::PreFull => true,
            _ => false,
        }
    }
}

impl fmt::Display for VerifierPhase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            VerifierPhase::PreMinor => "pre minor",
            VerifierPhase::PostMinor => "post minor",
            VerifierPhase::PreFull => "pre full",
            VerifierPhase::PostFull => "post full",
        };

        write!(f, "{}", text)
    }
}

pub struct Verifier<'a> {
    vm: &'a VM,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    large: &'a LargeSpace,
    readonly_space: &'a Space,

    in_old: bool,

    old_total: Region,
    young_total: Region,
    to_committed: Region,
    reserved_area: Region,

    phase: VerifierPhase,
}

impl<'a> Verifier<'a> {
    pub fn new(
        vm: &'a VM,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        large: &'a LargeSpace,
        readonly_space: &'a Space,
        reserved_area: Region,
        phase: VerifierPhase,
    ) -> Verifier<'a> {
        let old_protected = old.protected();

        Verifier {
            vm,
            young,
            old,
            old_protected,
            card_table,
            crossing_map,
            rootset,
            readonly_space,
            large,

            in_old: false,

            old_total: old.total(),
            to_committed: young.to_committed(),
            young_total: young.total(),
            reserved_area,

            phase,
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_heap();
    }

    fn verify_heap(&mut self) {
        assert!(!self.in_old);
        for page in self.young.pages() {
            self.verify_page(page);
        }

        self.in_old = true;
        for page in self.old_protected.pages() {
            self.verify_page(page);
        }
        self.in_old = false;

        self.large.visit_objects(|object_address| {
            self.verify_large_page(object_address);
        });
    }

    fn verify_roots(&mut self) {
        let mut refs_to_young_gen = 0;
        for root in self.rootset {
            self.verify_reference(*root, Address::null(), &mut refs_to_young_gen);
        }
    }

    fn verify_page(&mut self, page: Page) {
        let region = page.object_area();
        let mut curr = region.start;
        let mut refs_to_young_gen = 0;
        assert!(region.start.is_card_aligned());
        assert!(region.end.is_page_aligned());

        if self.in_old {
            let card_idx = self.card_table.card_idx(region.start);
            assert_eq!(
                self.crossing_map.get(card_idx),
                CrossingEntry::FirstObject(0),
            );
        }

        while curr < region.end {
            let object = curr.to_obj();
            let size = object.size();
            let object_end = curr.offset(size);

            // Object is not supposed to cross page boundary.
            let page = Page::from_address(curr);
            assert!(object_end <= page.end());

            if !object.is_filler(self.vm) {
                self.verify_object(curr, &mut refs_to_young_gen);
            }

            if on_different_cards(curr, object_end) {
                if self.in_old {
                    self.verify_card(curr, refs_to_young_gen);

                    if object_end < region.end {
                        self.verify_crossing(curr, object_end);
                    }
                }

                refs_to_young_gen = 0;
            }

            curr = object_end;
        }

        assert!(curr == region.end, "object doesn't end at region end");
        assert_eq!(refs_to_young_gen, 0);
    }

    fn verify_large_page(&mut self, object_address: Address) {
        let mut refs_to_young_gen = 0;
        self.verify_object(object_address, &mut refs_to_young_gen);
        self.verify_card(object_address, refs_to_young_gen);
    }

    fn verify_object(&mut self, object_address: Address, refs_to_young_gen: &mut usize) {
        let object = object_address.to_obj();
        assert!(object.header().metadata_fwdptr().is_null());
        assert_eq!(object.header().is_old(), self.in_old);
        assert!(!object.header().is_marked());

        object.visit_reference_fields(|child| {
            self.verify_reference(child, object_address, refs_to_young_gen);
        });
    }

    fn verify_card(&mut self, curr: Address, refs_to_young_gen: usize) {
        let curr_card = self.card_table.card_idx(curr);

        let expected_card_entry = if refs_to_young_gen > 0 {
            // full collections promote everything into old gen
            // young gen should be empty!
            assert!(!self.phase.is_post_full());

            CardEntry::Dirty
        } else {
            CardEntry::Clean
        };

        let actual_card_entry = self.card_table.get(curr_card);

        // In the verify-phase before the collection the card's dirty-entry isn't
        // guaranteed to be exact. It could be `dirty` although this card doesn't
        // actually contain any references into the young generation. But it should never
        // be clean when there are actual references into the young generation.
        if self.phase.is_pre() && expected_card_entry.is_clean() {
            return;
        }

        if actual_card_entry != expected_card_entry {
            let card_text = match actual_card_entry {
                CardEntry::Dirty => "dirty",
                CardEntry::Clean => "clean",
            };

            let card_start = self.card_table.to_address(curr_card);
            let card_end = card_start.offset(CARD_SIZE);

            println!(
                "CARD: {} ({}-{}) is marked {} but has {} reference(s) in phase {}.",
                curr_card.to_usize(),
                card_start,
                card_end,
                card_text,
                refs_to_young_gen,
                self.phase,
            );

            self.dump_spaces();

            panic!("card table entry wrong.");
        }

        assert_eq!(actual_card_entry, expected_card_entry);
    }

    fn verify_crossing(&mut self, old: Address, new: Address) {
        let new_card_idx = self.card_table.card_idx(new);
        let old_card_idx = self.card_table.card_idx(old);

        if new_card_idx == old_card_idx {
            return;
        }

        let new_card_start = self.card_table.to_address(new_card_idx);

        let offset = new.offset_from(new_card_start);
        let offset_words = (offset / mem::ptr_width_usize()) as u8;

        let middle_start = old_card_idx.to_usize() + 1;
        let actual = self.crossing_map.get(new_card_idx);
        let expected = CrossingEntry::FirstObject(offset_words);

        if actual != expected {
            println!("actual = {:?}", actual);
            println!("expected = {:?}", expected);
        }

        assert_eq!(expected, actual, "crossing at end not correct.");

        for c in middle_start..new_card_idx.to_usize() {
            assert!(
                self.crossing_map.get(c.into()) == CrossingEntry::NoRefs,
                "middle crossing not correct."
            );
        }
    }

    fn verify_reference(
        &mut self,
        slot: Slot,
        container_obj: Address,
        refs_to_young_gen: &mut usize,
    ) {
        let reference = slot.get();

        if reference.is_null() {
            return;
        }

        if self.old_total.contains(reference)
            || self.to_committed.contains(reference)
            || self.readonly_space.contains(reference)
            || self.large.contains(reference)
        {
            let object = reference.to_obj();

            // Verify that the address is the start of an object,
            // for this access its size.
            // To make sure this isn't optimized out by the compiler,
            // make sure that the size doesn't equal 1.
            assert!(object.size() != 1, "object size shouldn't be 1");

            if self.young_total.contains(reference) {
                *refs_to_young_gen += 1;
            }

            return;
        }

        self.dump_spaces();

        println!(
            "found invalid reference to {} (at {}, in object {}) during {} phase.",
            reference,
            slot.address(),
            container_obj,
            self.phase,
        );

        if container_obj.is_non_null() {
            let object = container_obj.to_obj();
            let cls = object.header().vtbl().class_instance();
            let size = object.size();
            println!("\tsource object of {:?} (size={})", cls.kind, size);
        }

        if self.young.contains(reference) && !self.to_committed.contains(reference) {
            println!("reference points into young generation but not into the active semi-space.");

            if self.young.from_total().contains(reference) {
                println!("\treference points into from-space");
            }

            if self.young.to_total().contains(reference) {
                println!("\treference points into to-space");
            }
        }

        println!("try print target object size and class:");

        let object = reference.to_obj();
        println!("\tsize {}", object.size());
        let cls = object.header().vtbl().class_instance();
        println!("\tclass {:?}", cls.kind);

        panic!("reference neither pointing into young nor old generation.");
    }

    fn dump_spaces(&self) {
        let perm_region = self.readonly_space.used_region();

        println!(
            " RO: {}; active: {} (size 0x{:x})",
            self.readonly_space.total(),
            perm_region,
            perm_region.size(),
        );
        println!(
            " TO: {}; active: {} (size 0x{:x})",
            self.young.to_total(),
            self.to_committed,
            self.to_committed.size(),
        );
        println!(
            "OLD total: {}; (size 0x{:x})",
            self.old.total(),
            self.old.total().size()
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
    }
}
