use parking_lot::MutexGuard;
use std::collections::HashSet;
use std::fmt;

use crate::gc::root::Slot;
use crate::gc::swiper::card::{CardEntry, CardTable};
use crate::gc::swiper::crossing::{CrossingEntry, CrossingMap};
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected};
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{on_different_cards, BasePage};
use crate::gc::swiper::{LargePage, ReadOnlySpace, RegularPage, Swiper, CARD_SIZE};
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
    swiper: &'a Swiper,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    large: &'a LargeSpace,
    readonly_space: &'a ReadOnlySpace,
    minimum_remset: Vec<Address>,

    heap: Region,

    phase: VerifierPhase,
}

impl<'a> Verifier<'a> {
    pub fn new(
        vm: &'a VM,
        swiper: &'a Swiper,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        rootset: &'a [Slot],
        large: &'a LargeSpace,
        readonly_space: &'a ReadOnlySpace,
        phase: VerifierPhase,
    ) -> Verifier<'a> {
        let old_protected = old.protected();

        Verifier {
            vm,
            swiper,
            young,
            old,
            old_protected,
            card_table,
            crossing_map,
            rootset,
            readonly_space,
            large,
            minimum_remset: Vec::new(),

            heap,

            phase,
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_heap();
        self.verify_remembered_set();
    }

    fn verify_heap(&mut self) {
        let mut survivor_seen = true;

        for page in self.young.to_pages() {
            assert!(page.is_young());
            assert!(!page.is_readonly());
            assert!(!page.is_large());

            if survivor_seen {
                if !page.is_survivor() {
                    survivor_seen = false;
                }
            } else {
                assert!(!page.is_survivor());
            }

            self.verify_page(page);
        }

        for page in self.old_protected.pages() {
            assert!(!page.is_young());
            assert!(!page.is_readonly());
            assert!(!page.is_survivor());
            assert!(!page.is_large());

            self.verify_page(page);
        }

        for page in self.readonly_space.pages() {
            assert!(!page.is_young());
            assert!(page.is_readonly());
            assert!(!page.is_survivor());
            assert!(!page.is_large());

            self.verify_page(page);
        }

        self.large.iterate_pages(|page| {
            assert!(!page.is_young());
            assert!(page.is_large());

            self.verify_large_page(page);
        });
    }

    fn verify_roots(&self) {
        let mut refs_to_young_gen = 0;
        for root in self.rootset {
            self.verify_slot(*root, Address::null(), &mut refs_to_young_gen);
        }
    }

    fn verify_remembered_set(&self) {
        if !self.vm.flags.object_write_barrier {
            return;
        }

        let remset = self.swiper.remset.read();

        if self.phase.is_post_full() {
            assert!(remset.is_empty());
        }

        let mut remset_as_set = HashSet::new();

        for &object in remset.iter() {
            assert!(!BasePage::from_address(object).is_young());
            remset_as_set.insert(object);
        }

        for object in &self.minimum_remset {
            assert!(remset_as_set.contains(object));
        }
    }

    fn verify_page(&mut self, page: RegularPage) {
        let region = page.object_area();
        let mut curr = region.start;
        let mut refs_to_young_gen = 0;
        assert!(region.start.is_card_aligned());
        assert!(region.end.is_page_aligned());
        assert!(!page.is_large());

        let in_old = !page.is_young() && !page.is_readonly();

        if in_old {
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
            let page_for_object = RegularPage::from_address(curr);
            assert_eq!(page, page_for_object);
            assert!(object_end <= page.end());

            if !object.is_filler(self.vm) {
                self.verify_object(page.as_base_page(), curr, &mut refs_to_young_gen);
            }

            if on_different_cards(curr, object_end) {
                if in_old {
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

    fn verify_large_page(&mut self, page: LargePage) {
        let mut refs_to_young_gen = 0;
        self.verify_object(
            page.as_base_page(),
            page.object_address(),
            &mut refs_to_young_gen,
        );
        self.verify_card(page.object_address(), refs_to_young_gen);
    }

    fn verify_object(
        &mut self,
        page: BasePage,
        object_address: Address,
        refs_to_young_gen: &mut usize,
    ) {
        let object = object_address.to_obj();
        assert!(object.header().metadata_fwdptr().is_null());
        assert_eq!(object.header().is_marked(), page.is_readonly());

        if self.phase.is_post_full() {
            assert!(!object.header().is_remembered());
        } else if page.is_young() {
            assert!(object.header().is_remembered());
        }

        let mut object_has_young_ref = false;

        object.visit_reference_fields(|child| {
            if self.verify_slot(child, object_address, refs_to_young_gen) {
                object_has_young_ref = true;
            }
        });

        if self.vm.flags.object_write_barrier && object_has_young_ref && !page.is_young() {
            assert!(object.header().is_remembered());
            self.minimum_remset.push(object_address);
        }
    }

    fn verify_card(&self, curr: Address, refs_to_young_gen: usize) {
        if self.vm.flags.object_write_barrier {
            return;
        }

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
        if (self.phase.is_pre() || self.phase.is_post_minor()) && expected_card_entry.is_clean() {
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

            panic!("card table entry wrong.");
        }

        assert_eq!(actual_card_entry, expected_card_entry);
    }

    fn verify_crossing(&self, old: Address, new: Address) {
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

    fn verify_slot(
        &self,
        slot: Slot,
        _container_obj: Address,
        refs_to_young_gen: &mut usize,
    ) -> bool {
        let referenced_object = slot.get();

        if referenced_object.is_null() {
            return false;
        }

        let page = BasePage::from_address(referenced_object);
        let is_young = page.is_young();

        let object = referenced_object.to_obj();

        // Verify that the address is the start of an object,
        // for this access its size.
        // To make sure this isn't optimized out by the compiler,
        // make sure that the size doesn't equal 1.
        assert!(object.size() != 1, "object size shouldn't be 1");

        if is_young {
            *refs_to_young_gen += 1;
        }

        is_young
    }
}
