use parking_lot::MutexGuard;
use std::fmt;

use crate::gc::root::Slot;
use crate::gc::space::Space;
use crate::gc::swiper::card::{CardEntry, CardTable};
use crate::gc::swiper::crossing::{CrossingEntry, CrossingMap};
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected, Page};
use crate::gc::swiper::on_different_cards;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::CARD_SIZE;
use crate::gc::{Address, Region};

use crate::mem;
use crate::object::Obj;
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

    refs_to_young_gen: usize,
    in_old: bool,
    in_large: bool,

    young_total: Region,
    to_committed: Region,
    reserved_area: Region,
    init_old_top: Address,

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
        init_old_top: Address,
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

            refs_to_young_gen: 0,
            in_old: false,
            in_large: false,

            to_committed: young.to_committed(),
            young_total: young.total(),
            reserved_area,

            phase,
            init_old_top,
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_young();
        self.verify_old();
        self.verify_large();
    }

    fn verify_young(&mut self) {
        let mut curr = self.to_committed.start();
        while curr < self.to_committed.end() {
            let page = Page::from_address(curr);
            self.verify_region(page.object_area(), "young gen");
            curr = page.end();
        }
        assert_eq!(curr, self.to_committed.end());
    }

    fn verify_old(&mut self) {
        self.in_old = true;

        let pages = self.old_protected.pages.clone();
        let mut last = self.old.total_start();

        for page in pages {
            assert_eq!(last, page.start());
            self.verify_region(page.object_area(), "old gen");
            last = page.end();
        }

        self.in_old = false;
    }

    fn verify_large(&mut self) {
        self.in_large = true;
        self.large.visit_objects(|addr| {
            let object = addr.to_obj();
            let region = Region::new(addr, addr.offset(object.size()));
            self.verify_region(region, "large space");
        });
        self.in_large = false;
    }

    fn verify_roots(&mut self) {
        for root in self.rootset {
            self.verify_reference(*root, Address::null(), "root set");
        }
    }

    fn verify_region(&mut self, region: Region, name: &str) {
        let mut curr = region.start;
        self.refs_to_young_gen = 0;

        while curr < region.end {
            let object = curr.to_obj();
            let vtblptr = object.header().raw_vtblptr();

            if object.is_filler(self.vm) {
                assert!(!self.in_large, "large object space should not have fillers");

                let size = if vtblptr.is_null() {
                    mem::ptr_width_usize()
                } else {
                    object.size()
                };
                let object_end = curr.offset(size);

                if self.in_old && on_different_cards(curr, object_end) {
                    self.verify_card(curr, region);
                    self.verify_crossing(curr, object_end);
                }

                curr = object_end;
                continue;
            }

            self.verify_object(object, curr, region, name);

            let object_end = curr.offset(object.size());

            if !self.in_large {
                // Object is not supposed to cross page boundary.
                let page = Page::from_address(curr);
                assert!(object_end <= page.end());
            }

            curr = object_end;
        }

        assert!(curr == region.end, "object doesn't end at region end");

        if (self.in_old || self.in_large) && !self.card_table.is_aligned(curr) {
            self.verify_card(curr, region);
        }

        if self.in_old || self.in_large {
            assert!(self.refs_to_young_gen == 0, "variable should be cleared");
        }
    }

    fn verify_object(&mut self, object: &Obj, object_address: Address, region: Region, name: &str) {
        assert!(object.header().metadata_fwdptr().is_null());
        assert_eq!(object.header().is_old(), self.in_old);
        assert!(!object.header().is_marked());

        object.visit_reference_fields(|child| {
            self.verify_reference(child, object_address, name);
        });

        let next = object_address.offset(object.size());

        if (self.in_old || self.in_large) && on_different_cards(object_address, next) {
            self.verify_card(object_address, region);
        }

        if self.in_old && on_different_cards(object_address, next) {
            self.verify_crossing(object_address, next);
        }
    }

    fn verify_card(&mut self, curr: Address, region: Region) {
        let curr_card = self.card_table.card_idx(curr);

        let expected_card_entry = if self.refs_to_young_gen > 0 {
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
            self.refs_to_young_gen = 0;
            return;
        }

        // The first card in an old region can't be cleaned if it is not card aligned.
        // Therefore this card could be dirty when it should actually be clean, but this is
        // allowed. Nevertheless it shouldn't be clean when it actually contains references
        // into the young generation.
        if curr_card == self.card_table.card_idx(region.start)
            && !region.start.is_card_aligned()
            && expected_card_entry.is_clean()
        {
            self.refs_to_young_gen = 0;
            return;
        }

        // The last card in an old region can't be cleaned as well.
        // Therefore this card could be dirty when it should actually be clean.
        // However, it shouldn't be clean when it actually contains references into
        // the young generation.
        if curr_card == self.card_table.card_idx(region.end) && expected_card_entry.is_clean() {
            assert!(!region.end.is_card_aligned());
            self.refs_to_young_gen = 0;
            return;
        }

        // The last card of a region is not cleared, promoting objects during the minor
        // collection phase can move this card back in the heap.
        if self.phase.is_post_minor()
            && expected_card_entry.is_clean()
            && actual_card_entry.is_dirty()
        {
            if curr_card == self.card_table.card_idx(self.init_old_top) {
                self.refs_to_young_gen = 0;
                return;
            }
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
                self.refs_to_young_gen,
                self.phase,
            );

            println!("CARD is in region {}", region);
            println!("");

            self.dump_spaces();

            panic!("card table entry wrong.");
        }

        assert!(actual_card_entry == expected_card_entry);

        self.refs_to_young_gen = 0;
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

        assert_eq!(
            self.crossing_map.get(new_card_idx),
            CrossingEntry::FirstObject(offset_words),
            "crossing at end not correct."
        );

        for c in middle_start..new_card_idx.to_usize() {
            assert!(
                self.crossing_map.get(c.into()) == CrossingEntry::NoRefs,
                "middle crossing not correct."
            );
        }
    }

    fn verify_reference(&mut self, slot: Slot, container_obj: Address, name: &str) {
        let reference = slot.get();

        if reference.is_null() {
            return;
        }

        if self.old_protected.contains(reference)
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
                self.refs_to_young_gen += 1;
            }

            return;
        }

        self.dump_spaces();

        println!(
            "found invalid reference to {} in {} (at {}, in object {}) during {} phase.",
            reference,
            name,
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
