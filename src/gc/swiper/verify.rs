use parking_lot::MutexGuard;
use std::fmt;

use ctxt::get_vm;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::old::{OldGen, OldGenProtected};
use gc::swiper::on_different_cards;
use gc::swiper::young::YoungGen;
use gc::swiper::{CARD_REFS, CARD_SIZE};
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

    fn is_post_full(self) -> bool {
        match self {
            VerifierPhase::PostFull => true,
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
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    rootset: &'a [Slot],
    large: &'a LargeSpace,
    perm_space: &'a Space,

    refs_to_young_gen: usize,
    in_old: bool,
    in_large: bool,

    young_total: Region,
    eden_active: Region,
    from_active: Region,
    to_active: Region,
    reserved_area: Region,

    phase: VerifierPhase,
    promotion_failed: bool,
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
        promotion_failed: bool,
    ) -> Verifier<'a> {
        let old_protected = old.protected();

        Verifier {
            young: young,
            old: old,
            old_protected: old_protected,
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
            young_total: young.total(),
            reserved_area: reserved_area,

            phase: phase,
            promotion_failed: promotion_failed,
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
        if !self.promotion_failed {
            assert!(region.empty(), "from-space should be empty.");
        }
        self.verify_objects(region, "young gen (from)");

        let region = self.to_active.clone();
        self.verify_objects(region, "young gen (to)");
    }

    fn verify_old(&mut self) {
        self.verify_mapped_regions();

        self.in_old = true;
        let old_regions = self.old_protected.regions.clone();
        let mut last = self.old.total().start;
        for old_region in old_regions {
            let region_total = old_region.total_region();
            assert_eq!(region_total.start, last);
            self.verify_objects(old_region.active_region(), "old gen");
            last = region_total.end;
        }
        assert_eq!(self.old.total().end, last);
        self.in_old = false;
    }

    fn verify_mapped_regions(&mut self) {
        let regions = self
            .old_protected
            .regions
            .iter()
            .map(|r| r.committed_region())
            .collect::<Vec<_>>();
        verify_mapped_regions(self.old.total(), &regions);
    }

    fn verify_large(&mut self) {
        self.in_large = true;
        self.large.visit_objects(|addr| {
            let object = addr.to_obj();
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
        let mut last_null = false;
        self.refs_to_young_gen = 0;

        while curr < region.end {
            let object = curr.to_mut_obj();

            if object.header().vtblptr().is_null() {
                assert!(
                    !self.in_large,
                    "large object space should not have null filler"
                );

                assert!(
                    !self.phase.is_post_full(),
                    "there should not be null fillers after full collection"
                );

                let next = curr.add_ptr(1);

                if self.in_old && on_different_cards(curr, next) {
                    self.verify_card(curr, region);
                    self.verify_crossing(curr, next, false);
                }

                assert!(
                    !last_null,
                    "there should not be nulls directly after each other"
                );

                curr = next;
                last_null = true;
                continue;
            } else {
                last_null = false;
            }

            if object.is_array_ref() {
                self.verify_array_ref(object, curr, region, name);
            } else {
                self.verify_object(object, curr, region, name);
            }

            curr = curr.offset(object.size());
        }

        assert!(curr == region.end, "object doesn't end at region end");

        if (self.in_old || self.in_large) && !self.card_table.is_aligned(curr) {
            self.verify_card(curr, region);
        }

        if self.in_old || self.in_large {
            assert!(self.refs_to_young_gen == 0, "variable should be cleared");
        }
    }

    fn verify_array_ref(
        &mut self,
        object: &mut Obj,
        object_address: Address,
        region: Region,
        name: &str,
    ) {
        let mut curr = object_address;

        object.visit_reference_fields(|element| {
            if (self.in_old || self.in_large) && on_different_cards(curr, element.address()) {
                self.verify_card(curr, region);
                curr = element.address();
            }

            self.verify_reference(element, object_address, name);
        });

        let next = object_address.offset(object.size());

        if (self.in_old || self.in_large) && on_different_cards(curr, next) {
            self.verify_card(curr, region);
        }

        if self.in_old && on_different_cards(object_address, next) {
            self.verify_crossing(object_address, next, true);
        }
    }

    fn verify_object(
        &mut self,
        object: &mut Obj,
        object_address: Address,
        region: Region,
        name: &str,
    ) {
        object.visit_reference_fields(|child| {
            self.verify_reference(child, object_address, name);
        });

        let next = object_address.offset(object.size());

        if (self.in_old || self.in_large) && on_different_cards(object_address, next) {
            self.verify_card(object_address, region);
        }

        if self.in_old && on_different_cards(object_address, next) {
            self.verify_crossing(object_address, next, false);
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

        if actual_card_entry != expected_card_entry {
            let card_text = match actual_card_entry {
                CardEntry::Dirty => "dirty",
                CardEntry::Clean => "clean",
            };

            let card_start = self.card_table.to_address(curr_card);
            let card_end = card_start.offset(CARD_SIZE);

            println!(
                "CARD: {} ({}-{}) is marked {} but has {} reference(s).",
                curr_card.to_usize(),
                card_start,
                card_end,
                card_text,
                self.refs_to_young_gen
            );

            panic!("card table entry wrong.");
        }

        assert!(actual_card_entry == expected_card_entry);

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
        let middle_start;

        if array_ref {
            crossing_middle = CrossingEntry::LeadingRefs(CARD_REFS as u8);

            if old.offset(offset_of_array_data() as usize) > old_card_end {
                let old_next = old_card_idx.to_usize() + 1;
                let crossing = self.crossing_map.get(old_next.into());
                let diff_words = old_card_end.offset_from(old) / mem::ptr_width_usize();
                assert!(
                    crossing == CrossingEntry::ArrayStart(diff_words as u8),
                    "array start crossing not correct."
                );

                middle_start = old_card_idx.to_usize() + 2;
            } else {
                middle_start = old_card_idx.to_usize() + 1;
            }

            if new_card_idx.to_usize() >= middle_start {
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
            middle_start = old_card_idx.to_usize() + 1;

            let crossing = self.crossing_map.get(new_card_idx);
            let expected = CrossingEntry::FirstObject(offset_words);
            assert!(crossing == expected, "crossing at end not correct.");
        }

        for c in middle_start..new_card_idx.to_usize() {
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

        if self.old_protected.contains_slow(reference)
            || self.eden_active.contains(reference)
            || self.to_active.contains(reference)
            || self.perm_space.contains(reference)
            || self.large.contains(reference)
            || (self.from_active.contains(reference) && self.promotion_failed)
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
        {
            println!(
                "OLD total: {} (size 0x{:x})",
                self.old.total(),
                self.old.total().size()
            );
            for old_region in &self.old_protected.regions {
                println!(
                    "OLD region: {}; active: {} (size 0x{:x})",
                    old_region.committed_region(),
                    old_region.active_region(),
                    old_region.active_size(),
                );
            }
        }

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
            "found invalid reference to {} in {} (at {}, in object {}) during {} phase.",
            reference,
            name,
            slot.address(),
            container_obj,
            self.phase,
        );

        if container_obj.is_non_null() {
            let object = container_obj.to_obj();
            let cls = object.header().vtbl().class();
            let size = object.size();
            println!("\tsource object of {} (size={})", cls.name(get_vm()), size);
        }

        if self.young.contains(reference)
            && !self.to_active.contains(reference)
            && !self.eden_active.contains(reference)
        {
            println!("reference points into young generation but not into the active semi-space.");

            if self.young.eden_total().contains(reference) {
                println!("\treference points into eden-space");
            }

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
        let cls = object.header().vtbl().class();
        println!("\tclass {}", cls.name(get_vm()));

        panic!("reference neither pointing into young nor old generation.");
    }
}

pub fn verify_mapped_regions(total: Region, regions: &[Region]) {
    memory::verify_mapped_regions(total, regions);
}

#[cfg(target_os = "linux")]
mod memory {
    use regex::Regex;
    use std::cmp;
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    use gc::{Address, Region};

    pub fn verify_mapped_regions(total: Region, regions: &[Region]) {
        let mappings = read_memory_mappings(total);

        verify_regions_are_writable(&mappings, regions);
        verify_non_accessible_mappings(&mappings, regions);
    }

    fn verify_regions_are_writable(mappings: &[MemoryMapping], regions: &[Region]) {
        for region in regions {
            if region.empty() {
                continue;
            }

            let mut found = false;

            for mapping in mappings {
                if !mapping.region.fully_contains(region) {
                    continue;
                }

                found = true;
                assert!(mapping.readable && mapping.writable && !mapping.executable);
                break;
            }

            assert!(found, "memory region is NOT writable");
        }
    }

    fn verify_non_accessible_mappings(mappings: &[MemoryMapping], regions: &[Region]) {
        for mapping in mappings {
            if mapping.readable {
                continue;
            }

            assert!(!mapping.writable && !mapping.executable);

            for region in regions {
                assert!(!mapping.region.overlaps(region));
            }
        }
    }

    struct MemoryMapping {
        region: Region,
        readable: bool,
        writable: bool,
        executable: bool,
    }

    fn read_memory_mappings(total: Region) -> Vec<MemoryMapping> {
        let f = File::open("/proc/self/maps").expect("opening /proc/self/maps failed");
        let file = BufReader::new(&f);

        let re = Regex::new(r"^([0-9a-f]+)-([0-9a-f]+) ([r\-])([w\-])([x\-])").unwrap();

        let mut mappings = Vec::new();

        for line in file.lines() {
            let l = line.unwrap();
            if let Some(matches) = re.captures(&l) {
                let start = matches.get(1).unwrap().as_str();
                let start = usize::from_str_radix(start, 16).unwrap();
                let start: Address = start.into();

                let end = matches.get(2).unwrap().as_str();
                let end = usize::from_str_radix(end, 16).unwrap();
                let end: Address = end.into();

                let readable = matches.get(3).unwrap().as_str() == "r";
                let writable = matches.get(4).unwrap().as_str() == "w";
                let executable = matches.get(5).unwrap().as_str() == "x";

                assert!(start <= end);

                if end <= total.start || start >= total.end {
                    continue;
                }

                let start = cmp::max(start, total.start);
                let end = cmp::min(end, total.end);
                let region = Region::new(start, end);

                mappings.push(MemoryMapping {
                    region: region,
                    readable: readable,
                    writable: writable,
                    executable: executable,
                });
            }
        }

        mappings
    }
}

#[cfg(not(target_os = "linux"))]
mod memory {
    use gc::Region;
    pub fn verify_mapped_regions(_total: Region, _regions: &[Region]) {}
}
