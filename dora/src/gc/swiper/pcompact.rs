use parking_lot::{Mutex, MutexGuard};
use scoped_threadpool::Pool;
use std::cmp;

use crate::gc::pmarking;
use crate::gc::root::Slot;
use crate::gc::space::Space;
use crate::gc::swiper::arena;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::compact::verify_marking;
use crate::gc::swiper::controller::FullCollectorPhases;
use crate::gc::swiper::crossing::{CrossingEntry, CrossingMap};
use crate::gc::swiper::large::{LargeAlloc, LargeSpace};
use crate::gc::swiper::old::{OldGen, OldGenProtected, OldGenRegion};
use crate::gc::swiper::verify::verify_mapped_regions;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{walk_region, walk_region_and_skip_garbage, CardIdx, CARD_REFS};
use crate::gc::{Address, GcReason, Region};
use crate::stdlib;
use crate::timer::Timer;
use crate::vm::{Trap, VM};

pub struct ParallelFullCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    old_total: Region,

    reason: GcReason,
    number_workers: usize,

    min_heap_size: usize,
    max_heap_size: usize,

    units: Vec<Unit>,
    young_units: Vec<Unit>,
    regions: Vec<CollectRegion>,

    phases: FullCollectorPhases,
}

impl<'a, 'ast> ParallelFullCollector<'a, 'ast> {
    pub fn new(
        vm: &'a VM<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        perm_space: &'a Space,
        rootset: &'a [Slot],
        reason: GcReason,
        number_workers: usize,
        min_heap_size: usize,
        max_heap_size: usize,
    ) -> ParallelFullCollector<'a, 'ast> {
        ParallelFullCollector {
            vm,
            heap,
            young,
            old,
            old_protected: old.protected(),
            large_space,
            rootset,
            card_table,
            crossing_map,
            perm_space,

            old_total: old.total(),

            reason,
            number_workers,

            min_heap_size,
            max_heap_size,

            units: Vec::new(),
            young_units: Vec::new(),
            regions: Vec::new(),

            phases: FullCollectorPhases::new(),
        }
    }

    pub fn phases(&self) -> FullCollectorPhases {
        self.phases.clone()
    }

    pub fn collect(&mut self, pool: &mut Pool) {
        let dev_verbose = self.vm.args.flag_gc_dev_verbose;
        let stats = self.vm.args.flag_gc_stats;

        let mut timer = Timer::new(stats);

        if dev_verbose {
            println!("Full GC: Start");
        }

        self.mark_live(pool);

        if stats {
            let duration = timer.stop();
            self.phases.marking = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 1 (marking)");
        }

        if self.vm.args.flag_gc_verify {
            verify_marking(
                self.young,
                &*self.old_protected,
                self.large_space,
                self.heap,
            );

            if stats {
                timer.stop();
            }

            if dev_verbose {
                println!("Full GC: Phase 1b (verify marking)");
            }
        }

        self.compute_forward(pool);

        if stats {
            let duration = timer.stop();
            self.phases.compute_forward = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 2 (compute forward)");
        }

        self.update_references(pool);

        if stats {
            let duration = timer.stop();
            self.phases.update_refs = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }

        self.relocate_and_reset_cards(pool);

        if stats {
            let duration = timer.stop();
            self.phases.relocate = duration;
        }

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.young.clear();
        self.young.protect_from();

        let regions = self.compute_old_regions();
        self.old_protected.update_regions(regions);
    }

    fn compute_old_regions(&mut self) -> Vec<OldGenRegion> {
        let mut old_regions = Vec::new();
        let even_regions = (0..self.regions.len()).step_by(2);

        for idx in even_regions {
            let r = &self.regions[idx];
            assert!(r.slide_start);

            let mut total = r.object();
            let top = r.object_end();

            let mut total_mapping = r.mapping;
            let mapping_top = r.mapping.end;

            if idx > 0 {
                let prev = &self.regions[idx - 1];
                assert!(!prev.slide_start);
                assert!(prev.object_end() == total.start);
                total.start = prev.object_start();

                assert!(total_mapping.start == prev.mapping.end);
                total_mapping.start = prev.mapping.start;
            }

            if idx + 1 < self.regions.len() {
                let next = &self.regions[idx + 1];
                assert!(!next.slide_start);
                total.end = next.object_start();
                total_mapping.end = next.mapping.start;
            } else {
                total.end = self.old_total.end;
                total_mapping.end = self.old_total.end;
            }

            old_regions.push(OldGenRegion::new(total, top, total_mapping, mapping_top));
        }

        if self.regions.len() % 2 == 0 {
            if let Some(last) = self.regions.last() {
                let total = Region::new(last.young_start(), self.old_total.end);
                let top = last.compact.end;
                let total_mapping = Region::new(last.mapping.start, self.old_total.end);
                let mapping_top = last.mapping.end;

                old_regions.push(OldGenRegion::new(total, top, total_mapping, mapping_top));
            }
        }

        old_regions
    }

    fn mark_live(&mut self, pool: &mut Pool) {
        pmarking::start(
            self.rootset,
            self.heap.clone(),
            self.perm_space.total(),
            pool,
        );
    }

    fn compute_forward(&mut self, pool: &mut Pool) {
        self.compute_units();
        self.compute_live_bytes(pool);
        self.compute_collect_regions();
        self.place_young_units();

        if self.vm.args.flag_gc_verify {
            self.check_units_disjoint();
            self.check_regions_disjoint();
        }

        let regions: Vec<Region> = self.regions.iter().map(|r| r.mapping).collect();

        if !self.fits_into_heap(&regions) {
            stdlib::trap(Trap::OOM.int());
        }

        self.compute_actual_forward(pool);
        self.old_protected.commit_regions(&regions);

        if self.vm.args.flag_gc_verify {
            self.verify_mapped_regions(&regions);
        }
    }

    fn verify_mapped_regions(&mut self, regions: &[Region]) {
        let mut new_regions = regions.to_vec();
        let old_regions = self
            .old_protected
            .regions
            .iter()
            .map(|r| r.committed_region())
            .collect::<Vec<_>>();
        let mut all_regions = old_regions;
        all_regions.append(&mut new_regions);

        verify_mapped_regions(self.old.total(), &all_regions);
    }

    fn fits_into_heap(&mut self, regions: &[Region]) -> bool {
        let (eden_size, semi_size) = self.young.committed_size();
        let young_size = eden_size + semi_size;
        let old_size: usize = regions.iter().map(|r| r.size()).sum();
        let large_size = self.large_space.committed_size();

        (young_size + old_size + large_size) <= self.max_heap_size
    }

    fn check_units_disjoint(&self) {
        let mut last = self.old_total.start;

        for unit in &self.units {
            assert!(last <= unit.region.start);
            last = unit.region.end;
        }
    }

    fn check_regions_disjoint(&self) {
        let mut last_region = 0;
        let mut last_mapping_end = self.old_total.start;
        let mut last_span_end = self.old_total.start;

        for region in &self.regions {
            assert!(last_region == region.start_idx);
            assert!(last_mapping_end <= region.mapping.start);
            assert_eq!(last_span_end, region.span.start);
            assert!(region.span.fully_contains(&region.object()));

            last_region = region.start_idx + region.units;
            last_mapping_end = region.mapping.end;
            last_span_end = region.span.end;
        }

        if let Some(last) = self.regions.last() {
            assert_eq!(self.units.len(), last.start_idx + last.units);
            assert_eq!(last_span_end, self.old_total.end);
        }
    }

    fn compute_units(&mut self) {
        let active = self.old_protected.active_size();
        let unit_size = active / (8 * self.number_workers);

        let old_regions = self.old_protected.regions.len();
        for idx in 0..old_regions {
            let active = self.old_protected.regions[idx].active_region();
            self.units_for_old_region(active, unit_size);
        }

        let eden = self.young.eden_active();
        self.young_units.push(Unit::young(eden));

        let from = self.young.from_active();
        self.young_units.push(Unit::young(from));

        let to = self.young.to_active();
        self.young_units.push(Unit::young(to));
    }

    fn units_for_old_region(&mut self, region: Region, unit_size: usize) {
        let mut last = region.start;

        while last < region.end {
            let end = self.find_object_start(last, unit_size, region.end);
            debug_assert!(end <= region.end);

            let region = Region::new(last, end);
            self.units.push(Unit::old(region));
            last = end;
        }

        assert_eq!(last, region.end);
    }

    fn find_object_start(&mut self, last: Address, unit_size: usize, end: Address) -> Address {
        let ptr = last.offset(unit_size);

        if ptr >= end {
            return end;
        }

        let (card_start, card_end) = self.card_table.card_indices(ptr, end);
        let card_start = cmp::max(card_start, self.card_table.card_idx(last).to_usize() + 1);

        for card in card_start..card_end {
            let card: CardIdx = card.into();

            let crossing_entry = self.crossing_map.get(card);
            let card_start = self.card_table.to_address(card);

            match crossing_entry {
                CrossingEntry::NoRefs => {}
                CrossingEntry::LeadingRefs(refs) => {
                    if (refs as usize) < CARD_REFS {
                        return card_start.add_ptr(refs as usize);
                    }
                }

                CrossingEntry::FirstObject(offset) => {
                    return card_start.add_ptr(offset as usize);
                }

                CrossingEntry::ArrayStart(offset) => {
                    return card_start.sub_ptr(offset as usize);
                }
            }
        }

        end
    }

    fn compute_live_bytes(&mut self, pool: &mut Pool) {
        pool.scoped(|scoped| {
            let vm = self.vm;

            for unit in &mut self.units {
                scoped.execute(move || {
                    compute_live_bytes_in_unit(vm, unit);
                });
            }

            for unit in &mut self.young_units {
                scoped.execute(move || {
                    compute_live_bytes_in_unit(vm, unit);
                });
            }
        });
    }

    fn compute_collect_regions(&mut self) {
        let live: usize = self.units.iter().map(|u| u.live).sum();
        let number_regions = self.number_workers;

        let region_size = ((live as f64 / number_regions as f64) * 0.90f64) as usize;

        let mut regions = Vec::new();
        let mut start = 0;
        let mut size = 0;
        let mut last_span_end = self.old_total.start;
        let mut last_mapping_end = self.old_total.start;

        for (id, unit) in self.units.iter().enumerate() {
            size += unit.live;

            if size > region_size {
                self.add_collect_region(
                    start,
                    id,
                    &mut size,
                    &mut last_span_end,
                    &mut last_mapping_end,
                    &mut regions,
                );
                start = id + 1;
            }
        }

        if start < self.units.len() {
            let end = self.units.len() - 1;
            self.add_collect_region(
                start,
                end,
                &mut size,
                &mut last_span_end,
                &mut last_mapping_end,
                &mut regions,
            );
        }

        if regions.is_empty() {
            let object = self.old_total;
            let empty = Region::new(self.old_total.start, self.old_total.start);
            let compact = empty;
            let mapping = empty;
            regions.push(CollectRegion::new(0, 0, true, object, compact, mapping));
        }

        if let Some(last) = regions.last_mut() {
            if last.compact.end > self.old_total.end {
                panic!("OOM");
            }

            last.span.end = self.old_total.end;
        } else {
            unreachable!();
        }

        std::mem::replace(&mut self.regions, regions);
    }

    fn place_young_units(&mut self) {
        let young_units = std::mem::replace(&mut self.young_units, Vec::new());

        for unit in young_units {
            self.place_young_unit(unit);
        }
    }

    fn place_young_unit(&mut self, unit: Unit) {
        let live = unit.live;

        if live == 0 {
            return;
        }

        for region_idx in 0..self.regions.len() {
            let slide_start;
            let young_start;
            let young_end;
            let span_start;
            let span_end;

            let mapping_limit;

            {
                let region = &self.regions[region_idx];
                slide_start = region.slide_start;
                young_start = region.young_start();
                young_end = region.young_end();
                span_start = region.span.start;
                span_end = region.span.end;

                mapping_limit = if slide_start {
                    if region_idx + 1 < self.regions.len() {
                        let next = &self.regions[region_idx + 1];
                        next.mapping.start
                    } else {
                        self.old_total.end
                    }
                } else {
                    if region_idx > 0 {
                        let prev = &self.regions[region_idx - 1];
                        prev.mapping.end
                    } else {
                        self.old_total.start
                    }
                };
            }

            if slide_start {
                let new = young_end.offset(live);

                if new <= span_end {
                    let region = &mut self.regions[region_idx];
                    region.young_units.push(unit);
                    region.young_live += live;
                    region.mapping.end = cmp::min(new.align_page(), mapping_limit);
                    assert!(region.span.fully_contains(&region.object()));
                    return;
                }
            } else {
                let new = young_start.sub(live);

                if new >= span_start {
                    let region = &mut self.regions[region_idx];
                    region.young_units.push(unit);
                    region.young_live += live;
                    region.mapping.start = cmp::max(new.align_page_down(), mapping_limit);
                    assert!(region.span.fully_contains(&region.object()));
                    return;
                }
            }
        }

        panic!("OOM: no space for young found!");
    }

    fn add_collect_region(
        &self,
        unit_start_idx: usize,
        unit_end_idx: usize,
        size: &mut usize,
        last_span_end: &mut Address,
        last_mapping_end: &mut Address,
        regions: &mut Vec<CollectRegion>,
    ) {
        let slide_start = regions.len() % 2 == 0;
        let span_start = *last_span_end;

        let unit_end = &self.units[unit_end_idx];
        let span_end = unit_end.region.end;

        let (compact_start, compact_end) = if slide_start {
            (span_start, span_start.offset(*size))
        } else {
            (span_end.sub(*size), span_end)
        };

        let mapping_start = cmp::max(compact_start.align_page_down(), *last_mapping_end);
        let mapping_end = cmp::max(compact_end.align_page(), mapping_start);

        let span = Region::new(span_start, span_end);
        let compact = Region::new(compact_start, compact_end);
        let mapping = Region::new(mapping_start, mapping_end);

        regions.push(CollectRegion::new(
            unit_start_idx,
            unit_end_idx - unit_start_idx + 1,
            slide_start,
            span,
            compact,
            mapping.clone(),
        ));

        *size = 0;
        *last_span_end = span.end;
        *last_mapping_end = mapping.end;
    }

    fn compute_actual_forward(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            let pfull = &self;

            for region in &self.regions {
                let mut fwd = region.compact.start;
                for unit in &self.units[region.start_idx..region.start_idx + region.units] {
                    scope.execute(move || {
                        pfull.compute_forward_unit(unit, fwd);
                    });
                    fwd = fwd.offset(unit.live);
                }
                assert_eq!(fwd, region.compact.end);

                let mut fwd = region.young_start();
                for unit in &region.young_units {
                    scope.execute(move || {
                        pfull.compute_forward_unit(unit, fwd);
                    });
                    fwd = fwd.offset(unit.live);
                }
                assert_eq!(fwd, region.young_end());
            }
        });
    }

    fn compute_forward_unit(&self, unit: &Unit, mut fwd: Address) {
        if unit.live == 0 {
            return;
        }

        walk_region(unit.region, |obj, _address, size| {
            if obj.header().is_marked_non_atomic() {
                obj.header_mut().set_fwdptr_non_atomic(fwd);
                fwd = fwd.offset(size);
            }
        });
    }

    fn update_references(&mut self, pool: &mut Pool) {
        let next_large = Mutex::new(Address::null());
        let next_large = &next_large;

        pool.scoped(|scope| {
            let rootset = self.rootset;
            let pfull = &self;

            scope.execute(move || {
                for root in rootset {
                    pfull.forward_reference(*root);
                }
            });

            for unit in &self.units {
                scope.execute(move || {
                    pfull.update_references_unit(unit);
                });
            }

            for region in &self.regions {
                for unit in &region.young_units {
                    scope.execute(move || {
                        pfull.update_references_unit(unit);
                    });
                }
            }

            let head = self.large_space.remove_head();

            if head.is_null() {
                return;
            }

            *next_large.lock() = head;

            for _ in 0..self.number_workers {
                let pfull = &self;
                let card_table = self.card_table;
                let large = self.large_space;

                scope.execute(move || {
                    let mut head = Address::null();
                    let mut tail = Address::null();
                    let mut free_regions = Vec::new();
                    let mut freed = 0;

                    let mut addr = next(next_large);

                    while let Some(large_alloc) = addr {
                        let large_alloc = LargeAlloc::from_address(large_alloc);
                        let object_start = large_alloc.object_address();
                        let object = object_start.to_mut_obj();

                        // reset cards for object, also do this for dead objects
                        // to reset card entries to clean.
                        if object.is_array_ref() {
                            let object_end = object_start.offset(object.size());
                            card_table.reset_region(object_start, object_end);
                        } else {
                            card_table.reset_addr(object_start);
                        }

                        addr = next(next_large);

                        if object.header().is_marked_non_atomic() {
                            object.visit_reference_fields(|field| {
                                pfull.forward_reference(field);
                            });

                            if head.is_null() {
                                head = large_alloc.address();
                            }

                            if tail.is_non_null() {
                                let tail = LargeAlloc::from_address(tail);
                                tail.next = large_alloc.address();
                            }

                            large_alloc.prev = tail;
                            large_alloc.next = Address::null();

                            // unmark object for next collection
                            object.header_mut().unmark_non_atomic();

                            tail = large_alloc.address();
                        } else {
                            let free_start = large_alloc.address();
                            let free_size = large_alloc.size;

                            arena::discard(free_start, free_size);
                            free_regions.push(free_start.region_start(free_size));
                            freed += free_size;
                        }
                    }

                    if head.is_non_null() {
                        large.append_chain(head, tail, freed, free_regions);
                    }
                });
            }
        });
    }

    fn update_references_unit(&self, unit: &Unit) {
        walk_region(unit.region, |obj, _address, _size| {
            if obj.header().is_marked_non_atomic() {
                obj.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });
            }
        });
    }

    fn forward_reference(&self, slot: Slot) {
        let object_addr = slot.get();

        if self.heap.contains(object_addr) {
            if self.large_space.contains(object_addr) {
                return;
            }

            // Do not check mark-bit for large objects. Mark-bit clearing for large
            // objects overlaps with reference forwarding.
            debug_assert!(object_addr.to_obj().header().is_marked_non_atomic());

            let fwd_addr = object_addr.to_obj().header().fwdptr_non_atomic();
            debug_assert!(self.old_total.contains(fwd_addr));
            slot.set(fwd_addr);
        } else {
            debug_assert!(object_addr.is_null() || self.perm_space.contains(object_addr));
        }
    }

    fn relocate_and_reset_cards(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            for region in &self.regions {
                let units = &self.units;
                let pfull = &self;

                scope.execute(move || {
                    let units_in_region = &units[region.start_idx..region.start_idx + region.units];

                    if region.slide_start {
                        for unit in units_in_region {
                            pfull.relocate_unit(unit);
                        }
                    } else {
                        for unit in units_in_region.iter().rev() {
                            if unit.live == 0 {
                                continue;
                            }

                            let mut objects = Vec::new();

                            walk_region(unit.region, |object, address, _| {
                                if object.header().is_marked_non_atomic() {
                                    objects.push(address);
                                }
                            });

                            while let Some(object) = objects.pop() {
                                let size = object.to_obj().size() as usize;
                                pfull.relocate_object(object, size);
                            }
                        }
                    }

                    for unit in &region.young_units {
                        pfull.relocate_unit(unit);
                    }
                });
            }

            let regions = self
                .old_protected
                .regions
                .iter()
                .map(|r| (r.start(), r.top()))
                .collect::<Vec<_>>();

            for (start, end) in regions {
                let card_table = self.card_table;

                scope.execute(move || {
                    card_table.reset_region(start, end);
                });
            }
        });
    }

    fn relocate_unit(&self, unit: &Unit) {
        if unit.live == 0 {
            return;
        }

        walk_region(unit.region, |_, object, size| {
            self.relocate_object(object, size);
        });
    }

    fn relocate_object(&self, address: Address, object_size: usize) {
        let object = address.to_obj();

        if object.header().is_marked_non_atomic() {
            // get new location
            let dest = object.header().fwdptr_non_atomic();

            // determine location after relocated object
            let next_dest = dest.offset(object_size);

            if address != dest {
                object.copy_to(dest, object_size);
            }

            // unmark object for next collection
            let dest_obj = dest.to_mut_obj();
            dest_obj.header_mut().unmark_non_atomic();

            self.old
                .update_crossing(dest, next_dest, dest_obj.is_array_ref());
        }
    }
}

fn compute_live_bytes_in_unit(vm: &VM, unit: &mut Unit) {
    let mut live = 0;

    walk_region_and_skip_garbage(vm, unit.region, |obj, _addr, size| {
        if obj.header().is_marked_non_atomic() {
            live += size;
            true
        } else {
            false
        }
    });

    unit.live = live;
}

fn next(next_large: &Mutex<Address>) -> Option<Address> {
    let mut next_large = next_large.lock();

    if next_large.is_null() {
        return None;
    }

    let large_alloc = LargeAlloc::from_address(*next_large);
    *next_large = large_alloc.next;

    Some(large_alloc.address())
}

#[derive(Clone)]
struct Unit {
    region: Region,
    live: usize,
    young: bool,
}

impl Unit {
    fn old(region: Region) -> Unit {
        Unit {
            region,
            live: 0,
            young: false,
        }
    }

    fn young(region: Region) -> Unit {
        Unit {
            region,
            live: 0,
            young: true,
        }
    }

    fn size(&self) -> usize {
        self.region.size()
    }
}

struct CollectRegion {
    start_idx: usize,
    units: usize,

    young_units: Vec<Unit>,
    young_live: usize,

    // true when objects should be relocated to start of region and
    // false when objects should be relocated towards end.
    slide_start: bool,

    // Maximum span of this region. The region
    // is not allowed to become bigger than that: otherwise
    // data from other regions would be overwritten.
    span: Region,

    // compaction region
    compact: Region,

    // Page boundaries around the compacted area
    mapping: Region,
}

impl CollectRegion {
    fn new(
        start_idx: usize,
        units: usize,
        slide_start: bool,
        span: Region,
        compact: Region,
        mapping: Region,
    ) -> CollectRegion {
        CollectRegion {
            start_idx,
            units,
            young_units: Vec::new(),
            young_live: 0,
            slide_start,
            span,
            compact,
            mapping,
        }
    }

    fn object(&self) -> Region {
        Region::new(self.object_start(), self.object_end())
    }

    fn object_start(&self) -> Address {
        if self.slide_start {
            self.compact.start
        } else {
            self.young_start()
        }
    }

    fn object_end(&self) -> Address {
        if self.slide_start {
            self.young_end()
        } else {
            self.compact.end
        }
    }

    fn young_start(&self) -> Address {
        if self.slide_start {
            self.compact.end
        } else {
            self.compact.start.sub(self.young_live)
        }
    }

    fn young_end(&self) -> Address {
        if self.slide_start {
            self.compact.end.offset(self.young_live)
        } else {
            self.compact.start
        }
    }

    fn live(&self) -> usize {
        self.compact.size()
    }
}
