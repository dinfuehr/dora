use parking_lot::{Mutex, MutexGuard};
use scoped_threadpool::Pool;
use std::cmp;

use ctxt::VM;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::arena;
use gc::swiper::card::CardTable;
use gc::swiper::controller::FullCollectorPhases;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::full::verify_marking;
use gc::swiper::large::{LargeAlloc, LargeSpace};
use gc::swiper::marking;
use gc::swiper::old::{OldGen, OldGenProtected, OldRegion};
use gc::swiper::young::YoungGen;
use gc::swiper::{walk_region, walk_region_and_skip_garbage, CardIdx, CARD_REFS};
use gc::{Address, GcReason, Region};
use timer::Timer;

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
    old_top: Address,
    old_limit: Address,
    init_old_top: Vec<Address>,

    reason: GcReason,
    number_workers: usize,

    min_heap_size: usize,
    max_heap_size: usize,

    units: Vec<Unit>,
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
        let old_total = old.total();

        ParallelFullCollector {
            vm: vm,
            heap: heap,
            young: young,
            old: old,
            old_protected: old.protected(),
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            old_total: old.total(),
            old_top: old_total.start,
            old_limit: old_total.end,
            init_old_top: Vec::new(),

            reason: reason,
            number_workers: number_workers,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            units: Vec::new(),
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
        self.init_old_top = self.old_protected.regions.iter().map(|r| r.top()).collect();

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

        let regions: Vec<_> = self
            .regions
            .iter()
            .map(|r| OldRegion::new(r.object_region, r.top, r.mapped_region))
            .collect();
        self.old_protected.update_regions(regions);
    }

    fn mark_live(&mut self, pool: &mut Pool) {
        marking::start(
            self.rootset,
            self.heap.clone(),
            self.perm_space.total(),
            pool,
        );
    }

    fn compute_forward(&mut self, pool: &mut Pool) {
        self.compute_units();
        self.compute_live_bytes(pool);
        self.compute_regions();

        if self.vm.args.flag_gc_verify {
            self.check_units_disjoint();
            self.check_regions_disjoint();
        }

        self.compute_actual_forward(pool);

        let regions: Vec<_> = self.regions.iter().map(|r| r.mapped_region).collect();
        self.old_protected.commit_regions(&regions);
    }

    fn check_units_disjoint(&self) {
        let mut units: Vec<Region> = self.units.iter().map(|u| u.region).collect();
        units.sort_by(|a, b| a.start.cmp(&b.start));

        let mut last = Address::null();

        for unit in units {
            assert!(last <= unit.start);
            last = unit.end;
        }
    }

    fn check_regions_disjoint(&self) {
        let mut last_object = Address::null();
        let mut last_mapped = Address::null();

        for region in &self.regions {
            assert!(last_object <= region.object_region.start);
            assert!(last_mapped <= region.mapped_region.start);
            last_object = region.object_region.end;
            last_mapped = region.mapped_region.end;
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
        self.units.push(Unit::young(eden));

        let from = self.young.from_active();
        self.units.push(Unit::young(from));

        let to = self.young.to_active();
        self.units.push(Unit::young(to));
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

        let (card_start, card_end) = self.card_table.card_indices(ptr, end.align_card());
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
            for unit in &mut self.units {
                let vm = self.vm;

                scoped.execute(move || {
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
                });
            }
        });
    }

    fn compute_regions(&mut self) {
        let live: usize = self.units.iter().map(|u| u.live).sum();
        let number_regions = self.number_workers;

        let region_size = ((live as f64 / number_regions as f64) * 0.90f64) as usize;
        let mut regions = Vec::with_capacity(number_regions);

        let mut start = 0;
        let mut size = 0;

        for (id, unit) in self.units.iter().enumerate() {
            size += unit.live;

            if size > region_size {
                self.add_region(start, id, &mut size, &mut regions);
                start = id + 1;
            }
        }

        if start < self.units.len() {
            let end = self.units.len() - 1;
            self.add_region(start, end, &mut size, &mut regions);
        }

        if let Some(last) = regions.last_mut() {
            // We realize after computing all regions whether all surviving objects fit into the heap
            if last.object_region.end > self.old_total.end {
                panic!("OOM");
            }

            // last region can be extended up to the heap end
            last.object_region.end = self.old_total.end;
        } else {
            unreachable!();
        }

        std::mem::replace(&mut self.regions, regions);
    }

    fn add_region(
        &self,
        unit_start_idx: usize,
        unit_end_idx: usize,
        size: &mut usize,
        regions: &mut Vec<CollectRegion>,
    ) {
        let units = unit_end_idx - unit_start_idx + 1;
        let unit_start = &self.units[unit_start_idx];

        let object_start = if unit_start.young {
            regions
                .last()
                .map(|r| r.object_region.end)
                .unwrap_or(self.old_total.start)
        } else {
            unit_start.region.start
        };

        let unit_end = &self.units[unit_end_idx];

        let object_end = if unit_end.young {
            let mut object_end = Address::null();

            for unit_idx in unit_end_idx..=unit_start_idx {
                let unit = &self.units[unit_idx];

                if !unit.young {
                    object_end = unit.region.end;
                    break;
                }
            }

            cmp::max(object_start.offset(*size), object_end)
        } else {
            unit_end.region.end
        };

        let object_region = Region::new(object_start, object_end);

        let last_mapped = regions
            .last()
            .map(|r| r.mapped_region.end)
            .unwrap_or(self.old_total.start);

        let mapped_start = cmp::max(object_start.align_page_down(), last_mapped);
        let mapped_end = object_start.offset(*size).align_page();
        let mapped_region = Region::new(mapped_start, mapped_end);

        if let Some(region) = regions.last_mut() {
            assert!(region.object_region.end <= object_region.start);
            region.object_region.end = object_region.start;
        }

        regions.push(CollectRegion::new(
            unit_start_idx,
            units,
            object_region,
            object_start.offset(*size),
            mapped_region,
        ));
        *size = 0;
    }

    fn compute_actual_forward(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            for region in &mut self.regions {
                let units = &self.units;

                scope.execute(move || {
                    let mut fwd = region.object_region.start;

                    for unit in units.iter().skip(region.idx).take(region.units) {
                        if unit.live == 0 {
                            continue;
                        }

                        walk_region(unit.region, |obj, _address, size| {
                            if obj.header().is_marked_non_atomic() {
                                obj.header_mut().set_fwdptr_non_atomic(fwd);
                                fwd = fwd.offset(size);
                            }
                        });
                    }

                    assert_eq!(region.top, fwd);
                });
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
                if unit.live == 0 {
                    continue;
                }

                let pfull = &self;

                scope.execute(move || {
                    walk_region(unit.region, |obj, _address, _size| {
                        if obj.header().is_marked_non_atomic() {
                            obj.visit_reference_fields(|field| {
                                pfull.forward_reference(field);
                            });
                        }
                    });
                });
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

                            arena::forget(free_start, free_size);
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
                    for unit in units.iter().skip(region.idx).take(region.units) {
                        if unit.live == 0 {
                            continue;
                        }

                        walk_region(unit.region, |object, address, object_size| {
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

                                pfull
                                    .old
                                    .update_crossing(dest, next_dest, dest_obj.is_array_ref());
                            }
                        });
                    }
                });
            }

            let regions = self
                .old_protected
                .regions
                .iter()
                .map(|r| (r.start(), r.top()))
                .collect::<Vec<_>>();

            for ((start, top), init_top) in regions.into_iter().zip(&self.init_old_top) {
                let card_table = self.card_table;

                scope.execute(move || {
                    let top = cmp::max(top, *init_top);
                    card_table.reset_region(start, top);
                });
            }
        });
    }
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

struct Unit {
    region: Region,
    live: usize,
    young: bool,
}

impl Unit {
    fn old(region: Region) -> Unit {
        Unit {
            region: region,
            live: 0,
            young: false,
        }
    }

    fn young(region: Region) -> Unit {
        Unit {
            region: region,
            live: 0,
            young: true,
        }
    }

    fn size(&self) -> usize {
        self.region.size()
    }
}

struct CollectRegion {
    idx: usize,
    units: usize,

    object_region: Region,
    top: Address,
    mapped_region: Region,
}

impl CollectRegion {
    fn new(
        idx: usize,
        units: usize,
        object_region: Region,
        top: Address,
        mapped_region: Region,
    ) -> CollectRegion {
        CollectRegion {
            idx: idx,
            units: units,

            object_region: object_region,
            top: top,
            mapped_region: mapped_region,
        }
    }
}
