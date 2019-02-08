use parking_lot::MutexGuard;
use scoped_threadpool::Pool;
use std::cmp;

use ctxt::VM;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::marking;
use gc::swiper::old::{OldGen, OldGenProtected};
use gc::swiper::young::YoungGen;
use gc::swiper::{CardIdx, CARD_REFS};
use gc::{Address, GcReason, Region};
use object::Obj;

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
    regions: Vec<OldRegion>,
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
        }
    }

    pub fn collect(&mut self, pool: &mut Pool) {
        let dev_verbose = self.vm.args.flag_gc_dev_verbose;
        self.init_old_top = self.old_protected.regions.iter().map(|r| r.top()).collect();

        if dev_verbose {
            println!("Full GC: Phase 1 (marking)");
        }

        self.mark_live(pool);

        if dev_verbose {
            println!("Full GC: Phase 2 (compute forward)");
        }

        self.compute_forward(pool);

        if dev_verbose {
            println!("Full GC: Phase 3 (update refs)");
        }

        self.update_references(pool);

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.relocate(pool);

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects)");
        }

        self.update_large_objects();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects) finished.");
        }

        self.reset_cards();

        self.young.clear();
        self.young.protect_to();
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
        self.compute_actual_forward(pool);
    }

    fn compute_units(&mut self) {
        let active = self.old.active_size();
        let unit_size = active / (4 * self.number_workers);

        let protected = self.old.protected();

        for old_region in &protected.regions {
            self.units_for_old_region(old_region.active_region(), unit_size);
        }

        let eden = self.young.eden_active();
        self.units.push(Unit::new(eden));

        let from = self.young.from_active();
        self.units.push(Unit::new(from));

        let to = self.young.to_active();
        self.units.push(Unit::new(to));
    }

    fn units_for_old_region(&mut self, region: Region, unit_size: usize) {
        let mut last = region.start;

        while last < region.end {
            let end = self.find_object_start(last, unit_size, region.end);
            let region = Region::new(last, end);
            self.units.push(Unit::new(region));
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
                scoped.execute(move || {
                    let mut live = 0;

                    walk_region(unit.region, |obj, _address, size| {
                        if obj.header().is_marked_non_atomic() {
                            live += size;
                        }
                    });

                    unit.live = 0;
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
                let units = id - start + 1;
                let region_start = self.units[start].region.start;
                let region_end = self.units[id].region.end;
                regions.push(OldRegion::new(start, units, region_start, region_end));

                start = id + 1;
            }
        }

        if start != self.units.len() {
            let units = self.units.len() - start;
            let region_start = self.units[start].region.start;
            let region_end = self.units[self.units.len() - 1].region.end;
            regions.push(OldRegion::new(start, units, region_start, region_end));
        }

        std::mem::replace(&mut self.regions, regions);
    }

    fn compute_actual_forward(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            for region in &mut self.regions {
                let units = &self.units;

                scope.execute(move || {
                    let mut fwd = region.start;

                    for unit in units.iter().skip(region.idx).take(region.units) {
                        walk_region(unit.region, |obj, _address, size| {
                            if obj.header().is_marked_non_atomic() {
                                obj.header_mut().set_fwdptr_non_atomic(fwd);
                                fwd = fwd.offset(size);
                            }
                        });
                    }

                    region.top = fwd;
                });
            }
        });
    }

    fn update_references(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            for (idx, region) in self.regions.iter().enumerate() {
                let units = &self.units;
                let pfull = &self;
                let regions = self.regions.len();
                let rootset = self.rootset;

                scope.execute(move || {
                    for root in rootset.iter().skip(idx).step_by(regions) {
                        pfull.forward_reference(*root);
                    }

                    for unit in units.iter().skip(region.idx).take(region.units) {
                        walk_region(unit.region, |obj, _address, _size| {
                            if obj.header().is_marked_non_atomic() {
                                obj.visit_reference_fields(|field| {
                                    pfull.forward_reference(field);
                                });
                            }
                        });
                    }
                });
            }
        });

        self.large_space.visit_objects(|object_start| {
            let object = object_start.to_mut_obj();

            if object.header().is_marked_non_atomic() {
                object.visit_reference_fields(|field| {
                    self.forward_reference(field);
                });
            }
        });
    }

    fn forward_reference(&self, slot: Slot) {
        let object_addr = slot.get();

        if self.old_total.contains(object_addr) {
            debug_assert!(object_addr.to_obj().header().is_marked_non_atomic());
            let fwd_addr = object_addr.to_obj().header().fwdptr_non_atomic();
            debug_assert!(self.old_total.contains(fwd_addr));
            slot.set(fwd_addr);
        } else if self.large_space.contains(object_addr) {
            debug_assert!(object_addr.to_obj().header().is_marked_non_atomic());
        } else {
            debug_assert!(object_addr.is_null() || self.perm_space.contains(object_addr));
        }
    }

    fn relocate(&mut self, pool: &mut Pool) {
        pool.scoped(|scope| {
            for region in &self.regions {
                let units = &self.units;
                let pfull = &self;

                scope.execute(move || {
                    for unit in units.iter().skip(region.idx).take(region.units) {
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
        });
    }

    fn update_large_objects(&mut self) {
        self.large_space.remove_objects(|object_start| {
            let object = object_start.to_mut_obj();

            // reset cards for object, also do this for dead objects
            // to reset card entries to clean.
            if object.is_array_ref() {
                let object_end = object_start.offset(object.size());
                self.card_table.reset_region(object_start, object_end);
            } else {
                self.card_table.reset_addr(object_start);
            }

            if !object.header().is_marked_non_atomic() {
                // object is unmarked -> free it
                return false;
            }

            // unmark object for next collection
            object.header_mut().unmark_non_atomic();

            // keep object
            true
        });
    }

    fn reset_cards(&mut self) {
        let regions = self
            .old
            .protected()
            .regions
            .iter()
            .map(|r| (r.start(), r.top()))
            .collect::<Vec<_>>();

        for ((start, top), init_top) in regions.into_iter().zip(&self.init_old_top) {
            let top = cmp::max(top, *init_top);
            self.card_table.reset_region(start, top);
        }
    }
}

fn walk_region<F>(region: Region, mut fct: F)
where
    F: FnMut(&mut Obj, Address, usize),
{
    let mut scan = region.start;

    while scan < region.end {
        let object = scan.to_mut_obj();

        if object.header().vtblptr().is_null() {
            scan = scan.add_ptr(1);
            continue;
        }

        let object_size = object.size();

        fct(object, scan, object_size);

        scan = scan.offset(object_size);
    }
}

struct Unit {
    region: Region,
    live: usize,
}

impl Unit {
    fn new(region: Region) -> Unit {
        Unit {
            region: region,
            live: 0,
        }
    }

    fn size(&self) -> usize {
        self.region.size()
    }
}

struct OldRegion {
    idx: usize,
    units: usize,

    start: Address,
    end: Address,
    top: Address,
}

impl OldRegion {
    fn new(idx: usize, units: usize, start: Address, end: Address) -> OldRegion {
        OldRegion {
            idx: idx,
            units: units,

            start: start,
            end: end,
            top: Address::null(),
        }
    }
}
