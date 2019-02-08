use scoped_threadpool::Pool;
use std::cmp;

use ctxt::VM;
use gc::root::Slot;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::large::LargeSpace;
use gc::swiper::marking;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::{CardIdx, CARD_REFS};
use gc::{Address, GcReason, Region};
use object::Obj;

pub struct ParallelFullCollector<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [Slot],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

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
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

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
        self.init_old_top = {
            let protected = self.old.protected();
            protected.regions.iter().map(|r| r.top()).collect()
        };

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

        self.update_references();

        if dev_verbose {
            println!("Full GC: Phase 4 (relocate)");
        }

        self.relocate();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects)");
        }

        self.update_large_objects();

        if dev_verbose {
            println!("Full GC: Phase 5 (large objects) finished.");
        }
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
                regions.push(OldRegion::new(start, units));

                start = id + 1;
            }
        }

        if start != self.units.len() {
            let units = self.units.len() - start;
            regions.push(OldRegion::new(start, units));
        }

        std::mem::replace(&mut self.regions, regions);
    }

    fn compute_actual_forward(&mut self, _pool: &mut Pool) {
        unimplemented!();
    }

    fn update_references(&mut self) {
        unimplemented!();
    }

    fn relocate(&mut self) {
        unimplemented!();
    }

    fn update_large_objects(&mut self) {
        unimplemented!();
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
    start: usize,
    regions: usize,
}

impl OldRegion {
    fn new(start: usize, regions: usize) -> OldRegion {
        OldRegion {
            start: start,
            regions: regions,
        }
    }
}
