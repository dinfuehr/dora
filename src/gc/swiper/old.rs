use parking_lot::{Mutex, MutexGuard};
use std::cmp::{max, min};
use std::mem::replace;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::{arena, Address, Region, GEN_SIZE};
use mem;
use object::offset_of_array_data;

pub struct OldGen {
    total: Region,
    protected: Mutex<OldGenProtected>,

    crossing_map: CrossingMap,
    card_table: CardTable,
    config: SharedHeapConfig,
}

impl OldGen {
    pub fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
        config: SharedHeapConfig,
    ) -> OldGen {
        let total = Region::new(start, end);

        // first object is allocated on old.total.start
        crossing_map.set_first_object(0.into(), 0);

        let old = OldGen {
            total: total.clone(),
            protected: Mutex::new(OldGenProtected::new(total)),

            crossing_map: crossing_map,
            card_table: card_table,
            config: config,
        };

        old
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn total_start(&self) -> Address {
        self.total.start
    }

    pub fn committed_size(&self) -> usize {
        let protected = self.protected.lock();

        protected.size
    }

    pub fn active_size(&self) -> usize {
        let protected = self.protected.lock();
        let mut size = 0;

        for old_region in &protected.regions {
            size += old_region.active_size();
        }

        size
    }

    pub fn alloc(&self, size: usize) -> Address {
        let mut protected = self.protected.lock();
        protected.alloc(&self.config, size)
    }

    pub fn update_crossing(&self, old: Address, new: Address, array_ref: bool) {
        debug_assert!(self.total.valid_top(old) && self.total.valid_top(new));

        if (old.to_usize() >> CARD_SIZE_BITS) == (new.to_usize() >> CARD_SIZE_BITS) {
            // object does not span multiple cards

        } else if array_ref {
            let new_card_idx = self.card_table.card_idx(new.into());
            let new_card_start = self.card_table.to_address(new_card_idx);

            let old_card_idx = self.card_table.card_idx(old);
            let old_card_end = self.card_table.to_address(old_card_idx).offset(CARD_SIZE);

            let refs_per_card = CARD_SIZE / mem::ptr_width_usize();
            let mut loop_card_start = old_card_idx.to_usize() + 1;

            // If you allocate an object array just before the card end,
            // it could happen that the card starts with part of the header
            // or the length-field.
            if old.offset(offset_of_array_data() as usize) > old_card_end {
                let diff = old_card_end.offset_from(old) / mem::ptr_width_usize();
                self.crossing_map
                    .set_array_start(loop_card_start.into(), diff);

                loop_card_start += 1;
            }

            // all cards between ]old_card; new_card[ are full with references
            for c in loop_card_start..new_card_idx.to_usize() {
                self.crossing_map
                    .set_references_at_start(c.into(), refs_per_card);
            }

            // new_card starts with x references, then next object
            if new_card_idx.to_usize() >= loop_card_start && new < self.total.end {
                if new == new_card_start {
                    self.crossing_map.set_first_object(new_card_idx, 0);
                } else {
                    let refs_dist = new.offset_from(new_card_start) / mem::ptr_width_usize();
                    self.crossing_map
                        .set_references_at_start(new_card_idx, refs_dist);
                }
            }
        } else {
            let new_card_idx = self.card_table.card_idx(new.into());
            let new_card_start = self.card_table.to_address(new_card_idx);

            let old_card_idx = self.card_table.card_idx(old.into());

            // all cards between ]old_card; new_card[ are set to NoRefs
            for c in old_card_idx.to_usize() + 1..new_card_idx.to_usize() {
                self.crossing_map.set_no_references(c.into());
            }

            // new_card stores x words of object, then next object
            if new < self.total.end {
                self.crossing_map.set_first_object(
                    new_card_idx,
                    new.offset_from(new_card_start) / mem::ptr_width_usize(),
                );
            }
        }
    }

    pub fn protected(&self) -> MutexGuard<OldGenProtected> {
        self.protected.lock()
    }
}

pub struct OldGenProtected {
    pub total: Region,
    pub size: usize,
    pub regions: Vec<OldRegion>,
    pub alloc_region: usize,
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        OldGenProtected {
            total: total.clone(),
            size: 0,
            regions: vec![OldRegion::new(total)],
            alloc_region: 0,
        }
    }

    pub fn contains_slow(&self, addr: Address) -> bool {
        for old_region in &self.regions {
            if old_region.start <= addr && addr < old_region.alloc_top {
                return true;
            }
        }

        false
    }

    pub fn commit_single_region(&mut self, top: Address) {
        let limit = top.align_gen();
        assert!(self.total.valid_top(limit));

        let mut last = self.total.start;

        for region in &self.regions {
            let start = last;
            let end = min(region.start, limit);
            let size = end.offset_from(start);

            if size > 0 {
                arena::commit(start, size, false);
            }

            if end == limit {
                return;
            }

            last = region.alloc_limit;
        }

        let start = last;
        let end = limit;

        if end <= start {
            return;
        }

        let size = end.offset_from(start);
        arena::commit(start, size, false);
    }

    pub fn update_single_region(&mut self, top: Address) {
        let limit = top.align_gen();
        assert!(self.total.valid_top(limit));

        let region = OldRegion {
            start: self.total.start,
            end: self.total.end,

            alloc_top: top,
            alloc_limit: limit,
        };

        let regions = replace(&mut self.regions, vec![region]);

        for region in regions {
            if limit >= region.alloc_limit {
                continue;
            }

            let start = max(region.start, limit);
            let size = region.alloc_limit.offset_from(start);

            if size > 0 {
                arena::forget(start, size);
            }
        }

        self.size = limit.offset_from(self.total.start);
        self.alloc_region = 0;
    }

    pub fn alloc(&mut self, config: &SharedHeapConfig, size: usize) -> Address {
        let ptr = self.pure_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        {
            let mut config = config.lock();

            if !config.grow_old(GEN_SIZE) {
                return Address::null();
            }
        }

        if !self.extend(GEN_SIZE) {
            return Address::null();
        }

        self.pure_alloc(size)
    }

    fn pure_alloc(&mut self, size: usize) -> Address {
        let alloc_region = self.alloc_region;

        if let Some(addr) = self.regions[alloc_region].pure_alloc(size) {
            return addr;
        }

        for (idx, old_region) in &mut self.regions.iter_mut().enumerate() {
            if let Some(addr) = old_region.pure_alloc(size) {
                self.alloc_region = idx;
                return addr;
            }
        }

        Address::null()
    }

    fn extend(&mut self, size: usize) -> bool {
        let alloc_region = self.alloc_region;

        if self.regions[alloc_region].extend(size) {
            self.size += size;
            return true;
        }

        for (idx, old_region) in &mut self.regions.iter_mut().enumerate() {
            if old_region.extend(size) {
                self.size += size;
                self.alloc_region = idx;
                return true;
            }
        }

        false
    }
}

#[derive(Clone)]
pub struct OldRegion {
    start: Address,
    end: Address,

    alloc_top: Address,
    alloc_limit: Address,
}

impl OldRegion {
    fn new(region: Region) -> OldRegion {
        OldRegion {
            start: region.start,
            end: region.end,

            alloc_top: region.start,
            alloc_limit: region.start,
        }
    }

    pub fn start(&self) -> Address {
        self.start
    }

    pub fn top(&self) -> Address {
        self.alloc_top
    }

    pub fn size(&self) -> usize {
        self.end.offset_from(self.start)
    }

    pub fn active_size(&self) -> usize {
        self.alloc_top.offset_from(self.start)
    }

    pub fn active_region(&self) -> Region {
        Region::new(self.start, self.alloc_top)
    }

    pub fn total_region(&self) -> Region {
        Region::new(self.start, self.end)
    }

    pub fn committed_region(&self) -> Region {
        Region::new(self.start, self.alloc_limit)
    }

    fn pure_alloc(&mut self, size: usize) -> Option<Address> {
        let new_alloc_top = self.alloc_top.offset(size);

        if new_alloc_top <= self.alloc_limit {
            let addr = self.alloc_top;
            self.alloc_top = new_alloc_top;
            return Some(addr);
        }

        None
    }

    fn extend(&mut self, size: usize) -> bool {
        let new_alloc_limit = self.alloc_limit.offset(size);

        if new_alloc_limit <= self.end {
            arena::commit(self.alloc_limit, size, false);
            self.alloc_limit = new_alloc_limit;

            true
        } else {
            false
        }
    }
}
