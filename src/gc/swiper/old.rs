use parking_lot::{Mutex, MutexGuard};
use std::cmp::{max, min};
use std::mem::replace;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::{arena, gen_aligned, Address, Region, GEN_SIZE};
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

    pub fn top(&self) -> Address {
        let protected = self.protected.lock();
        protected.top()
    }

    pub fn limit(&self) -> Address {
        let protected = self.protected.lock();
        protected.limit()
    }

    pub fn update_top(&self, top: Address) {
        let mut protected = self.protected.lock();
        protected.update_top(top);
    }

    pub fn set_committed_size(&self, new_size: usize) {
        let mut protected = self.protected.lock();
        protected.set_committed_size(new_size);
    }

    pub fn alloc(&self, size: usize) -> Address {
        let mut protected = self.protected.lock();
        protected.alloc(&self.config, size)
    }

    pub fn update_crossing(&self, old: Address, new: Address, array_ref: bool) {
        if (old.to_usize() >> CARD_SIZE_BITS) == (new.to_usize() >> CARD_SIZE_BITS) {
            // object does not span multiple cards
            if (old.to_usize() & (CARD_SIZE - 1)) == 0 {
                let card = self.card_table.card_idx(old.into());
                self.crossing_map.set_first_object(card, 0);
            }
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
            if new_card_idx.to_usize() >= loop_card_start {
                let refs_dist = new.offset_from(new_card_start) / mem::ptr_width_usize();

                if refs_dist > 0 {
                    self.crossing_map
                        .set_references_at_start(new_card_idx, refs_dist);
                } else {
                    self.crossing_map.set_first_object(new_card_idx, 0);
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
            self.crossing_map.set_first_object(
                new_card_idx,
                new.offset_from(new_card_start) / mem::ptr_width_usize(),
            );
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
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        OldGenProtected {
            total: total.clone(),
            size: 0,
            regions: vec![OldRegion::new(total)],
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

    pub fn top(&self) -> Address {
        self.single_region().alloc_top
    }

    pub fn limit(&self) -> Address {
        self.single_region().alloc_limit
    }

    pub fn update_top(&mut self, top: Address) {
        assert!(self.total.valid_top(top));
        let region = self.single_region_mut();
        region.alloc_top = top;
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
    }

    pub fn set_committed_size(&mut self, new_size: usize) {
        assert!(gen_aligned(new_size));
        let total = self.total.clone();
        let region = self.single_region_mut();

        let old_committed = region.alloc_limit;
        let new_committed = total.start.offset(new_size);
        assert!(new_committed <= total.end);
        assert!(new_committed <= region.end);
        assert!(region.alloc_top <= new_committed);

        if old_committed < new_committed {
            let size = new_committed.offset_from(old_committed);
            arena::commit(old_committed.into(), size, false);
        } else if old_committed > new_committed {
            let size = old_committed.offset_from(new_committed);
            arena::forget(new_committed.into(), size);
        }

        region.alloc_limit = new_committed;
    }

    fn single_region(&self) -> &OldRegion {
        assert!(self.regions.len() == 1);
        self.regions.first().unwrap()
    }

    fn single_region_mut(&mut self) -> &mut OldRegion {
        assert!(self.regions.len() == 1);
        self.regions.first_mut().unwrap()
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
        for old_region in &mut self.regions {
            let new_alloc_top = old_region.alloc_top.offset(size);

            if new_alloc_top <= old_region.alloc_limit {
                let addr = old_region.alloc_top;
                old_region.alloc_top = new_alloc_top;
                return addr;
            }
        }

        Address::null()
    }

    fn extend(&mut self, size: usize) -> bool {
        for old_region in &mut self.regions {
            let new_alloc_limit = old_region.alloc_limit.offset(size);

            if new_alloc_limit <= old_region.end {
                arena::commit(old_region.alloc_limit, size, false);
                old_region.alloc_limit = new_alloc_limit;
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
}
