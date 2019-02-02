use parking_lot::{Mutex, MutexGuard};

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
        let old = OldGen {
            total: Region::new(start, end),
            protected: Mutex::new(OldGenProtected::new(start)),

            crossing_map: crossing_map,
            card_table: card_table,
            config: config,
        };

        old
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn active(&self) -> Region {
        let protected = self.protected.lock();
        assert!(protected.regions.len() == 1);
        let region = protected.regions.first().unwrap();

        Region::new(region.start, region.top)
    }

    pub fn committed(&self) -> Region {
        let protected = self.protected.lock();
        assert!(protected.regions.len() == 1);
        let region = protected.regions.first().unwrap();

        Region::new(region.start, region.end)
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
        assert!(protected.regions.len() == 1);

        protected.regions.first().unwrap().top
    }

    pub fn limit(&self) -> Address {
        let protected = self.protected.lock();
        assert!(protected.regions.len() == 1);

        protected.regions.first().unwrap().end
    }

    pub fn update_top(&self, top: Address) {
        assert!(self.total.valid_top(top));
        let mut protected = self.protected.lock();
        assert!(protected.regions.len() == 1);

        let region = protected.regions.first_mut().unwrap();
        region.top = top;
    }

    pub fn set_committed_size(&self, new_size: usize) {
        assert!(gen_aligned(new_size));

        let mut protected = self.protected.lock();
        assert!(protected.regions.len() == 1);
        let region = protected.regions.first_mut().unwrap();

        let old_committed = region.end;
        let new_committed = self.total.start.offset(new_size);
        assert!(new_committed <= self.total.end);
        assert!(region.top <= new_committed);

        if old_committed < new_committed {
            let size = new_committed.offset_from(old_committed);
            arena::commit(old_committed.into(), size, false);
        } else if old_committed > new_committed {
            let size = old_committed.offset_from(new_committed);
            arena::forget(new_committed.into(), size);
        }

        region.end = new_committed;
    }

    pub fn grow(&self) -> Address {
        let mut protected = self.protected.lock();
        assert!(protected.regions.len() == 1);

        let mut config = self.config.lock();

        if !config.grow_old(GEN_SIZE) {
            return protected.regions.first().unwrap().start;
        }

        protected.size += GEN_SIZE;

        let region = protected.regions.first_mut().unwrap();
        arena::commit(region.end, GEN_SIZE, false);
        region.end = region.end.offset(GEN_SIZE);

        region.end
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
    pub size: usize,
    pub regions: Vec<OldRegion>,
}

impl OldGenProtected {
    fn new(start: Address) -> OldGenProtected {
        OldGenProtected {
            size: 0,
            regions: vec![OldRegion::new(start)],
        }
    }

    pub fn contains_slow(&self, addr: Address) -> bool {
        for old_region in &self.regions {
            if old_region.start <= addr && addr < old_region.top {
                return true;
            }
        }

        false
    }
}

#[derive(Clone)]
pub struct OldRegion {
    start: Address,
    top: Address,
    end: Address,
}

impl OldRegion {
    fn new(start: Address) -> OldRegion {
        OldRegion {
            start: start,
            top: start,
            end: start,
        }
    }

    pub fn size(&self) -> usize {
        self.end.offset_from(self.start)
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.start)
    }

    pub fn active_region(&self) -> Region {
        Region::new(self.start, self.top)
    }

    pub fn total_region(&self) -> Region {
        Region::new(self.start, self.end)
    }
}
