use parking_lot::Mutex;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::{arena, gen_aligned, Address, Region, GEN_SIZE};
use mem;
use object::offset_of_array_data;

pub struct OldGen {
    total: Region,
    protected: Mutex<OldProtected>,

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
            protected: Mutex::new(OldProtected::new(start)),

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
        Region::new(self.total.start, self.top())
    }

    pub fn committed(&self) -> Region {
        let protected = self.protected.lock();
        Region::new(self.total.start, protected.limit)
    }

    pub fn committed_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.limit.offset_from(self.total.start)
    }

    pub fn active_size(&self) -> usize {
        self.top().offset_from(self.total.start)
    }

    pub fn top(&self) -> Address {
        let protected = self.protected.lock();

        protected.top
    }

    pub fn limit(&self) -> Address {
        let protected = self.protected.lock();

        protected.limit
    }

    pub fn update_top(&self, top: Address) {
        assert!(self.total.valid_top(top));
        let mut protected = self.protected.lock();

        protected.top = top;
    }

    pub fn set_committed_size(&self, new_size: usize) {
        assert!(gen_aligned(new_size));

        let mut protected = self.protected.lock();

        let old_committed = protected.limit;
        let new_committed = self.total.start.offset(new_size);
        assert!(new_committed <= self.total.end);
        assert!(protected.top <= new_committed);

        if old_committed < new_committed {
            let size = new_committed.offset_from(old_committed);
            arena::commit(old_committed.into(), size, false);
        } else if old_committed > new_committed {
            let size = old_committed.offset_from(new_committed);
            arena::forget(new_committed.into(), size);
        }

        protected.limit = new_committed;
    }

    pub fn grow(&self) -> Address {
        let mut alloc = self.protected.lock();
        let mut config = self.config.lock();

        if !config.grow_old(GEN_SIZE) {
            return alloc.limit;
        }

        arena::commit(alloc.limit, GEN_SIZE, false);
        alloc.limit = alloc.limit.offset(GEN_SIZE);

        alloc.limit
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

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn valid_top(&self, addr: Address) -> bool {
        self.total.valid_top(addr)
    }
}

struct OldProtected {
    top: Address,
    limit: Address,
}

impl OldProtected {
    fn new(start: Address) -> OldProtected {
        OldProtected {
            top: start,
            limit: start,
        }
    }
}
