use std::sync::atomic::{AtomicUsize, Ordering};

use ctxt::VM;
use gc::arena;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::{fill_region, Address, Region};
use mem;
use object::offset_of_array_data;

pub struct OldGen {
    total: Region,

    top: AtomicUsize,
    limit: AtomicUsize,

    crossing_map: CrossingMap,
    card_table: CardTable,
}

impl OldGen {
    pub fn new(
        start: Address,
        end: Address,
        committed_size: usize,
        crossing_map: CrossingMap,
        card_table: CardTable,
    ) -> OldGen {
        let limit = start.offset(committed_size);

        let old = OldGen {
            total: Region::new(start, end),

            top: AtomicUsize::new(start.to_usize()),
            limit: AtomicUsize::new(limit.to_usize()),

            crossing_map: crossing_map,
            card_table: card_table,
        };

        old.commit();

        old
    }

    fn commit(&self) {
        let start = self.total.start;
        let size = self.limit.load(Ordering::Relaxed) - start.to_usize();

        arena::commit(start, size, false);
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn active(&self) -> Region {
        Region::new(self.total.start, self.top())
    }

    pub fn committed(&self) -> Region {
        let start = self.total.start;
        let limit = self.limit.load(Ordering::Relaxed).into();
        Region::new(start, limit)
    }

    pub fn committed_size(&self) -> usize {
        let start = self.total.start;
        let limit = self.limit.load(Ordering::Relaxed);

        limit - start.to_usize()
    }

    pub fn active_size(&self) -> usize {
        self.top().offset_from(self.total.start)
    }

    pub fn top(&self) -> Address {
        self.top.load(Ordering::Relaxed).into()
    }

    pub fn align_to_card(&self, vm: &VM) -> Address {
        let ctop = self.top();
        let ntop = mem::align_usize(ctop.to_usize(), CARD_SIZE).into();
        fill_region(vm, ctop, ntop);

        self.update_top(ntop);
        ntop
    }

    pub fn update_top(&self, top: Address) {
        self.top.store(top.to_usize(), Ordering::SeqCst);
    }

    pub fn set_committed_size(&self, new_size: usize) {
        let old_committed = self.limit.load(Ordering::Relaxed);
        let new_committed = self.total.start.offset(new_size).to_usize();
        assert!(new_committed <= self.total.end.to_usize());
        assert!(self.top().to_usize() <= new_committed);

        if old_committed == new_committed {
            return;
        }

        let updated = self
            .limit
            .compare_and_swap(old_committed, new_committed, Ordering::SeqCst);
        assert!(updated == old_committed);

        if old_committed < new_committed {
            let size = new_committed - old_committed;
            arena::commit(old_committed.into(), size, false);
        } else if old_committed > new_committed {
            let size = old_committed - new_committed;
            arena::forget(new_committed.into(), size);
        }
    }

    pub fn bump_alloc(&self, size: usize, array_ref: bool) -> Address {
        let mut old = self.top.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new > self.limit.load(Ordering::Relaxed) {
                return Address::null();
            }

            let res = self
                .top
                .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        self.update_crossing(old.into(), new.into(), array_ref);

        old.into()
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
