use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use gc::Address;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::Region;
use mem;
use object::offset_of_array_data;

pub struct OldGen {
    pub total: Region,
    pub free: AtomicUsize,
    crossing_map: CrossingMap,
    card_table: CardTable,
}

impl OldGen {
    pub fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
    ) -> OldGen {
        OldGen {
            total: Region::new(start, end),
            free: AtomicUsize::new(start.to_usize()),
            crossing_map: crossing_map,
            card_table: card_table,
        }
    }

    pub fn used_region(&self) -> Region {
        Region::new(self.total.start, self.free())
    }

    pub fn free(&self) -> Address {
        self.free.load(Ordering::Relaxed).into()
    }

    pub fn alloc(&self, size: usize, is_obj_array: bool) -> *const u8 {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.total.end.to_usize() {
                return ptr::null();
            }

            let res =
                self.free
                    .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        if (old >> CARD_SIZE_BITS) == (new >> CARD_SIZE_BITS) {
            if (old & (CARD_SIZE - 1)) == 0 {
                let card = self.card_table.card(old.into());
                self.crossing_map.set_first_object(card, 0);
            }
        } else if is_obj_array {
            let card = self.card_table.card(new.into());
            let card_start = self.card_table.to_address(card).to_usize();

            let old = Address::from(old);
            let old_card = self.card_table.card(old);
            let old_card_end = self.card_table.to_address(old_card).offset(CARD_SIZE);

            let refs_per_card = CARD_SIZE / mem::ptr_width_usize();
            let mut loop_card_start = old_card.to_usize() + 1;

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
            for c in loop_card_start..card.to_usize() {
                self.crossing_map
                    .set_references_at_start(c.into(), refs_per_card);
            }

            if card.to_usize() > loop_card_start {
                self.crossing_map
                    .set_references_at_start(card, (new - card_start) / mem::ptr_width_usize());
            }
        } else {
            let card = self.card_table.card(new.into());
            let card_start = self.card_table.to_address(card).to_usize();

            let old_card = self.card_table.card(old.into());

            // all cards between ]old_card; new_card[ are set to NoRefs
            for c in old_card.to_usize() + 1..card.to_usize() {
                self.crossing_map.set_no_references(c.into());
            }

            self.crossing_map
                .set_first_object(card, (new - card_start) / mem::ptr_width_usize());
        }

        old as *const u8
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn valid_top(&self, addr: Address) -> bool {
        self.total.valid_top(addr)
    }
}
