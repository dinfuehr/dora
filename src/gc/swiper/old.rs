use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use gc::Address;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::crossing::{Card, CrossingMap};
use gc::swiper::Region;

pub struct OldGen {
    pub total: Region,
    pub free: AtomicUsize,
    crossing_map: CrossingMap,
}

impl OldGen {
    pub fn new(start: Address, end: Address, crossing_map: CrossingMap) -> OldGen {
        OldGen {
            total: Region::new(start, end),
            free: AtomicUsize::new(start.to_usize()),
            crossing_map: crossing_map,
        }
    }

    pub fn used_region(&self) -> Region {
        Region::new(self.total.start, self.free.load(Ordering::Relaxed).into())
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.total.end.to_usize() {
                return ptr::null();
            }

            let res = self.free.compare_exchange_weak(
                old,
                new,
                Ordering::SeqCst,
                Ordering::Relaxed,
            );

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        if (old >> CARD_SIZE_BITS) == (new >> CARD_SIZE_BITS) {
            if (old & (CARD_SIZE - 1)) == 0 {
                let card = self.card_from(old);
                self.crossing_map.set_first_object(card, 0);
            }
        } else {
            let card = self.card_from(new);
            let card_start = self.address_from_card(card).to_usize();
            self.crossing_map.set_first_object(card, new - card_start);
        }

        old as *const u8
    }

    #[inline(always)]
    pub fn is_card_aligned(&self, addr: Address) -> bool {
        (addr.offset_from(self.total.start) & !(CARD_SIZE - 1)) == 0
    }

    #[inline(always)]
    pub fn address_from_card(&self, card: Card) -> Address {
        let addr = self.total.start.to_usize() + (card.to_usize() << CARD_SIZE_BITS);

        addr.into()
    }

    #[inline(always)]
    fn card_from(&self, addr: usize) -> Card {
        self.card_from_address(Address::from(addr))
    }

    #[inline(always)]
    pub fn card_from_address(&self, addr: Address) -> Card {
        debug_assert!(self.contains(addr));
        let idx = addr.offset_from(self.total.start) >> CARD_SIZE_BITS;

        idx.into()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }
}
