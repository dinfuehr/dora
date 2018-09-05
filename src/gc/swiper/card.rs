use std::ptr;

use gc::swiper::crossing::Card;
use gc::swiper::CARD_SIZE_BITS;
use gc::Address;

pub struct CardTable {
    // card table boundaries for old gen (not young gen)
    // table is actually larger but we shouldn't need
    // to touch the rest
    start: Address,
    end: Address,
}

impl CardTable {
    pub fn new(start: Address, end: Address, young_size: usize) -> CardTable {
        // only keep track of card table for old gen,
        // just ignore the card table for the young gen
        let start = start.offset(young_size >> CARD_SIZE_BITS);

        let card = CardTable {
            start: start,
            end: end,
        };

        // reset card table to all 1's
        card.reset();

        card
    }

    // reset card table entries to 1 (not dirty)
    fn reset(&self) {
        let size = self.size();

        unsafe {
            ptr::write_bytes(self.start.to_mut_ptr::<u8>(), 1, size);
        }
    }

    fn size(&self) -> usize {
        self.end.offset_from(self.start)
    }

    // visits all dirty cards
    pub fn visit_dirty<F>(&self, mut f: F)
    where
        F: FnMut(Card),
    {
        let mut ptr = self.start;

        while ptr < self.end {
            let val: u8 = unsafe { *ptr.to_ptr() };

            if val == 0 {
                f(Card::from(ptr.offset_from(self.start)));
            }

            ptr = ptr.offset(1);
        }
    }

    pub fn get(&self, card: Card) -> CardEntry {
        let ptr = self.start.offset(card.to_usize());
        debug_assert!(ptr < self.end);

        let val: u8 = unsafe { *ptr.to_ptr() };

        if val == 0 {
            CardEntry::Dirty
        } else {
            CardEntry::Clean
        }
    }

    pub fn set(&self, card: Card, entry: CardEntry) {
        let ptr = self.start.offset(card.to_usize());
        debug_assert!(ptr < self.end);

        let val: u8 = match entry {
            CardEntry::Clean => 1,
            CardEntry::Dirty => 0,
        };

        unsafe {
            *ptr.to_mut_ptr() = val;
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum CardEntry {
    Clean,
    Dirty,
}

impl CardEntry {
    pub fn is_clean(self) -> bool {
        self == CardEntry::Clean
    }

    pub fn is_dirty(self) -> bool {
        self == CardEntry::Dirty
    }
}
