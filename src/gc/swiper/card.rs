use std::ptr;

use gc::Address;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::crossing::Card;
use gc::swiper::Region;

#[derive(Clone)]
pub struct CardTable {
    // card table boundaries for old gen (not young gen)
    // table is actually larger but we shouldn't need
    // to touch the rest
    start: Address,
    end: Address,

    heap_size: usize,

    // contiguous old & large space that can have references
    // into young generation
    old_and_large: Region,
}

impl CardTable {
    pub fn new(
        start: Address,
        end: Address,
        heap_size: usize,
        old_and_large: Region,
    ) -> CardTable {
        // only keep track of card table for old gen,
        // just ignore the card table for the young gen
        let start = start.offset(heap_size >> CARD_SIZE_BITS);

        let card = CardTable {
            start: start,
            end: end,
            heap_size: heap_size,
            old_and_large: old_and_large,
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
        let end = self.start.offset(self.heap_size >> CARD_SIZE_BITS);

        while ptr < end {
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

    pub fn clear(&self, card: Card) {
        self.set(card, CardEntry::Clean);
    }

    #[inline(always)]
    pub fn is_aligned(&self, addr: Address) -> bool {
        debug_assert!(self.old_and_large.contains(addr));
        (addr.offset_from(self.old_and_large.start) & !(CARD_SIZE - 1)) == 0
    }

    #[inline(always)]
    pub fn to_address(&self, card: Card) -> Address {
        let addr = self.old_and_large.start.to_usize() + (card.to_usize() << CARD_SIZE_BITS);

        addr.into()
    }

    #[inline(always)]
    pub fn card(&self, addr: Address) -> Card {
        debug_assert!(self.old_and_large.contains(addr));
        let idx = addr.offset_from(self.old_and_large.start) >> CARD_SIZE_BITS;

        idx.into()
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
