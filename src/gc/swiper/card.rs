use std::ptr;

use gc::swiper::CardIdx;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::Address;
use gc::swiper::Region;

use mem;

#[derive(Clone)]
pub struct CardTable {
    // card table boundaries for old gen (not young gen)
    // table is actually larger but we shouldn't need
    // to touch the rest
    start: Address,
    end: Address,

    // contiguous region with old generation and large space
    old_and_large: Region,

    // end of old generation and start of large space
    old_end: Address,
}

impl CardTable {
    pub fn new(start: Address, end: Address, old_and_large: Region, old_end: Address, young_size: usize) -> CardTable {
        // only keep track of card table for old gen,
        // just ignore the card table for the young gen
        let start = start.offset(young_size >> CARD_SIZE_BITS);

        let card = CardTable {
            start: start,
            end: end,
            old_and_large: old_and_large,
            old_end: old_end,
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

    // visits all dirty cards in region
    pub fn visit_dirty_in_old<F>(&self, end: Address, mut f: F)
    where
        F: FnMut(CardIdx),
    {
        assert!(end >= self.old_and_large.start && end <= self.old_end);
        let mut ptr = self.start;

        let no_cards = number_of_cards(end.offset_from(self.old_and_large.start));
        let end = self.start.offset(no_cards);

        while ptr < end {
            let val: u8 = unsafe { *ptr.to_ptr() };

            if val == 0 {
                f(CardIdx::from(ptr.offset_from(self.start)));
            }

            ptr = ptr.offset(1);
        }
    }

    pub fn get(&self, card: CardIdx) -> CardEntry {
        let ptr = self.start.offset(card.to_usize());
        debug_assert!(ptr < self.end);

        let val: u8 = unsafe { *ptr.to_ptr() };

        if val == 0 {
            CardEntry::Dirty
        } else {
            CardEntry::Clean
        }
    }

    pub fn set(&self, card: CardIdx, entry: CardEntry) {
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

    #[inline(always)]
    pub fn is_aligned(&self, addr: Address) -> bool {
        (addr.offset_from(self.old_and_large.start) & !(CARD_SIZE - 1)) == 0
    }

    #[inline(always)]
    pub fn to_address(&self, card: CardIdx) -> Address {
        self.old_and_large.start.offset(card.to_usize() << CARD_SIZE_BITS)
    }

    #[inline(always)]
    pub fn card_idx(&self, addr: Address) -> CardIdx {
        debug_assert!(self.old_and_large.contains(addr));
        let idx = addr.offset_from(self.old_and_large.start) >> CARD_SIZE_BITS;

        idx.into()
    }
}

fn number_of_cards(size: usize) -> usize {
    mem::align_usize(size, CARD_SIZE)
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
