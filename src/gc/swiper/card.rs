use std::ptr;

use gc::swiper::CardIdx;
use gc::swiper::Region;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::Address;

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

    // number of cards in heap_size
    cards_in_heap: usize,
}

impl CardTable {
    pub fn new(
        start: Address,
        end: Address,
        old_and_large: Region,
        old_end: Address,
        heap_size: usize,
    ) -> CardTable {
        // only keep track of card table for old gen,
        // just ignore the card table for the young gen
        let start = start.offset(heap_size >> CARD_SIZE_BITS);

        assert!(old_and_large.contains(old_end));

        let card = CardTable {
            start: start,
            end: end,
            old_and_large: old_and_large,
            old_end: old_end,
            cards_in_heap: heap_size / CARD_SIZE,
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

    // reset cards for memory region to 1 (not dirty)
    pub fn reset_region(&self, start: Address, end: Address) {
        let start_idx = self.card_idx(start).to_usize();

        let end = mem::align_usize(end.to_usize(), CARD_SIZE).into();
        let end_idx = self.card_idx(end).to_usize();

        for card_idx in start_idx..end_idx {
            self.set(card_idx.into(), CardEntry::Clean);
        }
    }

    // reset cards for address to 1 (not dirty)
    pub fn reset_addr(&self, addr: Address) {
        let card_idx = self.card_idx(addr);
        self.set(card_idx, CardEntry::Clean);
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
        let (start_card_idx, end_card_idx) = self.card_indices(self.old_and_large.start, end);

        for card_idx in start_card_idx..end_card_idx {
            if self.get(card_idx.into()).is_dirty() {
                f(card_idx.into());
            }
        }
    }

    pub fn is_dirty(&self, addr: Address) -> bool {
        let card_idx = self.card_idx(addr);
        self.get(card_idx) == CardEntry::Dirty
    }

    pub fn get(&self, card: CardIdx) -> CardEntry {
        let card = self.index_from_card_idx(card);
        let ptr = self.start.offset(card);
        debug_assert!(ptr < self.end);

        let val: u8 = unsafe { *ptr.to_ptr() };

        if val == 0 {
            CardEntry::Dirty
        } else {
            CardEntry::Clean
        }
    }

    pub fn set(&self, card: CardIdx, entry: CardEntry) {
        let card = self.index_from_card_idx(card);
        let ptr = self.start.offset(card);
        debug_assert!(ptr < self.end);

        let val: u8 = match entry {
            CardEntry::Clean => 1,
            CardEntry::Dirty => 0,
        };

        unsafe {
            *ptr.to_mut_ptr() = val;
        }
    }

    fn index_from_card_idx(&self, card: CardIdx) -> usize {
        let card = card.to_usize();
        assert!(card < 3 * self.cards_in_heap);
        card
    }

    #[inline(always)]
    pub fn is_aligned(&self, addr: Address) -> bool {
        (addr.offset_from(self.old_and_large.start) & !(CARD_SIZE - 1)) == 0
    }

    #[inline(always)]
    pub fn to_address(&self, card: CardIdx) -> Address {
        let card = self.index_from_card_idx(card);
        self.old_and_large.start.offset(card << CARD_SIZE_BITS)
    }

    #[inline(always)]
    pub fn card_idx(&self, addr: Address) -> CardIdx {
        debug_assert!(self.old_and_large.contains(addr));
        let idx = addr.offset_from(self.old_and_large.start) >> CARD_SIZE_BITS;

        idx.into()
    }

    #[inline(always)]
    pub fn card_indices(&self, start: Address, end: Address) -> (usize, usize) {
        assert!(end >= start);

        let card_start = self.card_idx(start);

        let end = mem::align_usize(end.to_usize(), CARD_SIZE).into();
        let card_end = self.card_idx(end);

        (card_start.to_usize(), card_end.to_usize())
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
