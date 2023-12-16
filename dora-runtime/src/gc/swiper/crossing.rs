use crate::gc::swiper::CardIdx;
use crate::gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use crate::gc::{Address, Region};
use crate::mem;

// see GC Handbook 11.8: Crossing Maps
// meaning of byte value
//
// 0 <= v < 64: [0-63] FirstObject
//     first object starts v words after card start
//     no references before first object
//
// 64: NoRefs
//     no references in this card

#[derive(Clone)]
pub struct CrossingMap {
    // boundaries for crossing map
    start: Address,
    end: Address,
    cards_in_heap: usize,
}

impl CrossingMap {
    pub fn new(start: Address, end: Address, heap_size: usize) -> CrossingMap {
        CrossingMap {
            start,
            end,
            cards_in_heap: heap_size / CARD_SIZE,
        }
    }

    fn set_no_references(&self, card: CardIdx) {
        self.set(card, 64);
    }

    pub fn set_first_object(&self, card: CardIdx, words: usize) {
        assert!(words < 64);
        self.set(card, words as u8);
    }

    fn set(&self, card: CardIdx, val: u8) {
        let card = self.index_from_card_idx(card);

        unsafe {
            *self.start.offset(card).to_mut_ptr::<u8>() = val;
        }
    }

    pub fn get(&self, card: CardIdx) -> CrossingEntry {
        let card = self.index_from_card_idx(card);

        let val = unsafe { *self.start.offset(card).to_ptr::<u8>() };

        if val < 64 {
            CrossingEntry::FirstObject(val)
        } else if val == 64 {
            CrossingEntry::NoRefs
        } else {
            panic!("invalid crossing table entry")
        }
    }

    #[inline(always)]
    fn index_from_card_idx(&self, card: CardIdx) -> usize {
        let card = card.to_usize();
        assert!(card < self.cards_in_heap);
        card
    }

    pub fn update(&self, old_total: Region, object_start: Address, object_end: Address) {
        debug_assert!(old_total.valid_top(object_start) && old_total.valid_top(object_end));

        if (object_start.to_usize() >> CARD_SIZE_BITS) != (object_end.to_usize() >> CARD_SIZE_BITS)
        {
            let end_card_idx = card_idx(object_end, old_total.start);
            let end_card_addr = card_address(end_card_idx, old_total.start);

            let start_card_idx = card_idx(object_start, old_total.start);

            // all cards between ]start_card; end_card[ are set to NoRefs
            for c in start_card_idx + 1..end_card_idx {
                self.set_no_references(c.into());
            }

            // end_card stores x words of object, then next object
            if object_end < old_total.end {
                self.set_first_object(
                    end_card_idx.into(),
                    object_end.offset_from(end_card_addr) / mem::ptr_width_usize(),
                );
            }
        }
    }
}

fn card_idx(address: Address, start: Address) -> usize {
    address.offset_from(start) >> CARD_SIZE_BITS
}

fn card_address(card: usize, start: Address) -> Address {
    start.offset(card << CARD_SIZE_BITS)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CrossingEntry {
    NoRefs,
    FirstObject(u8),
}
