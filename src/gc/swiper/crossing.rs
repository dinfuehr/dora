use crate::gc::swiper::CardIdx;
use crate::gc::swiper::{CARD_REFS, CARD_SIZE, CARD_SIZE_BITS};
use crate::gc::{Address, Region};
use crate::mem;
use crate::object::offset_of_array_data;

// see GC Handbook 11.8: Crossing Maps
// meaning of byte value
//
// 0 <= v < 64: [0-63] FirstObject
//     first object starts v words after card start
//     no references before first object
//
// 64: NoRefs
//     no references in this card
//
// 64 < v <= 128: [1-64] LeadingRefs
//     there are (v-64) references before first object
//
// 128 < v <= 130: [1-2] ArrayStart
//     object starts (v-128) words before this card
//     used only for object arrays when array starts in the card before
//     and some non-reference content is in the next card

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
            start: start,
            end: end,
            cards_in_heap: heap_size / CARD_SIZE,
        }
    }

    pub fn set_no_references(&self, card: CardIdx) {
        self.set(card, 64);
    }

    pub fn set_first_object(&self, card: CardIdx, words: usize) {
        assert!(words < 64);
        self.set(card, words as u8);
    }

    pub fn set_array_start(&self, card: CardIdx, words: usize) {
        assert!(words == 1 || words == 2);
        self.set(card, (128 + words) as u8);
    }

    pub fn set_references_at_start(&self, card: CardIdx, refs: usize) {
        assert!(refs > 0 && refs <= 64);
        self.set(card, 64 + (refs as u8));
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
        } else if val <= 128 {
            CrossingEntry::LeadingRefs(val - 64)
        } else {
            assert!(val == 129 || val == 130);
            CrossingEntry::ArrayStart(val - 128)
        }
    }

    #[inline(always)]
    fn index_from_card_idx(&self, card: CardIdx) -> usize {
        let card = card.to_usize();
        assert!(card < self.cards_in_heap);
        card
    }

    pub fn update(
        &self,
        old_total: Region,
        object_start: Address,
        object_end: Address,
        array_ref: bool,
    ) {
        debug_assert!(old_total.valid_top(object_start) && old_total.valid_top(object_end));

        if (object_start.to_usize() >> CARD_SIZE_BITS) == (object_end.to_usize() >> CARD_SIZE_BITS)
        {
            // object does not span multiple cards

        } else if array_ref {
            let end_card_idx = card_idx(object_end, old_total.start);
            let end_card_addr = card_address(end_card_idx, old_total.start);

            let start_card_idx = card_idx(object_start, old_total.start);
            let start_card_end = card_address(start_card_idx, old_total.start).offset(CARD_SIZE);

            let mut loop_card_start = start_card_idx + 1;

            // If you allocate an object array just before the card end,
            // it could happen that the card starts with part of the header
            // or the length-field.
            if object_start.offset(offset_of_array_data() as usize) > start_card_end {
                let diff = start_card_end.offset_from(object_start) / mem::ptr_width_usize();
                self.set_array_start(loop_card_start.into(), diff);

                loop_card_start += 1;
            }

            // all cards between ]start_card; end_card[ are full with references
            for c in loop_card_start..end_card_idx {
                self.set_references_at_start(c.into(), CARD_REFS);
            }

            // end_card starts with x references, then next object
            if end_card_idx >= loop_card_start && object_end < old_total.end {
                if object_end == end_card_addr {
                    self.set_first_object(end_card_idx.into(), 0);
                } else {
                    let refs_dist = object_end.offset_from(end_card_addr) / mem::ptr_width_usize();
                    self.set_references_at_start(end_card_idx.into(), refs_dist);
                }
            }
        } else {
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
    LeadingRefs(u8),
    FirstObject(u8),
    ArrayStart(u8),
}
