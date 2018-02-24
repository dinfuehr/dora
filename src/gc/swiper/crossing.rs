use gc::Address;
use gc::swiper::CARD_SIZE;

// see GC Handbook 11.8: Crossing Maps
// meaning of byte value
//
// 0 < v <= 64
//     first object starts v words after card start, there are
//     no references before the first object
//     64 means no references
// 64 < v < 128
//     there are -v references before first object

pub struct CrossingMap {
    // boundaries for crossing map
    start: Address,
    end: Address,
}

impl CrossingMap {
    pub fn new(start: Address, end: Address) -> CrossingMap {
        CrossingMap {
            start: start,
            end: end,
        }
    }

    pub fn set_no_references(&self, card: usize) {
        self.set(card, 64);
    }

    pub fn set_first_object(&self, card: usize, words: usize) {
        assert!(words < 64);
        self.set(card, words as u8);
    }

    pub fn set_references_at_start(&self, card: usize, refs: usize) {
        assert!(refs > 0 && refs <= 64);
        self.set(card, 64 + (refs as u8));
    }

    fn set(&self, card: usize, val: u8) {
        unsafe {
            *self.start.offset(card).to_mut_ptr::<u8>() = val;
        }
    }

    pub fn get(&self, card: usize) -> CrossingEntry {
        let val = unsafe { *self.start.offset(card).to_ptr::<u8>() };

        if val < 64 {
            CrossingEntry::FirstObjectOffset(val)
        } else if val > 64 {
            CrossingEntry::LeadingRefs(val - 64)
        } else {
            CrossingEntry::NoRefs
        }
    }

    pub fn address_of_card(&self, card: usize) -> Address {
        self.start.offset(card * CARD_SIZE)
    }
}

#[derive(Copy, Clone)]
pub enum CrossingEntry {
    NoRefs,
    LeadingRefs(u8),
    FirstObjectOffset(u8),
}
