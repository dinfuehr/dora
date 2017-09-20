use gc::Address;
use gc::swiper::CARD_SIZE;

// see GC Handbook 11.8: Crossing Maps
// meaning of byte value
//
// 0 = no references (set_no_refernces)
// 0 < v <= 64:   first object starts v words before end of card (set_first_object)
// 64 < v <= 128: the first v - 128 words of this card are a sequence of references at
//                end of object (set_references_at_start)
// v > 128:       consult the card v - 128 before (set_previous_card)

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
        self.set(card, 0);
    }

    pub fn set_first_object(&self, card: usize, words: usize) {
        assert!(words > 0 && words <= 64);
        self.set(card, words as u8);
    }

    pub fn set_references_at_start(&self, card: usize, refs: usize) {
        assert!(refs > 0 && refs <= 64);
        self.set(card, 64 + (refs as u8));
    }

    pub fn set_previous_card(&self, card: usize, prev: usize) {
        assert!(prev > 0);

        let val = prev + 128;
        let val = if val >= 255 { 255 } else { val };

        self.set(card, val as u8);
    }

    fn set(&self, card: usize, val: u8) {
        unsafe {
            *self.start.offset(card).to_mut_ptr::<u8>() = val;
        }
    }

    pub fn get(&self, card: usize) -> CrossingEntry {
        let val = unsafe { *self.start.offset(card).to_ptr::<u8>() };

        CrossingEntry(val)
    }

    pub fn address_of_card(&self, card: usize) -> Address {
        self.start.offset(card * CARD_SIZE)
    }
}

#[derive(Copy, Clone)]
pub struct CrossingEntry(u8);

impl CrossingEntry {
    pub fn is_no_references(self) -> bool {
        self.0 == 0
    }

    pub fn is_first_object(self) -> bool {
        self.0 > 0 && self.0 <= 64
    }

    pub fn first_object(self) -> u8 {
        self.0
    }

    pub fn is_references_at_start(self) -> bool {
        self.0 > 64 && self.0 <= 128
    }

    pub fn references_at_start(self) -> u8 {
        self.0 - 64
    }

    pub fn is_previous_card(self) -> bool {
        self.0 > 128
    }

    pub fn previous_card(self) -> u8 {
        self.0 - 128
    }
}
