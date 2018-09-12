use gc::Address;

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
//     there are v references before first object
//
// 128 < v: [1] ArrayStart
//     object starts v words before this card
//     used only for object arrays when array starts in the card before
//     and some non-reference content is in the next card

#[derive(Copy, Clone)]
pub struct CardIdx(usize);

impl CardIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for CardIdx {
    fn from(val: usize) -> CardIdx {
        CardIdx(val)
    }
}

#[derive(Clone)]
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

    pub fn set_no_references(&self, card: CardIdx) {
        self.set(card, 64);
    }

    pub fn set_first_object(&self, card: CardIdx, words: usize) {
        assert!(words < 64);
        self.set(card, words as u8);
    }

    pub fn set_array_start(&self, card: CardIdx, words: usize) {
        assert!(words == 1);
        self.set(card, (128 + words) as u8);
    }

    pub fn set_references_at_start(&self, card: CardIdx, refs: usize) {
        assert!(refs > 0 && refs <= 64);
        self.set(card, 64 + (refs as u8));
    }

    fn set(&self, card: CardIdx, val: u8) {
        unsafe {
            *self.start.offset(card.to_usize()).to_mut_ptr::<u8>() = val;
        }
    }

    pub fn get(&self, card: CardIdx) -> CrossingEntry {
        let val = unsafe { *self.start.offset(card.to_usize()).to_ptr::<u8>() };

        if val < 64 {
            CrossingEntry::FirstObject(val)

        } else if val == 64 {
            CrossingEntry::NoRefs

        } else if val <= 128 {
            CrossingEntry::LeadingRefs(val - 64)

        } else {
            assert!(val == 129);
            CrossingEntry::ArrayStart(1)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CrossingEntry {
    NoRefs,
    LeadingRefs(u8),
    FirstObject(u8),
    ArrayStart(u8),
}
