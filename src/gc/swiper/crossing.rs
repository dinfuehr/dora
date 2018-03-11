use gc::Address;

// see GC Handbook 11.8: Crossing Maps
// meaning of byte value
//
// 0 < v <= 64
//     first object starts v words after card start, there are
//     no references before the first object
//     64 means no references
// 64 < v < 128
//     there are -v references before first object

#[derive(Copy, Clone)]
pub struct Card(usize);

impl Card {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for Card {
    fn from(val: usize) -> Card {
        Card(val)
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

    pub fn set_no_references(&self, card: Card) {
        self.set(card, 64);
    }

    pub fn set_first_object(&self, card: Card, words: usize) {
        assert!(words < 64);
        self.set(card, words as u8);
    }

    pub fn set_array_start(&self, card: Card, words: usize) {
        self.set(card, (129 + words) as u8);
    }

    pub fn set_references_at_start(&self, card: Card, refs: usize) {
        assert!(refs > 0 && refs <= 64);
        self.set(card, 64 + (refs as u8));
    }

    fn set(&self, card: Card, val: u8) {
        unsafe {
            *self.start.offset(card.to_usize()).to_mut_ptr::<u8>() = val;
        }
    }

    pub fn get(&self, card: Card) -> CrossingEntry {
        let val = unsafe { *self.start.offset(card.to_usize()).to_ptr::<u8>() };

        if val < 64 {
            CrossingEntry::FirstObjectOffset(val)
        } else if val > 64 && val <= 128 {
            CrossingEntry::LeadingRefs(val - 64)
        } else if val > 128 {
            CrossingEntry::ArrayStart(val - 129)
        } else {
            CrossingEntry::NoRefs
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CrossingEntry {
    NoRefs,
    LeadingRefs(u8),
    FirstObjectOffset(u8),
    ArrayStart(u8),
}
