use gc::Address;

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
}