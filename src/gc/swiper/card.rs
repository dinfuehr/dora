use gc::Address;

pub struct CardTable {
    start: Address,
    end: Address,
}

impl CardTable {
    pub fn new(start: Address, end: Address) -> CardTable {
        CardTable {
            start: start,
            end: end,
        }
    }
}
