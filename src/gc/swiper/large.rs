use gc::Address;
use gc::swiper::Region;

pub struct LargeSpace {
    total: Region,
}

impl LargeSpace {
    pub fn new(start: Address, end: Address) -> LargeSpace {
        LargeSpace {
            total: Region::new(start, end),
        }
    }
}

struct LargeAlloc {
    prev: Address,
    next: Address,
}