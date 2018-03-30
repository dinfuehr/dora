use gc::Address;
use gc::swiper::Region;

pub struct LargeObjectSpace {
    total: Region,
}

impl LargeObjectSpace {
    pub fn new(total: Region) -> LargeObjectSpace {
        LargeObjectSpace {
            total: total,
        }
    }
}

struct LargeAlloc {
    prev: Address,
    next: Address,
}