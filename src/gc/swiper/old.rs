use gc::Address;
use gc::swiper::Region;

pub struct OldGen {
    total: Region,
}

impl OldGen {
    pub fn new(old_start: Address, old_end: Address) -> OldGen {
        OldGen {
            total: Region::new(old_start, old_end)
        }
    }
}
