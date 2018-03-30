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