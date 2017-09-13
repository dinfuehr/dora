use ctxt::SemContext;
use gc::Address;
use gc::Collector;
use gc::swiper::Region;

pub struct OldGen {
    total: Region,
}

impl OldGen {
    pub fn new(old_start: Address, old_end: Address) -> OldGen {
        OldGen {
            total: Region::new(old_start, old_end),
        }
    }
}

impl Collector for OldGen {
    fn alloc(&self, _: &SemContext, _: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, _: &SemContext) {
        unimplemented!();
    }
}
