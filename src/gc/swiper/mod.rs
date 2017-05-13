use ctxt::Context;
use gc::Collector;

pub struct Swiper {

}

impl Collector for Swiper {
    fn alloc(&self, ctxt: &Context, size: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, ctxt: &Context) {
        unimplemented!();
    }
}