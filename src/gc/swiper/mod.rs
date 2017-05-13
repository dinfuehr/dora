use ctxt::Context;
use driver::cmd::Args;
use gc::Collector;

pub struct Swiper {

}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        Swiper {}
    }
}

impl Collector for Swiper {
    fn alloc(&self, ctxt: &Context, size: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, ctxt: &Context) {
        unimplemented!();
    }
}