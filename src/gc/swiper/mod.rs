use ctxt::Context;
use driver::cmd::Args;
use gc::Collector;

pub struct Swiper {
    no_regions: usize,
    region_size_bits: usize,
    region_size: usize,
    regions: Vec<RegionStatus>,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size
            .map(|s| *s)
            .unwrap_or(32 * 1024 * 1024);
        let no_regions = heap_size / (1024 * 1024);

        Swiper {
            no_regions: no_regions,
            region_size_bits: 20,
            region_size: 1024 * 1024,
            regions: vec![RegionStatus::Free, no_regions]
        }
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

enum RegionStatus {
    Young,
    Old,
    Huge,
    Free,
}