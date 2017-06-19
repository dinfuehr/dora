use std::cmp;
use std::ptr;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::Address;
use gc::Collector;
use gc::swiper::card::CardTable;

pub mod young;
pub mod old;
pub mod card;

const YOUNG_RATIO: u32 = 5;

pub struct Swiper {
    heap_start: Address,
    heap_end: Address,
    heap_size: usize,
    card_table: CardTable,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size
            .map(|s| *s)
            .unwrap_or(32 * 1024 * 1024);

        let ptr = arena::reserve(heap_size).expect("could not reserve memory");
        let ptr = Address::from_ptr(ptr);

        Swiper {
            heap_start: ptr,
            heap_end: ptr.offset(heap_size),
            heap_size: heap_size,
            card_table: CardTable::new(heap_size),
        }
    }
}

impl Collector for Swiper {
    fn alloc(&self, ctxt: &SemContext, size: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, ctxt: &SemContext) {
        unimplemented!();
    }
}
