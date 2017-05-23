use std::cmp;
use std::ptr;

use ctxt::Context;
use driver::cmd::Args;
use gc::arena;
use gc::Collector;
use gc::swiper::card::CardTable;

pub mod young;
pub mod old;
pub mod card;

const YOUNG_RATIO: u32 = 5;

pub struct Swiper {
    heap_start: *const u8,
    heap_end: *const u8,
    heap_size: usize,
    card_table: CardTable,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size
            .map(|s| *s)
            .unwrap_or(32 * 1024 * 1024);

        let ptr = arena::reserve(heap_size).expect("could not reserve memory");

        Swiper {
            heap_start: ptr,
            heap_end: unsafe { ptr.offset(heap_size as isize) },
            heap_size: heap_size,
            card_table: CardTable::new(heap_size),
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
