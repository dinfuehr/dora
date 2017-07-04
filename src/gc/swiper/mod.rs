use std::cmp;
use std::ptr;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::Address;
use gc::Collector;
use gc::swiper::card::CardTable;
use mem;

pub mod young;
pub mod old;
pub mod card;

const YOUNG_RATIO: u32 = 5;

// size of card is 128 bytes
// although stored in constant
// it is not really expected to change
const CARD_SIZE: usize = 128;
const CARD_SIZE_BITS: usize = 7;

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
        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine size for card table
        let card_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        let alloc_size = heap_size + card_size;

        let ptr = arena::reserve(alloc_size).expect("could not reserve memory");
        let ptr = Address::from_ptr(ptr);

        let heap_end = ptr.offset(heap_size);

        Swiper {
            heap_start: ptr,
            heap_end: heap_end,
            heap_size: heap_size,
            card_table: CardTable::new(heap_end, heap_end.offset(card_size)),
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
