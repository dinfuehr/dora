use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::Address;
use gc::Collector;
use gc::swiper::card::CardTable;
use gc::swiper::young::YoungGen;
use gc::swiper::old::OldGen;
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
    heap: Region,

    young: YoungGen,
    old: OldGen,
    card_table: CardTable,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size.map(|s| *s).unwrap_or(32 * 1024 * 1024);

        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine size for card table
        let card_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        let alloc_size = heap_size + card_size;

        let ptr = arena::reserve(alloc_size).expect("could not reserve memory");
        let ptr = Address::from_ptr(ptr);

        let heap_start = ptr;
        let heap_end = ptr.offset(heap_size);

        // determine boundaries of young generation
        let young_start = heap_start;
        let young_size = mem::page_align(heap_size / (YOUNG_RATIO as usize));
        let young_end = young_start.offset(young_size);
        let young = YoungGen::new(young_start, young_end);

        // determine boundaries of old generation
        let old_start = heap_start.offset(young_size);
        let old_end = heap_end;
        let old = OldGen::new(old_start, old_end);

        Swiper {
            heap: Region::new(heap_start, heap_end),

            young: young,
            old: old,
            card_table: CardTable::new(heap_end, heap_end.offset(card_size)),
        }
    }
}

impl Collector for Swiper {
    fn alloc(&self, _: &SemContext, _: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, _: &SemContext) {
        unimplemented!();
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }
}

pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    fn new(start: Address, end: Address) -> Region {
        Region {
            start: start,
            end: end,
        }
    }

    fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}