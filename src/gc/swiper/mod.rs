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

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: u32 = 5;

// size of card is 128 bytes
// although stored in constant
// it is not really expected to change
pub const CARD_SIZE: usize = 512;
pub const CARD_SIZE_BITS: usize = 9;

// the number of collections an object remains in
// young generation
pub const PROMOTION_AGE: u32 = 4;

pub struct Swiper {
    heap: Region,

    young: YoungGen,
    old: OldGen,
    card_table: CardTable,

    card_table_offset: usize,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size.map(|s| *s).unwrap_or(32 * 1024 * 1024);

        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine size for card table
        let card_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        // determine size for crossing map
        // allocate crossings for whole heap, although we should only
        // need it for the old generation
        let crossing_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

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

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        Swiper {
            heap: Region::new(heap_start, heap_end),

            young: young,
            old: old,
            card_table: CardTable::new(heap_end, heap_end.offset(card_size)),

            card_table_offset: card_table_offset,
        }
    }
}

impl Collector for Swiper {
    fn alloc(&self, _: &SemContext, size: usize) -> *const u8 {
        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        // TODO: invoke collect
        // self.young.collect(ctxt);

        self.young.alloc(size)
    }

    fn collect(&self, _: &SemContext) {
        unimplemented!();
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
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

    #[inline(always)]
    fn includes(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}
