use std::fmt;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::Address;
use gc::Collector;
use gc::root::{get_rootset, IndirectObj};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::minor::MinorCollector;
use gc::swiper::young::YoungGen;
use gc::swiper::old::OldGen;
use gc::swiper::verify::Verifier;
use mem;

pub mod card;
mod crossing;
mod minor;
pub mod old;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: u32 = 4;

// heap is divided into cards of size CARD_SIZE.
// card entry determines whether this part of the heap was modified
// in minor collections those parts of the heap need to be analyzed
pub const CARD_SIZE: usize = 512;
pub const CARD_SIZE_BITS: usize = 9;

pub struct Swiper {
    heap: Region,

    young: YoungGen,
    old: OldGen,
    card_table: CardTable,
    crossing_map: CrossingMap,

    card_table_offset: usize,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.flag_heap_size.map(|s| *s).unwrap_or(32 * 1024 * 1024);

        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine sizes of young/old-gen
        let young_size = mem::page_align(heap_size / (YOUNG_RATIO as usize));
        let old_size = heap_size - young_size;

        // determine size for card table
        let card_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        // determine size for crossing map
        let crossing_size = mem::page_align(old_size >> CARD_SIZE_BITS);

        let alloc_size = heap_size + card_size + crossing_size;

        let ptr = arena::reserve(alloc_size).expect("could not reserve heap.");
        arena::commit(ptr, alloc_size).expect("could not commit heap.");

        let heap_start = ptr;
        let heap_end = ptr.offset(heap_size);

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // determine boundaries for card table
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);
        let card_table = CardTable::new(card_start, card_end, young_size);

        // determine boundaries for crossing map
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);
        let crossing_map = CrossingMap::new(crossing_start, crossing_end);

        // determine boundaries of young generation
        let young_start = heap_start;
        let young_end = young_start.offset(young_size);
        let young = YoungGen::new(young_start, young_end);

        // determine boundaries of old generation
        let old_start = heap_start.offset(young_size);
        let old_end = heap_end;
        let old = OldGen::new(old_start, old_end, crossing_map.clone());

        if args.flag_gc_verbose {
            println!("OLD: {}-{}", old_start, old_end);
            println!("YNG: {}-{}", young_start, young_end);
        }

        Swiper {
            heap: Region::new(heap_start, heap_end),

            young: young,
            old: old,
            card_table: card_table,
            crossing_map: crossing_map,

            card_table_offset: card_table_offset,
        }
    }

    fn minor_collect(&self, ctxt: &SemContext) {
        let rootset = get_rootset(ctxt);

        self.verify(ctxt, "pre-minor", &rootset);

        let mut collector = MinorCollector::new(
            ctxt,
            &self.young,
            &self.old,
            &self.card_table,
            &self.crossing_map,
            &rootset,
        );
        collector.collect();

        self.verify(ctxt, "post-minor", &rootset);
    }

    fn verify(&self, ctxt: &SemContext, _name: &str, rootset: &[IndirectObj]) {
        if ctxt.args.flag_gc_verify {
            if ctxt.args.flag_gc_verbose {
                println!("VERIFY: {}", _name);
            }

            let mut verifier = Verifier::new(
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
            );
            verifier.verify();
        }
    }
}

impl Collector for Swiper {
    fn alloc_obj(&self, ctxt: &SemContext, size: usize) -> *const u8 {
        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.minor_collect(ctxt);

        self.young.alloc(size)
    }

    fn alloc_array(
        &self,
        _ctxt: &SemContext,
        _elements: usize,
        _element_size: usize,
        _is_ref: bool,
    ) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, ctxt: &SemContext) {
        self.minor_collect(ctxt);
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }
}

#[derive(Clone)]
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
    fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}