use std::fmt;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::arena;
use gc::root::{get_rootset, IndirectObj};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::full::FullCollector;
use gc::swiper::large::LargeSpace;
use gc::swiper::minor::MinorCollector;
use gc::swiper::old::OldGen;
use gc::swiper::verify::{Verifier, VerifierPhase};
use gc::swiper::young::YoungGen;
use gc::Address;
use gc::Collector;
use mem;
use os;

pub mod card;
mod crossing;
mod full;
mod large;
mod minor;
pub mod old;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: usize = 4;

// heap is divided into cards of size CARD_SIZE.
// card entry determines whether this part of the heap was modified
// in minor collections those parts of the heap need to be analyzed
pub const CARD_SIZE: usize = 512;
pub const CARD_SIZE_BITS: usize = 9;

pub const LARGE_OBJECT_SIZE: usize = 16 * 1024;

pub struct Swiper {
    heap: Region,

    young: YoungGen,
    old: OldGen,
    large: LargeSpace,

    card_table: CardTable,
    crossing_map: CrossingMap,

    card_table_offset: usize,

    // maximum heap size
    heap_size: usize,

    // committed memory size
    committed_size: usize,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let heap_size = args.heap_size();

        // set heap size to multiple of page
        let heap_size = mem::page_align(heap_size);

        // determine sizes of young/old-gen
        let alignment = os::page_size() as usize * 2;
        let young_size = mem::align_usize(heap_size / YOUNG_RATIO, alignment);
        let old_size = heap_size - young_size;

        // determine size for card table
        let card_size = mem::page_align((4 * heap_size) >> CARD_SIZE_BITS);

        // determine size for crossing map
        let crossing_size = mem::page_align(heap_size >> CARD_SIZE_BITS);

        let heap_reserve_size = heap_size * 4 + card_size + crossing_size;

        let ptr = arena::reserve(heap_reserve_size).expect("could not reserve heap.");

        let heap_start = ptr;
        let heap_end = ptr.offset(4 * heap_size);

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // determine boundaries for card table
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);

        arena::commit(card_start, card_size, false).expect("could not commit card table.");
        let card_table = CardTable::new(card_start, card_end, heap_size);

        // determine boundaries for crossing map
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);

        arena::commit(crossing_start, crossing_size, false)
            .expect("could not commit crossing table.");
        let crossing_map = CrossingMap::new(crossing_start, crossing_end);

        // determine boundaries of young generation
        let young_start = heap_start;
        let young_end = young_start.offset(heap_size);
        let young = YoungGen::new(young_start, young_end);

        arena::commit(young_start, heap_size, false).expect("could not commit young gen.");

        // determine boundaries of old generation
        let old_start = young_end;
        let old_end = old_start.offset(heap_size);
        let old = OldGen::new(old_start, old_end, crossing_map.clone());

        arena::commit(old_start, heap_size, false).expect("could not commit old gen.");

        // determine large object space
        let large_start = old_end;
        let large_end = large_start.offset(2 * heap_size);
        let large = LargeSpace::new(large_start, large_end);

        if args.flag_gc_verbose {
            println!(
                "GC: heap info: {:.1}K, old {:.1}K, young {:.1}K, card {:.1}K, crossing {:.1}K",
                in_kilo(heap_size),
                in_kilo(old_size),
                in_kilo(young_size),
                in_kilo(card_size),
                in_kilo(crossing_size)
            );
        }

        Swiper {
            heap: Region::new(heap_start, heap_end),

            young: young,
            old: old,
            large: large,

            card_table: card_table,
            crossing_map: crossing_map,

            card_table_offset: card_table_offset,
            heap_size: heap_size,
            committed_size: 0,
        }
    }

    fn minor_collect_inner(&self, ctxt: &SemContext) -> bool {
        let rootset = get_rootset(ctxt);

        self.verify(ctxt, VerifierPhase::PreMinor, "pre-minor", &rootset);

        let mut collector = MinorCollector::new(
            ctxt,
            &self.young,
            &self.old,
            &self.card_table,
            &self.crossing_map,
            &rootset,
        );
        collector.collect();

        let promotion_failed = collector.promotion_failed();
        self.verify(ctxt, VerifierPhase::PostMinor, "post-minor", &rootset);

        promotion_failed
    }

    fn full_collect(&self, ctxt: &SemContext) {
        let rootset = get_rootset(ctxt);

        self.verify(ctxt, VerifierPhase::PreFull, "pre-full", &rootset);

        let mut collector = FullCollector::new(
            ctxt,
            self.heap.clone(),
            &self.young,
            &self.old,
            &self.large,
            &self.card_table,
            &self.crossing_map,
            &ctxt.gc.perm_space,
            &rootset,
        );
        collector.collect();

        self.verify(ctxt, VerifierPhase::PostFull, "post-full", &rootset);
    }

    fn verify(
        &self,
        ctxt: &SemContext,
        phase: VerifierPhase,
        _name: &str,
        rootset: &[IndirectObj],
    ) {
        if ctxt.args.flag_gc_verify {
            if ctxt.args.flag_gc_verbose {
                println!("VERIFY: {}", _name);
            }

            let perm_space = &ctxt.gc.perm_space;

            let mut verifier = Verifier::new(
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
                &*perm_space,
                phase,
            );
            verifier.verify();
        }
    }
}

impl Collector for Swiper {
    fn alloc(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> *const u8 {
        if ctxt.args.flag_gc_stress_minor {
            self.minor_collect(ctxt);
        }

        if ctxt.args.flag_gc_stress {
            self.full_collect(ctxt);
        }

        if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(ctxt, size, array_ref)
        } else {
            self.alloc_large(ctxt, size, array_ref)
        }
    }

    fn collect(&self, ctxt: &SemContext) {
        self.full_collect(ctxt);
    }

    fn minor_collect(&self, ctxt: &SemContext) {
        self.minor_collect_inner(ctxt);
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }
}

impl Swiper {
    fn alloc_normal(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> *const u8 {
        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        let promotion_failed = self.minor_collect_inner(ctxt);

        if promotion_failed {
            self.full_collect(ctxt);
        }

        let ptr = self.young.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.old.alloc(size, array_ref)
    }

    fn alloc_large(&self, ctxt: &SemContext, size: usize, _: bool) -> *const u8 {
        let ptr = self.large.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.full_collect(ctxt);

        self.large.alloc(size)
    }
}

#[derive(Clone)]
pub struct Region {
    pub start: Address,
    pub end: Address,
}

impl Region {
    pub fn new(start: Address, end: Address) -> Region {
        Region {
            start: start,
            end: end,
        }
    }

    #[inline(always)]
    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }

    #[inline(always)]
    pub fn valid_top(&self, addr: Address) -> bool {
        self.start <= addr && addr <= self.end
    }

    #[inline(always)]
    pub fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

fn in_kilo(size: usize) -> f64 {
    (size as f64) / 1024.0
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

fn start_of_card(addr: Address) -> bool {
    (addr.to_usize() & (CARD_SIZE - 1)) == addr.to_usize()
}
