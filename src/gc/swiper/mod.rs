use std::sync::Mutex;

use threadpool::ThreadPool;

use driver::cmd::Args;
use gc::root::{get_rootset, Slot};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::full::FullCollector;
use gc::swiper::large::LargeSpace;
use gc::swiper::minor::MinorCollector;
use gc::swiper::old::OldGen;
use gc::swiper::pminor::ParMinorCollector;
use gc::swiper::verify::{Verifier, VerifierPhase};
use gc::swiper::young::YoungGen;
use gc::tlab;
use gc::Collector;
use gc::{align_gen, arena, GcReason};
use gc::{formatted_size, Address, Region};
use mem;
use vm::VM;

pub mod card;
mod controller;
mod crossing;
mod full;
mod large;
mod marking;
mod minor;
pub mod old;
mod paged_old;
mod pminor;
mod verify;
pub mod young;

// determines size of young generation in heap
// young generation size = heap size / YOUNG_RATIO
const YOUNG_RATIO: usize = 2;

// heap is divided into cards of size CARD_SIZE.
// card entry determines whether this part of the heap was modified
// in minor collections those parts of the heap need to be analyzed
pub const CARD_SIZE: usize = 512;
pub const CARD_SIZE_BITS: usize = 9;

pub const LARGE_OBJECT_SIZE: usize = 16 * 1024;

pub struct Swiper {
    // contiguous memory for young/old generation and large space
    heap: Region,

    // contains heap and also card table and crossing map
    reserved_area: Region,

    young: YoungGen,
    old: OldGen,
    large: LargeSpace,

    card_table: CardTable,
    crossing_map: CrossingMap,

    card_table_offset: usize,

    // minimum & maximum heap size
    min_heap_size: usize,
    max_heap_size: usize,

    threadpool: ThreadPool,

    stats: GcStats,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let min_heap_size = args.min_heap_size();
        let max_heap_size = args.max_heap_size();

        // determine initial young-ratio
        let mut young_ratio = args.flag_gc_young_ratio.unwrap_or(YOUNG_RATIO);

        if young_ratio == 0 {
            young_ratio = YOUNG_RATIO;
        }

        // determine size for generations
        let young_size = align_gen(max_heap_size / young_ratio);
        let old_size = align_gen(max_heap_size - young_size);

        let max_heap_size = young_size + old_size;

        // determine size for card table
        let card_size = mem::page_align((4 * max_heap_size) >> CARD_SIZE_BITS);

        // determine size for crossing map
        let crossing_size = mem::page_align(max_heap_size >> CARD_SIZE_BITS);

        // determine full memory size
        let reserve_size = max_heap_size * 4 + card_size + crossing_size;

        // reserve full memory
        let ptr = arena::reserve(reserve_size);

        // heap is young/old generation & large space
        let heap_start = ptr;
        let heap_end = ptr.offset(4 * max_heap_size);

        // reserved area also contains card table & crossing map
        let reserved_area = heap_start.region_start(reserve_size);

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // determine boundaries for card table
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);

        arena::commit(card_start, card_size, false);

        // determine boundaries for crossing map
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);

        arena::commit(crossing_start, crossing_size, false);

        // determine boundaries of young generation
        let young_start = heap_start;
        let young_end = young_start.offset(max_heap_size);
        let young = Region::new(young_start, young_end);

        // determine boundaries of old generation
        let old_start = young_end;
        let old_end = old_start.offset(max_heap_size);

        // determine large object space
        let large_start = old_end;
        let large_end = large_start.offset(2 * max_heap_size);

        let card_table = CardTable::new(
            card_start,
            card_end,
            Region::new(old_start, large_end),
            old_end,
            max_heap_size,
        );
        let crossing_map = CrossingMap::new(crossing_start, crossing_end, max_heap_size);
        let young = YoungGen::new(young, young_size, args.flag_gc_verify);
        let old = OldGen::new(
            old_start,
            old_end,
            old_size,
            crossing_map.clone(),
            card_table.clone(),
        );
        let large = LargeSpace::new(large_start, large_end);

        if args.flag_gc_verbose {
            println!(
                "GC: heap info: {}, old {}, young {}, card {}, crossing {}",
                formatted_size(max_heap_size),
                formatted_size(old_size),
                formatted_size(young_size),
                formatted_size(card_size),
                formatted_size(crossing_size)
            );
        }

        let nworkers = if args.flag_gc_worker > 1 {
            args.flag_gc_worker
        } else {
            1
        };

        Swiper {
            heap: Region::new(heap_start, heap_end),
            reserved_area: reserved_area,

            young: young,
            old: old,
            large: large,

            card_table: card_table,
            crossing_map: crossing_map,

            card_table_offset: card_table_offset,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            threadpool: ThreadPool::with_name("gc-worker".to_string(), nworkers),
            stats: GcStats::new(),
        }
    }

    fn minor_collect_inner(&self, ctxt: &VM, reason: GcReason) -> bool {
        // make heap iterable
        tlab::make_iterable(ctxt);
        let rootset = get_rootset(ctxt);

        self.verify(ctxt, VerifierPhase::PreMinor, "pre-minor", &rootset);

        let promotion_failed = if ctxt.args.flag_gc_parallel_minor {
            self.par_minor_collect(ctxt, reason, &rootset)
        } else {
            self.serial_minor_collect(ctxt, reason, &rootset)
        };

        self.verify(ctxt, VerifierPhase::PostMinor, "post-minor", &rootset);

        promotion_failed
    }

    fn serial_minor_collect(&self, ctxt: &VM, reason: GcReason, rootset: &[Slot]) -> bool {
        let mut collector = MinorCollector::new(
            ctxt,
            &self.young,
            &self.old,
            &self.large,
            &self.card_table,
            &self.crossing_map,
            rootset,
            reason,
            self.min_heap_size,
            self.max_heap_size,
            &self.stats,
        );
        collector.collect()
    }

    fn par_minor_collect(&self, ctxt: &VM, reason: GcReason, rootset: &[Slot]) -> bool {
        let mut collector = ParMinorCollector::new(
            ctxt,
            &self.young,
            &self.old,
            &self.large,
            &self.card_table,
            &self.crossing_map,
            rootset,
            reason,
            ctxt.args.flag_gc_worker,
        );
        collector.collect()
    }

    fn full_collect(&self, ctxt: &VM, reason: GcReason) {
        // make heap iterable
        tlab::make_iterable(ctxt);

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
            reason,
            &self.threadpool,
            self.min_heap_size,
            self.max_heap_size,
            &self.stats,
        );
        collector.collect();

        self.verify(ctxt, VerifierPhase::PostFull, "post-full", &rootset);
    }

    fn verify(&self, ctxt: &VM, phase: VerifierPhase, _name: &str, rootset: &[Slot]) {
        if ctxt.args.flag_gc_verify {
            if ctxt.args.flag_gc_dev_verbose {
                println!("GC: Verify {}", _name);
            }

            let perm_space = &ctxt.gc.perm_space;

            let mut verifier = Verifier::new(
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
                &self.large,
                &*perm_space,
                self.reserved_area.clone(),
                phase,
            );
            verifier.verify();

            if ctxt.args.flag_gc_dev_verbose {
                println!("GC: Verify {} finished", _name);
            }
        }
    }
}

impl Collector for Swiper {
    fn alloc_tlab_area(&self, ctxt: &VM, size: usize) -> Option<Region> {
        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return Some(ptr.region_start(size));
        }

        let promotion_failed = self.minor_collect_inner(ctxt, GcReason::AllocationFailure);

        if promotion_failed {
            self.full_collect(ctxt, GcReason::PromotionFailure);
            let ptr = self.young.bump_alloc(size);

            return if ptr.is_null() {
                None
            } else {
                Some(ptr.region_start(size))
            };
        }

        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return Some(ptr.region_start(size));
        }

        self.full_collect(ctxt, GcReason::AllocationFailure);
        let ptr = self.young.bump_alloc(size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn alloc_normal(&self, ctxt: &VM, size: usize, array_ref: bool) -> Address {
        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        let promotion_failed = self.minor_collect_inner(ctxt, GcReason::AllocationFailure);

        if promotion_failed {
            self.full_collect(ctxt, GcReason::PromotionFailure);
        }

        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.old.bump_alloc(size, array_ref)
    }

    fn alloc_large(&self, ctxt: &VM, size: usize, _: bool) -> Address {
        let ptr = self.large.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.full_collect(ctxt, GcReason::AllocationFailure);

        self.large.alloc(size)
    }

    fn collect(&self, ctxt: &VM, reason: GcReason) {
        self.full_collect(ctxt, reason);
    }

    fn minor_collect(&self, ctxt: &VM, reason: GcReason) {
        tlab::make_iterable(ctxt);
        self.minor_collect_inner(ctxt, reason);
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }

    fn dump_summary(&self, runtime: f32) {
        self.stats.update(|stats| {
            let total_gc = stats.minor_runtime() + stats.full_runtime();
            let gc_percentage = ((total_gc / runtime) * 100.0).round();
            let mutator_percentage = 100.0 - gc_percentage;

            println!(
                "GC summary: {:.1}ms minor ({}), {:.1}ms full ({}), {:.1}ms runtime ({}% mutator, {}% GC)",
                stats.minor_runtime(),
                stats.minor_collections(),
                stats.full_runtime(),
                stats.full_collections(),
                runtime,
                mutator_percentage,
                gc_percentage,
            );
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CardIdx(usize);

impl CardIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }

    pub fn offset(self, val: usize) -> CardIdx {
        (self.0 + val).into()
    }
}

impl From<usize> for CardIdx {
    fn from(val: usize) -> CardIdx {
        CardIdx(val)
    }
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

pub struct GcStats {
    stats: Mutex<GcStatsProtected>,
}

impl GcStats {
    fn new() -> GcStats {
        GcStats {
            stats: Mutex::new(GcStatsProtected::new()),
        }
    }

    fn update<F>(&self, fct: F)
    where
        F: FnOnce(&mut GcStatsProtected),
    {
        let mut stats = self.stats.lock().unwrap();
        fct(&mut *stats);
    }
}

struct GcStatsProtected {
    // number of minor GCs
    minor_collections: usize,

    // total runtime of minor GCs
    minor_runtime: f32,

    // number of full GCs
    full_collections: usize,

    // total runtime of full GCs
    full_runtime: f32,
}

impl GcStatsProtected {
    fn new() -> GcStatsProtected {
        GcStatsProtected {
            minor_collections: 0,
            minor_runtime: 0.0,
            full_collections: 0,
            full_runtime: 0.0,
        }
    }

    fn incr_minor_collections(&mut self) {
        self.minor_collections += 1;
    }

    fn minor_collections(&self) -> usize {
        self.minor_collections
    }

    fn incr_minor_runtime(&mut self, time: f32) {
        self.minor_runtime += time;
    }

    fn minor_runtime(&self) -> f32 {
        self.minor_runtime
    }

    fn incr_full_collections(&mut self) {
        self.full_collections += 1;
    }

    fn full_collections(&self) -> usize {
        self.full_collections
    }

    fn incr_full_runtime(&mut self, time: f32) {
        self.full_runtime += time;
    }

    fn full_runtime(&self) -> f32 {
        self.full_runtime
    }
}
