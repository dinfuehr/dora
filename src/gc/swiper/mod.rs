use parking_lot::Mutex;
use scoped_threadpool::Pool;
use std::fmt;
use std::mem::size_of;
use std::sync::Arc;

use ctxt::VM;
use driver::cmd::Args;
use gc::root::{get_rootset, Slot};
use gc::swiper::card::CardTable;
use gc::swiper::controller::{HeapConfig, SharedHeapConfig};
use gc::swiper::crossing::CrossingMap;
use gc::swiper::full::FullCollector;
use gc::swiper::large::LargeSpace;
use gc::swiper::minor::MinorCollector;
use gc::swiper::old::OldGen;
use gc::swiper::pfull::ParallelFullCollector;
use gc::swiper::pminor::ParallelMinorCollector;
use gc::swiper::verify::{Verifier, VerifierPhase};
use gc::swiper::young::YoungGen;
use gc::tlab;
use gc::Collector;
use gc::{align_gen, formatted_size, Address, Region, K};
use gc::{arena, GcReason};
use mem;
use safepoint;

pub mod card;
mod controller;
mod crossing;
mod full;
mod large;
mod marking;
mod minor;
pub mod old;
mod paged_old;
mod pfull;
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
pub const CARD_REFS: usize = CARD_SIZE / size_of::<usize>();

pub const LARGE_OBJECT_SIZE: usize = 16 * K;

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

    threadpool: Mutex<Pool>,
    config: SharedHeapConfig,
}

impl Swiper {
    pub fn new(args: &Args) -> Swiper {
        let max_heap_size = align_gen(args.max_heap_size());
        let min_heap_size = align_gen(args.min_heap_size());

        let mut config = HeapConfig::new(min_heap_size, max_heap_size);

        controller::init(&mut config, args);

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

        let eden_size = config.eden_size;
        let semi_size = config.semi_size;

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
        let young = YoungGen::new(young, eden_size, semi_size, args.flag_gc_verify);

        let config = Arc::new(Mutex::new(config));
        let old = OldGen::new(
            old_start,
            old_end,
            crossing_map.clone(),
            card_table.clone(),
            config.clone(),
        );
        let large = LargeSpace::new(large_start, large_end, config.clone());

        if args.flag_gc_verbose {
            println!(
                "GC: heap info: {}, eden {}, semi {}, card {}, crossing {}",
                formatted_size(max_heap_size),
                formatted_size(eden_size),
                formatted_size(semi_size),
                formatted_size(card_size),
                formatted_size(crossing_size)
            );
        }

        let nworkers = args.gc_workers();

        Swiper {
            heap: Region::new(heap_start, heap_end),
            reserved_area: reserved_area,

            young: young,
            old: old,
            large: large,

            card_table: card_table,
            crossing_map: crossing_map,
            config: config,

            card_table_offset: card_table_offset,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            threadpool: Mutex::new(Pool::new(nworkers as u32)),
        }
    }

    fn perform_collection_and_choose(&self, vm: &VM, reason: GcReason) -> CollectionKind {
        let kind = controller::choose_collection_kind(&self.config, &vm.args, &self.young);
        self.perform_collection(vm, kind, reason)
    }

    fn perform_collection(
        &self,
        vm: &VM,
        kind: CollectionKind,
        mut reason: GcReason,
    ) -> CollectionKind {
        safepoint::stop_the_world(vm, |threads| {
            controller::start(&self.config, &self.young, &self.old, &self.large);

            tlab::make_iterable_all(vm, threads);
            let rootset = get_rootset(vm, threads);

            let kind = match kind {
                CollectionKind::Minor => {
                    let promotion_failed = self.minor_collect(vm, reason, &rootset);

                    if promotion_failed {
                        reason = GcReason::PromotionFailure;
                        self.full_collect(vm, reason, &rootset);
                        CollectionKind::Full
                    } else {
                        CollectionKind::Minor
                    }
                }

                CollectionKind::Full => {
                    self.full_collect(vm, reason, &rootset);
                    CollectionKind::Full
                }
            };

            controller::stop(
                &self.config,
                kind,
                &self.young,
                &self.old,
                &self.large,
                &vm.args,
                reason,
            );

            kind
        })
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason, rootset: &[Slot]) -> bool {
        self.verify(vm, VerifierPhase::PreMinor, "pre-minor", &rootset, false);

        let promotion_failed = if vm.args.flag_gc_parallel_minor {
            let mut pool = self.threadpool.lock();
            let mut collector = ParallelMinorCollector::new(
                vm,
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                rootset,
                reason,
                self.min_heap_size,
                self.max_heap_size,
                &mut pool,
                &self.config,
            );

            collector.collect()
        } else {
            let mut collector = MinorCollector::new(
                vm,
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                rootset,
                reason,
                self.min_heap_size,
                self.max_heap_size,
                &self.config,
            );

            collector.collect()
        };

        self.verify(
            vm,
            VerifierPhase::PostMinor,
            "post-minor",
            &rootset,
            promotion_failed,
        );

        promotion_failed
    }

    fn full_collect(&self, vm: &VM, reason: GcReason, rootset: &[Slot]) {
        self.verify(
            vm,
            VerifierPhase::PreFull,
            "pre-full",
            &rootset,
            reason == GcReason::PromotionFailure,
        );

        if vm.args.flag_gc_parallel_full {
            let mut pool = self.threadpool.lock();
            let mut collector = ParallelFullCollector::new(
                vm,
                self.heap.clone(),
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                &vm.gc.perm_space,
                rootset,
                reason,
                &mut pool,
                self.min_heap_size,
                self.max_heap_size,
            );
            collector.collect();
        } else {
            let mut collector = FullCollector::new(
                vm,
                self.heap.clone(),
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                &vm.gc.perm_space,
                rootset,
                reason,
                self.min_heap_size,
                self.max_heap_size,
            );
            collector.collect();
        }

        self.verify(vm, VerifierPhase::PostFull, "post-full", &rootset, false);
    }

    fn verify(
        &self,
        vm: &VM,
        phase: VerifierPhase,
        _name: &str,
        rootset: &[Slot],
        promotion_failed: bool,
    ) {
        if vm.args.flag_gc_verify {
            if vm.args.flag_gc_dev_verbose {
                println!("GC: Verify {}", _name);
            }

            let perm_space = &vm.gc.perm_space;

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
                promotion_failed,
            );
            verifier.verify();

            if vm.args.flag_gc_dev_verbose {
                println!("GC: Verify {} finished", _name);
            }
        }
    }
}

impl Collector for Swiper {
    fn alloc_tlab_area(&self, vm: &VM, size: usize) -> Option<Region> {
        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return Some(ptr.region_start(size));
        }

        self.perform_collection_and_choose(vm, GcReason::AllocationFailure);

        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return Some(ptr.region_start(size));
        }

        self.perform_collection(vm, CollectionKind::Full, GcReason::AllocationFailure);
        let ptr = self.young.bump_alloc(size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn alloc_normal(&self, vm: &VM, size: usize, _array_ref: bool) -> Address {
        let ptr = self.young.bump_alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.perform_collection_and_choose(vm, GcReason::AllocationFailure);

        self.young.bump_alloc(size)
    }

    fn alloc_large(&self, vm: &VM, size: usize, _: bool) -> Address {
        let ptr = self.large.alloc(size);

        if !ptr.is_null() {
            return ptr;
        }

        self.perform_collection(vm, CollectionKind::Full, GcReason::AllocationFailure);

        self.large.alloc(size)
    }

    fn collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Full, reason);
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Minor, reason);
    }

    fn needs_write_barrier(&self) -> bool {
        return true;
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }

    fn dump_summary(&self, runtime: f32) {
        let config = self.config.lock();
        let total_gc = config.total_minor_pause + config.total_full_pause;
        let gc_percentage = ((total_gc / runtime) * 100.0).round();
        let mutator_percentage = 100.0 - gc_percentage;

        println!(
            "GC summary: {:.1}ms minor ({}), {:.1}ms full ({}), {:.1}ms runtime ({}% mutator, {}% GC)",
            config.total_minor_pause,
            config.total_minor_collections,
            config.total_full_pause,
            config.total_full_collections,
            runtime,
            mutator_percentage,
            gc_percentage,
        );
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

#[derive(Copy, Clone)]
pub enum CollectionKind {
    Minor,
    Full,
}

impl fmt::Display for CollectionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            CollectionKind::Minor => "Minor",
            CollectionKind::Full => "Full",
        };

        write!(f, "{}", name)
    }
}
