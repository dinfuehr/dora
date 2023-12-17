use parking_lot::Mutex;
use scoped_threadpool::Pool;
use std::fmt;
use std::mem::size_of;
use std::sync::Arc;

use crate::gc::root::{determine_strong_roots, Slot};
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::{HeapConfig, SharedHeapConfig};
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::full::FullCollector;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::minor::MinorCollector;
use crate::gc::swiper::old::OldGen;
use crate::gc::swiper::verify::{Verifier, VerifierPhase};
use crate::gc::swiper::young::YoungGen;
use crate::gc::tlab;
use crate::gc::Collector;
use crate::gc::GcReason;
use crate::gc::{align_page_up, formatted_size, Address, Region, K};
use crate::mem;
use crate::object::Obj;
use crate::os::{self, MemoryPermission, Reservation};
use crate::safepoint;
use crate::threads::DoraThread;
use crate::vm::{Flags, VM};

pub mod card;
mod controller;
mod crossing;
mod full;
mod large;
mod minor;
pub mod old;
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

pub const LARGE_OBJECT_SIZE: usize = 64 * K;
pub const PAGE_SIZE: usize = 128 * K;

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
    emit_write_barrier: bool,

    // minimum & maximum heap size
    min_heap_size: usize,
    max_heap_size: usize,

    threadpool: Mutex<Pool>,
    config: SharedHeapConfig,

    reservation: Reservation,
}

impl Swiper {
    pub fn new(args: &Flags) -> Swiper {
        let max_heap_size = align_page_up(args.max_heap_size());
        let min_heap_size = align_page_up(args.min_heap_size());

        let mut config = HeapConfig::new(min_heap_size, max_heap_size);

        controller::init(&mut config, args);

        // determine size for card table
        let card_size = mem::page_align((4 * max_heap_size) >> CARD_SIZE_BITS);

        // determine size for crossing map
        let crossing_size = mem::page_align(max_heap_size >> CARD_SIZE_BITS);

        // determine full memory size
        let reserve_size = max_heap_size * 4 + card_size + crossing_size;

        // reserve full memory
        let reservation = os::reserve_align(reserve_size, PAGE_SIZE, false);
        let heap_start = reservation.start();
        assert!(heap_start.is_gen_aligned());

        // heap is young/old generation & large space
        let heap_end = heap_start.offset(4 * max_heap_size);

        // reserved area also contains card table & crossing map
        let reserved_area = heap_start.region_start(reserve_size);

        // determine offset to card table (card table starts right after heap)
        // offset = card_table_start - (heap_start >> CARD_SIZE_BITS)
        let card_table_offset = heap_end.to_usize() - (heap_start.to_usize() >> CARD_SIZE_BITS);

        // determine boundaries for card table
        let card_start = heap_end;
        let card_end = card_start.offset(card_size);

        os::commit_at(card_start, card_size, MemoryPermission::ReadWrite);

        // determine boundaries for crossing map
        let crossing_start = card_end;
        let crossing_end = crossing_start.offset(crossing_size);

        os::commit_at(crossing_start, crossing_size, MemoryPermission::ReadWrite);

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
        let young = YoungGen::new(young, semi_size, args.gc_verify);

        let config = Arc::new(Mutex::new(config));
        let old = OldGen::new(
            old_start,
            old_end,
            crossing_map.clone(),
            card_table.clone(),
            config.clone(),
        );
        let large = LargeSpace::new(large_start, large_end, config.clone());

        if args.gc_verbose {
            println!(
                "GC: heap info: {}, eden {}, semi {}, card {}, crossing {} (region {})",
                formatted_size(max_heap_size),
                formatted_size(eden_size),
                formatted_size(semi_size),
                formatted_size(card_size),
                formatted_size(crossing_size),
                Region::new(heap_start, heap_end),
            );
        }

        let nworkers = args.gc_workers();

        let emit_write_barrier = !args.disable_barrier;

        let threadpool = Mutex::new(Pool::new(nworkers as u32));

        Swiper {
            heap: Region::new(heap_start, heap_end),
            reserved_area,
            reservation,

            young,
            old,
            large,

            card_table,
            crossing_map,
            config,

            card_table_offset,
            emit_write_barrier,

            min_heap_size,
            max_heap_size,

            threadpool,
        }
    }

    fn perform_collection_and_choose(&self, vm: &VM, reason: GcReason) -> CollectionKind {
        let kind = controller::choose_collection_kind(&self.config, &vm.flags, &self.young);
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
            let rootset = determine_strong_roots(vm, threads);

            let kind = match kind {
                CollectionKind::Minor => {
                    let promotion_failed = self.minor_collect(vm, reason, &rootset, threads);

                    if promotion_failed {
                        reason = GcReason::PromotionFailure;
                        self.full_collect(vm, reason, threads, &rootset);
                        CollectionKind::Full
                    } else {
                        CollectionKind::Minor
                    }
                }

                CollectionKind::Full => {
                    self.full_collect(vm, reason, threads, &rootset);
                    CollectionKind::Full
                }
            };

            controller::stop(
                &self.config,
                kind,
                &self.young,
                &self.old,
                &self.large,
                &vm.flags,
                reason,
            );

            kind
        })
    }

    fn minor_collect(
        &self,
        vm: &VM,
        reason: GcReason,
        rootset: &[Slot],
        threads: &[Arc<DoraThread>],
    ) -> bool {
        self.verify(
            vm,
            VerifierPhase::PreMinor,
            CollectionKind::Minor,
            "pre-minor",
            &rootset,
            false,
            Address::null(),
        );

        let init_old_top = self.old.protected().top;

        let promotion_failed = {
            let mut pool = self.threadpool.lock();
            let mut collector = MinorCollector::new(
                vm,
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                rootset,
                threads,
                reason,
                self.min_heap_size,
                self.max_heap_size,
                &mut pool,
                &self.config,
            );

            let promotion_failed = collector.collect();

            if vm.flags.gc_stats {
                let mut config = self.config.lock();
                config.add_minor(collector.phases());
            }

            promotion_failed
        };

        self.verify(
            vm,
            VerifierPhase::PostMinor,
            CollectionKind::Minor,
            "post-minor",
            &rootset,
            promotion_failed,
            init_old_top,
        );

        promotion_failed
    }

    fn full_collect(
        &self,
        vm: &VM,
        reason: GcReason,
        threads: &[Arc<DoraThread>],
        rootset: &[Slot],
    ) {
        self.verify(
            vm,
            VerifierPhase::PreFull,
            CollectionKind::Full,
            "pre-full",
            &rootset,
            reason == GcReason::PromotionFailure,
            Address::null(),
        );

        {
            let mut collector = FullCollector::new(
                vm,
                self.heap.clone(),
                &self.young,
                &self.old,
                &self.large,
                &self.card_table,
                &self.crossing_map,
                &vm.gc.readonly_space,
                rootset,
                threads,
                reason,
                self.min_heap_size,
                self.max_heap_size,
            );
            collector.collect();

            if vm.flags.gc_stats {
                let mut config = self.config.lock();
                config.add_full(collector.phases());
            }
        }

        self.verify(
            vm,
            VerifierPhase::PostFull,
            CollectionKind::Full,
            "post-full",
            &rootset,
            false,
            Address::null(),
        );
    }

    fn verify(
        &self,
        vm: &VM,
        phase: VerifierPhase,
        _kind: CollectionKind,
        name: &str,
        rootset: &[Slot],
        promotion_failed: bool,
        init_old_top: Address,
    ) {
        if vm.flags.gc_verify {
            if vm.flags.gc_dev_verbose {
                println!("GC: Verify {}", name);
            }

            let readonly_space = &vm.gc.readonly_space;

            let mut verifier = Verifier::new(
                &self.young,
                &self.old,
                &self.card_table,
                &self.crossing_map,
                rootset,
                &self.large,
                &*readonly_space,
                self.reserved_area.clone(),
                phase,
                promotion_failed,
                init_old_top,
            );
            verifier.verify();

            if vm.flags.gc_dev_verbose {
                println!("GC: Verify {} finished", name);
            }
        }
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
}

impl Collector for Swiper {
    fn supports_tlab(&self) -> bool {
        true
    }

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

    fn alloc(&self, vm: &VM, size: usize, array_ref: bool) -> Address {
        if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(vm, size, array_ref)
        } else {
            self.alloc_large(vm, size, array_ref)
        }
    }

    fn collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Full, reason);
    }

    fn minor_collect(&self, vm: &VM, reason: GcReason) {
        self.perform_collection(vm, CollectionKind::Minor, reason);
    }

    fn needs_write_barrier(&self) -> bool {
        self.emit_write_barrier
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }

    fn dump_summary(&self, runtime: f32) {
        let config = self.config.lock();
        let total_gc = config.total_minor_pause + config.total_full_pause;
        let gc_percentage = ((total_gc / runtime) * 100.0).round();
        let mutator = runtime - total_gc;
        let mutator_percentage = 100.0 - gc_percentage;

        println!("GC stats: total={:.1}", runtime);
        println!("GC stats: mutator={:.1}", mutator);
        println!("GC stats: collection={:.1}", total_gc);
        println!("GC stats: collection-minor={:.1}", config.total_minor_pause);
        println!("GC stats: collection-full={:.1}", config.total_full_pause);

        println!("");
        println!(
            "GC stats: full-collections={:.1}",
            config.total_full_collections
        );
        println!("GC stats: full-total={}", config.full_total_all());
        println!("GC stats: full-marking={}", config.full_marking_all());
        println!(
            "GC stats: full-compute-forward={}",
            config.full_compute_forward_all()
        );
        println!(
            "GC stats: full-update-refs={}",
            config.full_update_refs_all()
        );
        println!("GC stats: full-relocate={}", config.full_relocate_all());
        println!(
            "GC stats: full-reset-cards={}",
            config.full_reset_cards_all()
        );
        println!("");
        println!(
            "GC stats: minor-collections={:.1}",
            config.total_minor_collections
        );
        println!("GC stats: minor-total={}", config.minor_total_all());
        println!("GC stats: minor-roots={}", config.minor_roots_all());
        println!("GC stats: minor-tracing={}", config.minor_tracing_all());
        println!("");

        println!(
            "GC summary: {:.1}ms minor ({}), {:.1}ms full ({}), {:.1}ms collection, {:.1}ms mutator, {:.1}ms total ({}% mutator, {}% GC)",
            config.total_minor_pause,
            config.total_minor_collections,
            config.total_full_pause,
            config.total_full_collections,
            total_gc,
            mutator,
            runtime,
            mutator_percentage,
            gc_percentage,
        );

        println!("\nMinor:");
        println!("\tRoots:\t\t{}", config.minor_roots());
        println!("\tTracing:\t{}", config.minor_tracing());
        println!("\tTotal:\t\t{}", config.minor_total());

        println!("\nFull:");

        println!("\tMarking:\t{}", config.full_marking());
        println!("\tCompute Fwd:\t{}", config.full_compute_forward());
        println!("\tUpdate Refs:\t{}", config.full_update_refs());
        println!("\tRelocate:\t{}", config.full_relocate());
        println!("\tReset Cards:\t{}", config.full_reset_cards());
        println!("\tTotal:\t\t{}", config.full_total());

        println!("");
    }

    fn verify_ref(&self, vm: &VM, reference: Address) {
        let found = self.young.to_active().contains(reference)
            || vm.gc.readonly_space.contains(reference)
            || self.large.contains(reference)
            || (self.old.total().contains(reference) && self.old.contains_slow(reference));

        assert!(found, "write barrier found invalid reference");
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

impl CollectionKind {
    fn is_minor(&self) -> bool {
        match self {
            CollectionKind::Minor => true,
            CollectionKind::Full => false,
        }
    }

    fn is_full(&self) -> bool {
        match self {
            CollectionKind::Minor => false,
            CollectionKind::Full => true,
        }
    }
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

pub fn walk_region<F>(region: Region, mut fct: F)
where
    F: FnMut(&mut Obj, Address, usize),
{
    let mut scan = region.start;

    while scan < region.end {
        let object = scan.to_mut_obj();

        if object.header().vtblptr().is_null() {
            scan = scan.add_ptr(1);
            continue;
        }

        let object_size = object.size();
        fct(object, scan, object_size);
        scan = scan.offset(object_size);
    }

    assert_eq!(scan, region.end);
}

pub trait CommonOldGen {
    fn active_size(&self) -> usize;
    fn committed_size(&self) -> usize;
}

fn forward_full(object: Address, heap: Region, perm: Region, large: Region) -> Option<Address> {
    if heap.contains(object) {
        let obj = object.to_mut_obj();

        if obj.header().is_marked_non_atomic() {
            if large.contains(object) {
                Some(object)
            } else {
                let new_address = obj.header().fwdptr_non_atomic();
                Some(new_address)
            }
        } else {
            None
        }
    } else {
        debug_assert!(perm.contains(object));
        Some(object)
    }
}

fn forward_minor(object: Address, young: Region) -> Option<Address> {
    if young.contains(object) {
        let obj = object.to_mut_obj();

        if let Some(new_address) = obj.header().vtblptr_forwarded() {
            Some(new_address)
        } else {
            None
        }
    } else {
        Some(object)
    }
}
