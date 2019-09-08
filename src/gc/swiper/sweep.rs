use parking_lot::Mutex;
use scoped_threadpool::Pool;
use std::sync::Arc;

use crate::driver::cmd::Args;
use crate::gc::root::{get_rootset, Slot};
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::{self, HeapConfig, SharedHeapConfig};
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::sweep::old::OldGen;
use crate::gc::swiper::verify::VerifierPhase;
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::{CollectionKind, CARD_SIZE_BITS, LARGE_OBJECT_SIZE};
use crate::gc::tlab;
use crate::gc::{align_gen, arena, formatted_size, GEN_SIZE};
use crate::gc::{Address, Collector, GcReason, Region};
use crate::mem;
use crate::safepoint;
use crate::vm::VM;

mod old;
mod sweep;

pub struct SweepSwiper {
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
}

impl SweepSwiper {
    pub fn new(args: &Args) -> SweepSwiper {
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
        let heap_start = arena::reserve_align(reserve_size, GEN_SIZE);
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

        let emit_write_barrier = !args.flag_disable_barrier;

        SweepSwiper {
            heap: Region::new(heap_start, heap_end),
            reserved_area: reserved_area,

            young: young,
            old: old,
            large: large,

            card_table: card_table,
            crossing_map: crossing_map,
            config: config,

            card_table_offset: card_table_offset,
            emit_write_barrier: emit_write_barrier,

            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            threadpool: Mutex::new(Pool::new(nworkers as u32)),
        }
    }
}

impl Collector for SweepSwiper {
    fn supports_tlab(&self) -> bool {
        true
    }

    fn alloc_tlab_area(&self, _vm: &VM, _size: usize) -> Option<Region> {
        unimplemented!()
    }

    fn alloc(&self, vm: &VM, size: usize, array_ref: bool) -> Address {
        if size < LARGE_OBJECT_SIZE {
            self.alloc_normal(vm, size, array_ref)
        } else {
            self.alloc_large(vm, size, array_ref)
        }
    }

    fn collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn minor_collect(&self, _vm: &VM, _reason: GcReason) {
        unimplemented!()
    }

    fn needs_write_barrier(&self) -> bool {
        self.emit_write_barrier
    }

    fn card_table_offset(&self) -> usize {
        self.card_table_offset
    }

    fn dump_summary(&self, _runtime: f32) {
        unimplemented!()
    }

    fn verify_ref(&self, _vm: &VM, _reference: Address) {
        unimplemented!()
    }
}

impl SweepSwiper {
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

    fn minor_collect(&self, _vm: &VM, _reason: GcReason, _rootset: &[Slot]) -> bool {
        unimplemented!()
    }

    fn full_collect(&self, _vm: &VM, _reason: GcReason, _rootset: &[Slot]) {
        unimplemented!();
    }

    fn verify(
        &self,
        _vm: &VM,
        _phase: VerifierPhase,
        _kind: CollectionKind,
        _name: &str,
        _rootset: &[Slot],
        _promotion_failed: bool,
    ) {
        unimplemented!();
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
