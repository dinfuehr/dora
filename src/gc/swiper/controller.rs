use parking_lot::Mutex;
use std::cmp::{max, min};
use std::sync::Arc;

use driver::cmd::Args;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::CollectionKind;
use gc::{align_gen, formatted_size, GEN_SIZE, M};
use os::signal::Trap;
use stdlib;
use timer;

const MAX_YOUNG_SIZE: usize = 20 * M;

const INIT_HEAP_SIZE_RATIO: usize = 2;
const INIT_YOUNG_RATIO: usize = 4;
const INIT_SEMI_RATIO: usize = 3;

pub fn init(config: &mut HeapConfig, args: &Args) {
    assert!(config.min_heap_size <= config.max_heap_size);

    let young_size = if let Some(young_ratio) = args.flag_gc_young_ratio {
        assert!(young_ratio >= 1);
        align_gen(config.max_heap_size / young_ratio)
    } else {
        let young_size = align_gen(config.max_heap_size / INIT_YOUNG_RATIO);
        min(MAX_YOUNG_SIZE, young_size)
    };
    let young_size = max(young_size, GEN_SIZE);

    let semi_ratio = args.flag_gc_semi_ratio.unwrap_or(INIT_SEMI_RATIO);
    let semi_size = if semi_ratio == 0 {
        0
    } else {
        align_gen(young_size / semi_ratio)
    };
    let semi_size = max(semi_size, GEN_SIZE);
    let eden_size = young_size - semi_size;

    config.eden_size = eden_size;
    config.semi_size = semi_size;

    let max_old_limit = config.max_heap_size - young_size;
    let min_old_limit = if config.min_heap_size > young_size {
        config.min_heap_size - young_size
    } else {
        0
    };

    let old_limit = align_gen(config.max_heap_size / INIT_HEAP_SIZE_RATIO);
    let old_limit = min(old_limit, max_old_limit);
    let old_limit = max(old_limit, min_old_limit);

    config.old_size = 0;
    config.old_limit = old_limit;
}

pub fn choose_collection_kind(config: &SharedHeapConfig, args: &Args, young: &YoungGen) -> CollectionKind {
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    let rest = {
        let config = config.lock();
        config.old_limit - config.old_size
    };

    if args.flag_gc_young_ratio.is_some() {
        // With a large young generation with e.g. 1/2 or 1/3 of the
        // maximum heap size, we would do full collections most of the time.
        // Therefore always perform a minor collection. If objects cannot be promoted,
        // a full collection is performed automatically.
        return CollectionKind::Minor;
    }

    if rest < young_size {
        CollectionKind::Full
    } else {
        CollectionKind::Minor
    }
}

pub fn start(config: &SharedHeapConfig, young: &YoungGen, old: &OldGen, large: &LargeSpace) {
    let mut config = config.lock();

    config.gc_start = timer::timestamp();
    config.start_object_size = object_size(young, old, large);
    config.start_memory_size = memory_size(young, old, large);
}

pub fn stop(
    config: &SharedHeapConfig,
    kind: CollectionKind,
    young: &YoungGen,
    old: &OldGen,
    large: &LargeSpace,
    verbose: bool,
) {
    let mut config = config.lock();

    let gc_end = timer::timestamp();
    config.gc_duration = timer::in_ms(gc_end - config.gc_start);

    config.end_object_size = object_size(young, old, large);
    config.end_memory_size = memory_size(young, old, large);

    assert!(young.eden_active().empty());
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    let old_size = old.committed_size() + large.committed_size();
    config.old_size = old_size;

    let old_limit = config.max_heap_size - young_size;
    let old_limit = max(old_limit, old_size);
    config.old_limit = old_limit;

    if young_size + old_limit > config.max_heap_size {
        stdlib::trap(Trap::OOM.int());
    }

    assert!(young_size + old_limit <= config.max_heap_size);

    match kind {
        CollectionKind::Minor => {
            config.total_minor_collections += 1;
            config.total_minor_pause += config.gc_duration;
        }

        CollectionKind::Full => {
            config.total_full_collections += 1;
            config.total_full_pause += config.gc_duration;
        }
    }

    if verbose {
        print(&*config, kind);
    }
}

fn print(config: &HeapConfig, kind: CollectionKind) {
    match kind {
        CollectionKind::Minor => {
            println!(
                "GC: {} {}/{} -> {}/{}; {:.2} ms; {} promoted; {} copied",
                kind,
                formatted_size(config.start_object_size),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_object_size),
                formatted_size(config.end_memory_size),
                config.gc_duration,
                formatted_size(config.minor_promoted),
                formatted_size(config.minor_copied),
            );
        }

        CollectionKind::Full => {
            println!(
                "GC: {} {}/{} -> {}/{}; {:.2} ms",
                kind,
                formatted_size(config.start_object_size),
                formatted_size(config.start_memory_size),
                formatted_size(config.end_object_size),
                formatted_size(config.end_memory_size),
                config.gc_duration,
            );
        }
    }
}

fn object_size(young: &YoungGen, old: &OldGen, large: &LargeSpace) -> usize {
    young.active_size() + old.active_size() + large.committed_size()
}

fn memory_size(young: &YoungGen, old: &OldGen, large: &LargeSpace) -> usize {
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    young_size + old.committed_size() + large.committed_size()
}

pub struct HeapConfig {
    min_heap_size: usize,
    max_heap_size: usize,

    pub eden_size: usize,
    pub semi_size: usize,
    pub old_size: usize,
    pub old_limit: usize,

    gc_start: u64,
    gc_duration: f32,

    start_object_size: usize,
    start_memory_size: usize,
    end_object_size: usize,
    end_memory_size: usize,

    pub minor_promoted: usize,
    pub minor_copied: usize,

    pub total_minor_collections: usize,
    pub total_minor_pause: f32,
    pub total_full_collections: usize,
    pub total_full_pause: f32,
}

impl HeapConfig {
    pub fn new(min_heap_size: usize, max_heap_size: usize) -> HeapConfig {
        assert!(min_heap_size <= max_heap_size);

        HeapConfig {
            min_heap_size: min_heap_size,
            max_heap_size: max_heap_size,

            eden_size: 0,
            semi_size: 0,
            old_size: 0,
            old_limit: 0,

            gc_start: 0,
            gc_duration: 0f32,

            start_object_size: 0,
            start_memory_size: 0,
            end_object_size: 0,
            end_memory_size: 0,

            minor_promoted: 0,
            minor_copied: 0,

            total_minor_collections: 0,
            total_minor_pause: 0f32,
            total_full_collections: 0,
            total_full_pause: 0f32,
        }
    }

    pub fn grow_old(&mut self, size: usize) -> bool {
        if self.old_size + size <= self.old_limit {
            self.old_size += size;
            true
        } else {
            false
        }
    }
}

pub type SharedHeapConfig = Arc<Mutex<HeapConfig>>;
