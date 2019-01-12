use parking_lot::Mutex;
use std::sync::Arc;

use gc::formatted_size;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::CollectionKind;
use timer;

pub fn choose_collection_kind(config: &SharedHeapConfig, young: &YoungGen) -> CollectionKind {
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    let rest = {
        let config = config.lock();
        config.old_limit - config.old_size
    };

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
    max_heap_size: usize,
    config: &SharedHeapConfig,
    kind: CollectionKind,
    young: &YoungGen,
    old: &OldGen,
    large: &LargeSpace,
    verbose: bool,
) {
    let mut config = config.lock();

    let gc_end = timer::timestamp();
    let gc_duration = gc_end - config.gc_start;

    config.end_object_size = object_size(young, old, large);
    config.end_memory_size = memory_size(young, old, large);

    assert!(young.eden_active().empty());
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    let old_size = old.committed_size() + large.committed_size();
    config.old_size = old_size;

    let max_old_limit = max_heap_size - young_size;
    config.old_limit = max_old_limit;

    match kind {
        CollectionKind::Minor => {
            config.total_minor_collections += 1;
            config.total_minor_pause += timer::in_ms(gc_duration);
        }

        CollectionKind::Full => {
            config.total_full_collections += 1;
            config.total_full_pause += timer::in_ms(gc_duration);
        }
    }

    if verbose {
        println!(
            "GC: {} {}/{} -> {}/{}; {:.2} ms",
            kind,
            formatted_size(config.start_object_size),
            formatted_size(config.start_memory_size),
            formatted_size(config.end_object_size),
            formatted_size(config.end_memory_size),
            timer::in_ms(gc_duration),
        );
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
    eden_size: usize,
    semi_size: usize,
    old_size: usize,
    old_limit: usize,

    gc_start: u64,

    start_object_size: usize,
    start_memory_size: usize,
    end_object_size: usize,
    end_memory_size: usize,

    pub total_minor_collections: usize,
    pub total_minor_pause: f32,
    pub total_full_collections: usize,
    pub total_full_pause: f32,
}

impl HeapConfig {
    pub fn new(
        eden_size: usize,
        semi_size: usize,
        old_size: usize,
        old_limit: usize,
    ) -> HeapConfig {
        HeapConfig {
            eden_size: eden_size,
            semi_size: semi_size,
            old_size: old_size,
            old_limit: old_limit,

            gc_start: 0,
            start_object_size: 0,
            start_memory_size: 0,
            end_object_size: 0,
            end_memory_size: 0,

            total_minor_collections: 0,
            total_minor_pause: 0f32,
            total_full_collections: 0,
            total_full_pause: 0f32,
        }
    }

    pub fn eden_size(&self) -> usize {
        self.eden_size
    }

    pub fn set_eden_size(&mut self, size: usize) {
        self.eden_size = size;
    }

    pub fn semi_size(&self) -> usize {
        self.semi_size
    }

    pub fn set_semi_size(&mut self, size: usize) {
        self.semi_size = size;
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
