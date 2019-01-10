use parking_lot::Mutex;
use std::sync::Arc;

use gc::formatted_size;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::CollectionKind;

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

pub fn resize_gens(
    max_heap_size: usize,
    config: &SharedHeapConfig,
    young: &YoungGen,
    old: &OldGen,
    large: &LargeSpace,
    verbose: bool,
) {
    let mut config = config.lock();

    assert!(young.eden_active().empty());
    let (eden_size, semi_size) = young.committed_size();
    let young_size = eden_size + semi_size;

    let old_size = old.committed_size() + large.committed_size();
    config.old_size = old_size;

    let max_old_limit = max_heap_size - young_size;
    let old_old_limit = config.old_limit;
    config.old_limit = max_old_limit;

    if verbose {
        println!(
            "GC: Resize Old {} -> {}",
            formatted_size(old_old_limit),
            formatted_size(config.old_limit)
        );
    }

    young.set_committed_size(eden_size, semi_size);
}

pub struct HeapConfig {
    eden_size: usize,
    semi_size: usize,
    old_size: usize,
    old_limit: usize,
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
