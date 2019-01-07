use std::cmp::{max, min};

use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::{align_gen, formatted_size};
use gc::{GEN_ALIGNMENT_BITS, GEN_SIZE};

pub fn resize_gens_after_minor(
    _min_heap_size: usize,
    max_heap_size: usize,
    young: &YoungGen,
    old: &OldGen,
    verbose: bool,
) -> bool {
    let old_size = align_gen(old.active_size());
    assert!(young.eden_active().size() == 0);

    let (young_size, old_size) = compute_young_size(max_heap_size, old_size);
    assert!(young_size <= young.committed_size());

    if verbose {
        println!(
            "GC: Resize after Minor GC (young committed {}->{}, old committed {}->{})",
            formatted_size(young.committed_size()),
            formatted_size(young_size),
            formatted_size(old.committed_size()),
            formatted_size(old_size),
        );
    }

    young.set_committed_size(young_size);
    old.set_committed_size(old_size);

    // force full GC when young collection becomes too small
    young_size < MIN_YOUNG_SIZE
}

pub fn resize_gens_after_full(
    _min_heap_size: usize,
    max_heap_size: usize,
    young: &YoungGen,
    old: &OldGen,
    verbose: bool,
) {
    let old_size = align_gen(old.active_size());
    assert!(young.active_size() == 0);

    let (young_size, old_size) = compute_young_size(max_heap_size, old_size);

    if verbose {
        println!(
            "GC: Resize after Full GC (young committed {}->{}, old committed {}->{})",
            formatted_size(young.committed_size()),
            formatted_size(young_size),
            formatted_size(old.committed_size()),
            formatted_size(old_size),
        );
    }

    young.set_committed_size(young_size);
    old.set_committed_size(old_size);
}

const MAX_YOUNG_SIZE: usize = 256 * 1024 * 1024;
const MIN_YOUNG_SIZE: usize = 32 * 1024 * 1024;

// calculate young generation size from old generation and heap size.
pub fn compute_young_size(heap_size: usize, init_old_size: usize) -> (usize, usize) {
    assert!(init_old_size <= heap_size);
    let init_old_size = align_gen(init_old_size);
    let rest = heap_size - init_old_size;

    if rest < GEN_SIZE {
        panic!("Error: Heap too big! No space left for minimal young gen.");
    }

    // use young gen size of half of rest
    let current_max_young_size = (rest >> (GEN_ALIGNMENT_BITS + 1)) << GEN_ALIGNMENT_BITS;

    // but not bigger than general maximum young size
    let young_size = min(current_max_young_size, MAX_YOUNG_SIZE);

    // at least size of 512K
    let young_size = max(young_size, GEN_SIZE);

    let old_size = heap_size - young_size;

    assert!(young_size > 0 && old_size > 0 && old_size + young_size <= heap_size);

    (young_size, old_size)
}
