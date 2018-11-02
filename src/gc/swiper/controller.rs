use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::GEN_ALIGNMENT_BITS;
use gc::{align_gen, formatted_size};

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

    young_size < old_size / 16
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

// calculate young generation size from old generation and heap size.
fn compute_young_size(heap_size: usize, old_size: usize) -> (usize, usize) {
    assert!(old_size <= heap_size);
    let old_size = align_gen(old_size);
    let rest = ((heap_size - old_size) >> (GEN_ALIGNMENT_BITS + 1)) << GEN_ALIGNMENT_BITS;

    let young_size = rest;
    let old_size = heap_size - young_size;

    assert!(young_size > 0 && old_size > 0 && old_size + young_size <= heap_size);
    (young_size, old_size)
}
