use gc::align_gen;
use gc::GEN_ALIGNMENT_BITS;

// calculate young generation size from old generation and heap size.
pub fn compute_young_size(heap_size: usize, old_size: usize) -> (usize, usize) {
    assert!(old_size <= heap_size);
    let old_size = align_gen(old_size);
    let rest = ((heap_size - old_size) >> (GEN_ALIGNMENT_BITS + 1)) << GEN_ALIGNMENT_BITS;

    let young_size = rest;
    let old_size = heap_size - young_size;

    assert!(young_size > 0 && old_size > 0 && old_size + young_size <= heap_size);
    (young_size, old_size)
}
