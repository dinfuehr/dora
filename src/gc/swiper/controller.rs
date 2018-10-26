use gc::align_space;

// calculate young generation size from old generation and heap size.
pub fn compute_young_size(heap_size: usize, old_size: usize) -> (usize, usize) {
    assert!(old_size <= heap_size);
    let old_size = align_space(old_size);
    let rest = (heap_size - old_size) / 2;

    assert!(heap_size == old_size + 2 * rest);
    (rest, old_size + rest)
}
