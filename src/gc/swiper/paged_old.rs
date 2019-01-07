use fixedbitset::FixedBitSet;

use gc::Region;

// experimental implementation of an Old generation consisting of pages
// goal is to reduce work for full GCs
struct PagedOldGen {
    // total size of old generation
    total: Region,

    pages: Vec<Page>,

    // all used pages
    used_pages: PageSet,

    // completely free pages (no live objects)
    free_pages: PageSet,
}

// Choose 512K as page size for now
const PAGE_SIZE_BITS: usize = 19;
const PAGE_SIZE: usize = 2 << PAGE_SIZE_BITS;

struct PageId(usize);

struct Page {
    // page boundaries
    region: Region,

    // end of allocated area in page
    top: usize,

    // live objects in bytes
    live: usize,

    // state of page
    state: PageState,
}

// An array of Pages
struct PageSet {
    leftmost: usize,
    rightmost: usize,
    pages: FixedBitSet,
}

impl PageSet {
    fn empty(pages: usize) -> PageSet {
        assert!(pages > 0);

        PageSet {
            leftmost: 0,
            rightmost: pages - 1,
            pages: FixedBitSet::with_capacity(pages),
        }
    }
}

enum PageState {
    Free,
    Used,
}
