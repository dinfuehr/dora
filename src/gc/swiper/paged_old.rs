use gc::Region;

// experimental implementation of an Old generation consisting of pages
// goal is to reduce work for full GCs
struct PagedOldGen {
    // total size of old generation
    total: Region,

    live_pages: PageSet,
    free_pages: PageSet,
}

// Choose 512K as page size for now
const PAGE_SIZE_BITS: usize = 19;

struct PageId(usize);

struct Page {
    // end of allocated area in page
    top: usize,

    // live objects in bytes
    live: usize,

    // amount of garbage in bytes
    garbage: usize,
}

// An array of Pages
// TODO: implement this as bitset
struct PageSet {
    pages: Vec<PageId>,
}