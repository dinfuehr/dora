use parking_lot::Mutex;

use crate::gc::fill_region;
use crate::gc::freelist::FreeList;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::{RegularPage, get_swiper};
use crate::gc::{Address, GenerationAllocator, Region};
use crate::runtime::Runtime;

use super::young::YoungGen;

pub struct OldGen {
    protected: Mutex<OldGenProtected>,

    config: SharedHeapConfig,
}

impl OldGen {
    pub fn new(config: SharedHeapConfig) -> OldGen {
        let old = OldGen {
            protected: Mutex::new(OldGenProtected::new()),

            config,
        };

        old
    }

    pub fn pages(&self) -> Vec<RegularPage> {
        self.protected.lock().pages()
    }

    pub fn clear_freelist(&self) {
        self.protected.lock().clear_freelist();
    }

    pub fn free_page(&self, rt: &Runtime, page: RegularPage) {
        self.protected.lock().free_page(rt, page);
    }

    pub fn add_to_free_list(&self, rt: &Runtime, free_regions: Vec<Region>) {
        let mut protected = self.protected.lock();

        for free_region in free_regions {
            fill_region(rt, free_region.start, free_region.end);
            protected.add_to_freelist(rt, free_region.start, free_region.size());
        }
    }

    pub fn promote_pages(&self, rt: &Runtime, young: &YoungGen) {
        let mut protected = self.protected.lock();
        for page in young.take_over_to_pages() {
            protected.promote_page(rt, page);
        }
    }

    fn bump_alloc(&self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        self.protected.lock().bump_alloc(rt, min_size, max_size)
    }

    fn try_free_list(&self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        self.protected.lock().try_free_list(rt, min_size, max_size)
    }

    fn try_add_page(&self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        self.protected.lock().try_add_page(rt, min_size, max_size)
    }
}

impl GenerationAllocator for OldGen {
    fn allocate(&self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        if let Some(region) = self.bump_alloc(rt, min_size, max_size) {
            return Some(region);
        }

        if let Some(region) = self.try_free_list(rt, min_size, max_size) {
            return Some(region);
        }

        let swiper = get_swiper(rt);

        if swiper.sweeper.in_progress() {
            swiper.sweeper.sweep_in_allocation(rt);

            if let Some(region) = self.try_free_list(rt, min_size, max_size) {
                return Some(region);
            }
        }

        if let Some(region) = self.try_add_page(rt, min_size, max_size) {
            return Some(region);
        }

        swiper.sweeper.sweep_in_allocation_to_end(rt);

        if let Some(region) = self.try_free_list(rt, min_size, max_size) {
            return Some(region);
        }

        if let Some(region) = self.try_add_page(rt, min_size, max_size) {
            return Some(region);
        }

        None
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct OldGenProtected {
    top: Address,
    limit: Address,
    pages: Vec<RegularPage>,
    freelist: FreeList,
}

impl OldGenProtected {
    fn new() -> OldGenProtected {
        OldGenProtected {
            top: Address::null(),
            limit: Address::null(),
            pages: Vec::new(),
            freelist: FreeList::new(),
        }
    }

    fn pages(&self) -> Vec<RegularPage> {
        self.pages.clone()
    }

    fn clear_freelist(&mut self) {
        self.top = self.limit;
        self.freelist.clear();
    }

    fn add_to_freelist(&mut self, rt: &Runtime, start: Address, size: usize) {
        self.freelist.add(rt, start, size);
    }

    fn promote_page(&mut self, rt: &Runtime, page: RegularPage) {
        page.promote();
        self.pages.push(page);
        get_swiper(rt).heap.promote_page(page);
    }

    fn try_free_list(&mut self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        let free_space = self.freelist.alloc(rt, min_size);

        if free_space.is_non_null() {
            self.top = free_space.addr();
            self.limit = self.top.offset(free_space.size(rt.shape_base()));

            let region = self
                .bump_alloc(rt, min_size, max_size)
                .expect("allocation failed");

            fill_region(rt, self.top, self.limit);
            Some(region)
        } else {
            None
        }
    }

    fn try_add_page(&mut self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        if let Some(page) = self.allocate_page(rt) {
            self.pages.push(page);

            self.top = page.object_area_start();
            self.limit = page.object_area_end();
            let result = self.bump_alloc(rt, min_size, max_size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(rt, self.top, self.limit);

            result
        } else {
            None
        }
    }

    fn free_page(&mut self, rt: &Runtime, page: RegularPage) {
        let idx = self
            .pages
            .iter()
            .position(|&p| p == page)
            .expect("missing page");
        self.pages.swap_remove(idx);

        get_swiper(rt).heap.free_regular_old_page(page);
    }

    fn allocate_page(&mut self, rt: &Runtime) -> Option<RegularPage> {
        get_swiper(rt).heap.alloc_regular_old_page(rt)
    }

    fn bump_alloc(&mut self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region> {
        if self.top.offset(min_size) <= self.limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(max_size).min(self.limit);
            let alloc = Region::new(alloc_start, alloc_end);
            debug_assert!(alloc.size() >= min_size && alloc.size() <= max_size);
            self.top = alloc_end;
            fill_region(rt, self.top, self.limit);
            Some(alloc)
        } else {
            None
        }
    }
}
