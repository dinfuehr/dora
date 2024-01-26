use parking_lot::Mutex;

use crate::gc::fill_region;
use crate::gc::freelist::FreeList;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::{get_swiper, RegularPage};
use crate::gc::{Address, GenerationAllocator, Region};
use crate::vm::VM;

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

    pub fn free_page(&self, vm: &VM, page: RegularPage) {
        self.protected.lock().free_page(vm, page);
    }

    pub fn add_to_free_list(&self, vm: &VM, free_regions: Vec<Region>) {
        let mut protected = self.protected.lock();

        for free_region in free_regions {
            fill_region(vm, free_region.start, free_region.end);
            protected.add_to_freelist(vm, free_region.start, free_region.size());
        }
    }

    pub fn promote_pages(&self, vm: &VM, young: &YoungGen) {
        let mut protected = self.protected.lock();
        for page in young.take_over_to_pages() {
            protected.promote_page(vm, page);
        }
    }
}

impl GenerationAllocator for OldGen {
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        let mut protected = self.protected.lock();
        protected.allocate(vm, false, min_size, max_size)
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

    fn add_to_freelist(&mut self, vm: &VM, start: Address, size: usize) {
        self.freelist.add(vm, start, size);
    }

    fn promote_page(&mut self, vm: &VM, page: RegularPage) {
        page.promote();
        self.pages.push(page);
        get_swiper(vm).heap.promote_page(page);
    }

    fn allocate(
        &mut self,
        vm: &VM,
        in_gc: bool,
        min_size: usize,
        max_size: usize,
    ) -> Option<Region> {
        if let Some(region) = self.raw_alloc(min_size, max_size) {
            fill_region(vm, self.top, self.limit);
            return Some(region);
        }

        fill_region(vm, self.top, self.limit);

        let free_space = self.freelist.alloc(vm, min_size);

        if free_space.is_non_null() {
            self.top = free_space.addr();
            self.limit = self.top.offset(free_space.size(vm.meta_space_start()));

            let region = self
                .raw_alloc(min_size, max_size)
                .expect("allocation failed");

            fill_region(vm, self.top, self.limit);
            return Some(region);
        }

        if let Some(page) = self.allocate_page(vm, in_gc) {
            self.pages.push(page);
            self.pages.sort();

            self.top = page.object_area_start();
            self.limit = page.object_area_end();
            let result = self.raw_alloc(min_size, max_size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(vm, self.top, self.limit);

            result
        } else {
            None
        }
    }

    fn free_page(&mut self, vm: &VM, page: RegularPage) {
        let idx = self
            .pages
            .iter()
            .position(|&p| p == page)
            .expect("missing page");
        self.pages.swap_remove(idx);

        get_swiper(vm).heap.free_regular_old_page(page);
    }

    fn allocate_page(&mut self, vm: &VM, in_gc: bool) -> Option<RegularPage> {
        get_swiper(vm).heap.alloc_regular_old_page(vm, in_gc)
    }

    fn raw_alloc(&mut self, min_size: usize, max_size: usize) -> Option<Region> {
        if self.top.offset(min_size) <= self.limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(max_size).min(self.limit);
            let alloc = Region::new(alloc_start, alloc_end);
            debug_assert!(alloc.size() >= min_size && alloc.size() <= max_size);
            self.top = alloc_end;
            Some(alloc)
        } else {
            None
        }
    }
}
