use parking_lot::{Mutex, MutexGuard};

use crate::gc::freelist::FreeList;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::{RegularPage, PAGE_SIZE};
use crate::gc::{fill_region, fill_region_with};
use crate::gc::{Address, GenerationAllocator, Region};
use crate::vm::VM;

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

    pub fn protected(&self) -> MutexGuard<OldGenProtected> {
        self.protected.lock()
    }

    pub fn committed_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.size
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

pub struct OldGenProtected {
    size: usize,
    top: Address,
    current_limit: Address,
    pages: Vec<RegularPage>,
    freelist: FreeList,
}

impl OldGenProtected {
    fn new() -> OldGenProtected {
        OldGenProtected {
            size: 0,
            top: Address::null(),
            current_limit: Address::null(),
            pages: Vec::new(),
            freelist: FreeList::new(),
        }
    }

    pub fn pages(&self) -> Vec<RegularPage> {
        self.pages.clone()
    }

    pub fn clear_freelist(&mut self) {
        self.top = self.current_limit;
        self.freelist.clear();
    }

    pub fn add_to_freelist(&mut self, vm: &VM, start: Address, size: usize) {
        self.freelist.add(vm, start, size);
    }

    pub fn allocate(
        &mut self,
        vm: &VM,
        in_gc: bool,
        min_size: usize,
        max_size: usize,
    ) -> Option<Region> {
        if let Some(region) = self.raw_alloc(min_size, max_size) {
            fill_region_with(vm, self.top, self.current_limit, false);
            return Some(region);
        }

        fill_region_with(vm, self.top, self.current_limit, false);

        let free_space = self.freelist.alloc(vm, min_size);

        if free_space.is_non_null() {
            self.top = free_space.addr();
            self.current_limit = self.top.offset(free_space.size(vm.meta_space_start()));

            let region = self
                .raw_alloc(min_size, max_size)
                .expect("allocation failed");

            fill_region_with(vm, self.top, self.current_limit, false);
            return Some(region);
        }

        if let Some(page) = self.allocate_page(vm, in_gc) {
            self.pages.push(page);
            self.pages.sort();
            self.size += PAGE_SIZE;

            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            let result = self.raw_alloc(min_size, max_size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(vm, self.top, self.current_limit);

            result
        } else {
            None
        }
    }

    pub fn free_page(&mut self, vm: &VM, page: RegularPage) {
        let idx = self
            .pages
            .iter()
            .position(|&p| p == page)
            .expect("missing page");
        self.pages.swap_remove(idx);
        self.size -= PAGE_SIZE;

        vm.gc
            .collector
            .to_swiper()
            .mixed_heap
            .free_regular_page(page);
    }

    fn allocate_page(&mut self, vm: &VM, in_gc: bool) -> Option<RegularPage> {
        vm.gc
            .collector
            .to_swiper()
            .mixed_heap
            .alloc_regular_page(vm, in_gc)
    }

    fn raw_alloc(&mut self, min_size: usize, max_size: usize) -> Option<Region> {
        if self.top.offset(min_size) <= self.current_limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(max_size).min(self.current_limit);
            let alloc = Region::new(alloc_start, alloc_end);
            debug_assert!(alloc.size() >= min_size && alloc.size() <= max_size);
            self.top = alloc_end;
            Some(alloc)
        } else {
            None
        }
    }
}
