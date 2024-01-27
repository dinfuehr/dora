use parking_lot::Mutex;

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::swiper::{get_swiper, RegularPage, PAGE_SIZE, SURVIVOR_BIT};
use crate::gc::{fill_region, is_page_aligned, Address, GenerationAllocator, Region};
use crate::os::{self, MemoryPermission};
use crate::vm::VM;

pub struct YoungGen {
    from_index: AtomicUsize,
    protect: bool,
    protected: Mutex<YoungGenProtected>,
}

impl YoungGen {
    pub fn new(protect: bool) -> YoungGen {
        YoungGen {
            from_index: AtomicUsize::new(0),
            protect,
            protected: Mutex::new(YoungGenProtected {
                top: Address::null(),
                limit: Address::null(),
                next_page_idx: 0,

                pages: [Vec::new(), Vec::new()],
            }),
        }
    }

    pub(super) fn setup(&self, vm: &VM, young_size: usize) {
        assert_eq!(young_size % (2 * PAGE_SIZE), 0);
        let semi_size = young_size / 2;
        assert!(semi_size > 0);
        assert_eq!(semi_size % PAGE_SIZE, 0);

        self.commit(vm, semi_size);
        self.protect_from();

        let protected = self.protected.lock();
        self.make_pages_iterable(vm, &protected.pages[self.to_index()]);
    }

    fn make_pages_iterable(&self, vm: &VM, pages: &[RegularPage]) {
        for &page in pages {
            let page = RegularPage::setup(page.address(), true, false);
            fill_region(vm, page.object_area_start(), page.object_area_end());
        }
    }

    fn commit(&self, vm: &VM, semi_size: usize) {
        self.commit_semi_space(vm, 0, semi_size);
        self.commit_semi_space(vm, 1, semi_size);
    }

    pub fn unprotect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            let from_pages: &Vec<RegularPage> = &self.protected.lock().pages[self.from_index()];
            for &page in from_pages {
                os::protect(page.address(), page.size(), MemoryPermission::ReadWrite);
            }
        }
    }

    pub fn allocated_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.next_page_idx * PAGE_SIZE
    }

    pub fn protect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            let from_pages: &Vec<RegularPage> = &self.protected.lock().pages[self.from_index()];
            for page in from_pages {
                os::protect(page.address(), page.size(), MemoryPermission::None);
            }
        }
    }

    pub fn swap_semi(&self, vm: &VM) {
        self.swap_indices();

        let mut protected = self.protected.lock();

        self.make_pages_iterable(vm, &protected.pages[self.to_index()]);

        protected.top = Address::null();
        protected.limit = Address::null();
        protected.next_page_idx = 0;
    }

    pub fn reset_after_full_gc(&self, vm: &VM) {
        self.unprotect_from();
        self.swap_semi(vm);
        self.protect_from();

        for page in self.to_pages() {
            assert!(!page.is_survivor());
        }
    }

    pub fn reset_after_minor_gc(&self) {
        let next_page_idx = self.protected.lock().next_page_idx;

        for page in self.to_pages().iter().take(next_page_idx) {
            assert!(!page.base_page_header().is_survivor());
            page.base_page_header().add_flag(SURVIVOR_BIT);
        }
    }

    pub fn resize_after_gc(&self, vm: &VM, young_size: usize) {
        assert_eq!(young_size % (2 * PAGE_SIZE), 0);
        let new_semi_size = young_size / 2;
        assert_eq!(new_semi_size % PAGE_SIZE, 0);

        self.commit_semi_space(vm, 0, new_semi_size);
        self.commit_semi_space(vm, 1, new_semi_size);

        let protected = self.protected.lock();
        assert!(protected.top <= protected.limit);
        assert!(protected.next_page_idx <= new_semi_size / PAGE_SIZE);
    }

    fn commit_semi_space(&self, vm: &VM, semi_space_idx: usize, new_size: usize) {
        assert!(is_page_aligned(new_size));

        let mut protected = self.protected.lock();
        let space_pages = &mut protected.pages[semi_space_idx];

        let old_size = space_pages.len() * PAGE_SIZE;

        if old_size < new_size {
            let new_pages = (new_size - old_size) / PAGE_SIZE;
            let heap = &get_swiper(vm).heap;

            for _ in 0..new_pages {
                let page = heap
                    .alloc_regular_young_page(vm)
                    .expect("page allocation failed");
                space_pages.push(page);
            }
        } else if old_size > new_size {
            let target_pages = new_size / PAGE_SIZE;
            let heap = &get_swiper(vm).heap;

            while space_pages.len() > target_pages {
                let page = space_pages.pop().expect("missing page");
                heap.free_regular_young_page(page);
            }
        }

        assert_eq!(space_pages.len(), new_size / PAGE_SIZE);
    }

    fn from_index(&self) -> usize {
        self.from_index.load(Ordering::Relaxed)
    }

    fn to_index(&self) -> usize {
        self.from_index() ^ 1
    }

    fn swap_indices(&self) {
        let from_index = self.from_index();
        self.from_index.store(from_index ^ 1, Ordering::Relaxed);
    }

    pub fn to_pages(&self) -> Vec<RegularPage> {
        self.protected.lock().pages[self.to_index()].clone()
    }

    pub fn take_over_to_pages(&self) -> Vec<RegularPage> {
        let mut protected = self.protected.lock();
        std::mem::replace(&mut protected.pages[self.to_index()], Vec::new())
    }
}

impl GenerationAllocator for YoungGen {
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        let mut protected = self.protected.lock();
        protected.alloc(vm, self, min_size, max_size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct YoungGenProtected {
    top: Address,
    limit: Address,
    next_page_idx: usize,

    pages: [Vec<RegularPage>; 2],
}

impl YoungGenProtected {
    fn alloc(
        &mut self,
        vm: &VM,
        young: &YoungGen,
        min_size: usize,
        max_size: usize,
    ) -> Option<Region> {
        if let Some(region) = self.raw_alloc(vm, min_size, max_size) {
            return Some(region);
        }

        let pages = &self.pages[young.to_index()];

        assert!(self.next_page_idx <= pages.len());

        if self.next_page_idx < pages.len() {
            fill_region(vm, self.top, self.limit);
            let page = RegularPage::setup(pages[self.next_page_idx].address(), true, false);
            self.top = page.object_area_start();
            self.limit = page.object_area_end();
            self.next_page_idx += 1;
            fill_region(vm, self.top, self.limit);
            let result = self.raw_alloc(vm, min_size, max_size);
            assert!(result.is_some());
            result
        } else {
            None
        }
    }

    fn raw_alloc(&mut self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        if self.top.offset(min_size) <= self.limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(max_size).min(self.limit);
            let alloc = Region::new(alloc_start, alloc_end);
            debug_assert!(alloc.size() >= min_size);
            self.top = alloc_end;
            fill_region(vm, self.top, self.limit);
            Some(alloc)
        } else {
            None
        }
    }
}
