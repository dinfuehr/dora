use parking_lot::Mutex;

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::swiper::{RegularPage, PAGE_SIZE, SURVIVOR_BIT};
use crate::gc::{
    fill_region, fill_region_with, is_page_aligned, Address, GenerationAllocator, Region,
};
use crate::mem;
use crate::os::{self, MemoryPermission};
use crate::vm::VM;

pub struct YoungGen {
    total: Region,

    semispaces: [Region; 2],
    from_index: AtomicUsize,
    current_semi_size: AtomicUsize,

    protect: bool,

    protected: Mutex<YoungGenProtected>,
}

impl YoungGen {
    pub fn new(total: Region, young_size: usize, protect: bool) -> YoungGen {
        assert!(total.start().is_page_aligned());
        assert!(is_page_aligned(total.size()));

        let semi_size = young_size / 2;
        assert!(semi_size > 0);
        assert_eq!(semi_size % PAGE_SIZE, 0);

        let total_semi_size = total.size() / 2;
        assert!(semi_size <= total_semi_size);

        let first = total.start().region_start(total_semi_size);
        let second = first.end().region_start(total_semi_size);
        assert_eq!(second.end(), total.end());

        let semispaces = [first, second];
        let from_region = 0;
        let to_region = 1;
        let to_region = semispaces[to_region];

        assert!(to_region.start().offset(PAGE_SIZE) <= to_region.end());

        let committed_region = to_region.start().region_start(semi_size);

        YoungGen {
            total,
            semispaces,
            from_index: AtomicUsize::new(from_region),
            current_semi_size: AtomicUsize::new(semi_size),
            protect,
            protected: Mutex::new(YoungGenProtected {
                top: Address::null(),
                current_limit: Address::null(),
                next_page: committed_region.start(),
                limit: committed_region.end(),

                pages: [Vec::new(), Vec::new()],
            }),
        }
    }

    pub(super) fn setup(&self, vm: &VM) {
        self.commit(vm, self.current_semi_size());
        self.protect_from();

        let protected = self.protected.lock();
        self.make_pages_iterable(vm, &protected.pages[self.to_index()]);
    }

    fn make_pages_iterable(&self, vm: &VM, pages: &[RegularPage]) {
        for &page in pages {
            let page = RegularPage::setup(page.address(), true, false);
            fill_region_with(vm, page.object_area_start(), page.object_area_end(), true);
        }
    }

    fn commit(&self, vm: &VM, semi_size: usize) {
        self.commit_semi_space(vm, 0, 0, semi_size);
        self.commit_semi_space(vm, 1, 0, semi_size);
    }

    pub fn reset_after_full_gc(&self, vm: &VM) {
        self.unprotect_from();
        self.swap_semi(vm);
        self.protect_from();

        for page in self.to_pages() {
            assert!(!page.is_survivor());
        }
    }

    pub fn unprotect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            for page in self.from_pages() {
                os::protect(page.address(), page.size(), MemoryPermission::ReadWrite);
            }
        }
    }

    fn current_semi_size(&self) -> usize {
        self.current_semi_size.load(Ordering::Relaxed)
    }

    fn set_current_semi_size(&self, size: usize) {
        assert!(mem::is_os_page_aligned(size));
        self.current_semi_size.store(size, Ordering::Relaxed);
    }

    pub fn allocated_size(&self) -> usize {
        let protected = self.protected.lock();
        let to_committed = self.to_committed();
        protected.next_page.offset_from(to_committed.start())
    }

    pub fn protect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            for page in self.from_pages() {
                os::protect(page.address(), page.size(), MemoryPermission::None);
            }
        }
    }

    pub fn swap_semi(&self, vm: &VM) {
        self.swap_indices();

        let mut protected = self.protected.lock();

        self.make_pages_iterable(vm, &protected.pages[self.to_index()]);

        let to_committed = self.to_committed();
        protected.top = to_committed.start();
        protected.current_limit = to_committed.start();
        protected.next_page = to_committed.start();
        protected.limit = to_committed.end();
    }

    pub fn reset_after_minor_gc(&self) {
        let allocation_end = self.protected.lock().current_limit;

        for page in self.to_pages() {
            assert!(!page.base_page_header().is_survivor());

            if page.end() <= allocation_end {
                page.base_page_header().add_flag(SURVIVOR_BIT);
            }
        }
    }

    pub fn resize_after_gc(&self, vm: &VM, young_size: usize) {
        let new_semi_size = young_size / 2;
        assert_eq!(new_semi_size % PAGE_SIZE, 0);
        let old_semi_size = self.current_semi_size();
        self.set_current_semi_size(new_semi_size);

        self.commit_semi_space(vm, 0, old_semi_size, new_semi_size);
        self.commit_semi_space(vm, 1, old_semi_size, new_semi_size);

        let mut protected = self.protected.lock();
        protected.limit = self.to_committed().end();
        assert!(protected.top <= protected.current_limit);
        assert_eq!(protected.top.align_page_up(), protected.next_page);
        assert!(protected.current_limit <= protected.limit);
    }

    fn commit_semi_space(&self, vm: &VM, semi_space_idx: usize, old_size: usize, new_size: usize) {
        let space = self.semispaces[semi_space_idx];
        assert!(is_page_aligned(old_size));
        assert!(is_page_aligned(new_size));

        if old_size == new_size {
            return;
        }

        let mut protected = self.protected.lock();
        let space_pages = &mut protected.pages[semi_space_idx];
        assert_eq!(space_pages.len(), old_size / PAGE_SIZE);

        if old_size < new_size {
            let size = new_size - old_size;
            let start = space.start().offset(old_size);
            let end = start.offset(size);

            let mut curr = start;
            while curr < end {
                os::commit_at(curr, PAGE_SIZE, MemoryPermission::ReadWrite);

                let page = RegularPage::setup(curr, true, false);
                fill_region(vm, page.object_area_start(), page.object_area_end());
                space_pages.push(page);
                curr = page.end();
            }
        } else {
            assert!(new_size < old_size);
            let target_pages = new_size / PAGE_SIZE;

            while space_pages.len() > target_pages {
                let page = space_pages.pop().expect("missing page");
                os::discard(page.address(), page.size());
            }

            if new_size > 0 {
                let new_end = space.start().offset(new_size);
                assert_eq!(space_pages.first().unwrap().address(), space.start());
                assert_eq!(space_pages.last().unwrap().end(), new_end)
            }
        }

        assert_eq!(space_pages.len(), new_size / PAGE_SIZE);
    }

    fn from_committed(&self) -> Region {
        let size = self.current_semi_size();
        self.from_total().start().region_start(size)
    }

    fn from_total(&self) -> Region {
        self.semispaces[self.from_index()]
    }

    fn from_index(&self) -> usize {
        self.from_index.load(Ordering::Relaxed)
    }

    fn to_committed(&self) -> Region {
        let size = self.current_semi_size();
        self.to_total().start().region_start(size)
    }

    fn to_total(&self) -> Region {
        self.semispaces[self.to_index()]
    }

    fn to_index(&self) -> usize {
        self.from_index() ^ 1
    }

    fn swap_indices(&self) {
        let from_index = self.from_index();
        self.from_index.store(from_index ^ 1, Ordering::Relaxed);
    }

    pub fn committed_size(&self) -> usize {
        self.current_semi_size() * 2
    }

    fn from_pages(&self) -> Vec<RegularPage> {
        create_pages_for_region(self.from_committed())
    }

    pub fn to_pages(&self) -> Vec<RegularPage> {
        create_pages_for_region(self.to_committed())
    }
}

fn create_pages_for_region(region: Region) -> Vec<RegularPage> {
    let mut pages = Vec::new();
    let mut curr = region.start();
    while curr < region.end() {
        let page = RegularPage::from_address_unsafe(curr);
        pages.push(page);
        curr = page.end();
    }
    assert_eq!(curr, region.end());
    pages
}

impl GenerationAllocator for YoungGen {
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        let mut protected = self.protected.lock();
        protected.alloc(vm, min_size, max_size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct YoungGenProtected {
    top: Address,
    current_limit: Address,
    next_page: Address,
    limit: Address,

    pages: [Vec<RegularPage>; 2],
}

impl YoungGenProtected {
    fn alloc(&mut self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        if let Some(region) = self.raw_alloc(vm, min_size, max_size) {
            return Some(region);
        }

        if self.next_page < self.limit {
            fill_region_with(vm, self.top, self.current_limit, false);
            let page = RegularPage::setup(self.next_page, true, false);
            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            self.next_page = page.end();
            assert!(self.next_page <= self.limit);
            fill_region_with(vm, self.top, self.current_limit, false);
            let result = self.raw_alloc(vm, min_size, max_size);
            assert!(result.is_some());
            result
        } else {
            None
        }
    }

    fn raw_alloc(&mut self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        if self.top.offset(min_size) <= self.current_limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(max_size).min(self.current_limit);
            let alloc = Region::new(alloc_start, alloc_end);
            debug_assert!(alloc.size() >= min_size);
            self.top = alloc_end;
            fill_region_with(vm, self.top, self.current_limit, false);
            Some(alloc)
        } else {
            None
        }
    }
}
