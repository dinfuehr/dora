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
    current_size: AtomicUsize,

    protect: bool,

    alloc: YoungAlloc,
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
            current_size: AtomicUsize::new(semi_size),
            protect,
            alloc: YoungAlloc::new(committed_region),
        }
    }

    pub(super) fn setup(&self, vm: &VM) {
        self.commit(vm, self.current_size());
        self.protect_from();

        let to_committed = self.to_committed();
        self.make_pages_iterable(vm, to_committed.start());
    }

    fn make_pages_iterable(&self, vm: &VM, start: Address) {
        let to_committed = self.to_committed();
        assert!(to_committed.valid_top(start));
        let mut curr = start;

        while curr < to_committed.end() {
            let page = RegularPage::setup(curr, true, false);
            fill_region_with(vm, page.object_area_start(), page.object_area_end(), true);
            curr = page.end();
        }
    }

    fn commit(&self, vm: &VM, semi_size: usize) {
        self.commit_semi_space(vm, self.semispaces[0], 0, semi_size);
        self.commit_semi_space(vm, self.semispaces[1], 0, semi_size);
    }

    pub fn object_size(&self) -> usize {
        let protected = self.alloc.protected.lock();
        let start = self.to_total().start();
        protected.top.offset_from(start)
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
            let from_space = self.from_committed();

            os::protect(
                from_space.start,
                from_space.size(),
                MemoryPermission::ReadWrite,
            );
        }
    }

    fn current_size(&self) -> usize {
        self.current_size.load(Ordering::Relaxed)
    }

    fn set_current_size(&self, size: usize) {
        assert!(mem::is_os_page_aligned(size));
        self.current_size.store(size, Ordering::Relaxed);
    }

    pub fn allocated_size(&self) -> usize {
        let protected = self.alloc.protected.lock();
        let to_committed = self.to_committed();
        protected.current_limit.offset_from(to_committed.start())
    }

    pub fn protect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            let from_space = self.from_committed();
            os::protect(from_space.start, from_space.size(), MemoryPermission::None);
        }
    }

    pub fn swap_semi(&self, vm: &VM) {
        self.swap_indices();

        let to_committed = self.to_committed();
        self.make_pages_iterable(vm, to_committed.start());

        self.alloc.reset(to_committed);
    }

    pub fn reset_after_minor_gc(&self) {
        let allocation_end = self.alloc.protected.lock().current_limit;

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
        let old_semi_size = self.current_size();
        self.set_current_size(new_semi_size);

        self.commit_semi_space(vm, self.semispaces[0], old_semi_size, new_semi_size);
        self.commit_semi_space(vm, self.semispaces[1], old_semi_size, new_semi_size);

        self.alloc.resize(self.to_committed().end());
    }

    fn commit_semi_space(&self, vm: &VM, space: Region, old_size: usize, new_size: usize) {
        assert!(is_page_aligned(new_size));

        if old_size == new_size {
            return;
        }

        if old_size < new_size {
            let size = new_size - old_size;
            let start = space.start().offset(old_size);
            let end = start.offset(size);
            os::commit_at(start, size, MemoryPermission::ReadWrite);

            let mut curr = start;
            while curr < end {
                let page = RegularPage::setup(curr, true, false);
                fill_region(vm, page.object_area_start(), page.object_area_end());
                curr = page.end();
            }
        } else {
            assert!(new_size < old_size);
            let size = old_size - new_size;
            let start = space.start().offset(new_size);
            os::discard(start, size);
        }
    }

    fn from_committed(&self) -> Region {
        let size = self.current_size();
        self.from_total().start().region_start(size)
    }

    fn from_total(&self) -> Region {
        self.semispaces[self.from_index()]
    }

    fn from_index(&self) -> usize {
        self.from_index.load(Ordering::Relaxed)
    }

    fn to_committed(&self) -> Region {
        let size = self.current_size();
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
        self.current_size() * 2
    }

    pub fn from_pages(&self) -> Vec<RegularPage> {
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
        let page = RegularPage::from_address(curr);
        pages.push(page);
        curr = page.end();
    }
    assert_eq!(curr, region.end());
    pages
}

impl GenerationAllocator for YoungGen {
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        self.alloc.alloc(vm, min_size, max_size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct YoungAlloc {
    protected: Mutex<YoungAllocProtected>,
}

impl YoungAlloc {
    fn new(region: Region) -> YoungAlloc {
        assert!(region.size() > 0);
        assert_eq!(region.size() % PAGE_SIZE, 0);
        assert!(region.start().is_page_aligned());
        assert!(region.end().is_page_aligned());

        YoungAlloc {
            protected: Mutex::new(YoungAllocProtected {
                top: region.start(),
                current_limit: region.start(),
                next_page: region.start(),
                limit: region.end(),
            }),
        }
    }

    fn alloc(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        let mut protected = self.protected.lock();
        protected.alloc(vm, min_size, max_size)
    }

    fn reset(&self, region: Region) {
        let mut protected = self.protected.lock();
        protected.top = region.start();
        protected.current_limit = region.start();
        protected.next_page = region.start();
        protected.limit = region.end();
    }

    fn reuse(&self, alloc: YoungAllocProtected) {
        let mut protected = self.protected.lock();
        *protected = alloc;
    }

    fn resize(&self, new_limit: Address) {
        let mut protected = self.protected.lock();
        protected.limit = new_limit;
        assert!(protected.top <= protected.current_limit);
        assert_eq!(protected.top.align_page_up(), protected.next_page);
        assert!(protected.current_limit <= protected.limit);
    }
}

struct YoungAllocProtected {
    top: Address,
    current_limit: Address,
    next_page: Address,
    limit: Address,
}

impl YoungAllocProtected {
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
