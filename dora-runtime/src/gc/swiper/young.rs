use parking_lot::Mutex;

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::swiper::{Page, PAGE_SIZE};
use crate::gc::{
    fill_region, fill_region_with, is_page_aligned, Address, GenerationAllocator, Region,
};
use crate::mem;
use crate::os::{self, MemoryPermission};
use crate::vm::{get_vm, VM};

pub struct YoungGen {
    total: Region,

    semispaces: [Region; 2],
    from_index: AtomicUsize,
    current_size: AtomicUsize,

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

        let young = YoungGen {
            total,
            semispaces,
            from_index: AtomicUsize::new(from_region),
            current_size: AtomicUsize::new(semi_size),
            protect,
            protected: Mutex::new(YoungGenProtected {
                start: to_region.start(),
                top: to_region.start(),
                current_limit: to_region.start().offset(PAGE_SIZE),
                limit: to_region.start().offset(semi_size),
                age_marker: to_region.start(),
            }),
        };

        young.commit(semi_size);
        young.protect_from();

        young
    }

    pub(super) fn setup(&self, vm: &VM) {
        let to_committed = self.to_committed();
        self.make_pages_iterable(vm, to_committed.start());
    }

    fn make_pages_iterable(&self, vm: &VM, start: Address) {
        let to_committed = self.to_committed();
        assert!(to_committed.valid_top(start));
        let mut curr = start;

        while curr < to_committed.end() {
            let page = Page::from_address(curr);
            page.initialize_iterable_header(vm);
            fill_region_with(vm, page.object_area_start(), page.object_area_end(), true);
            curr = page.end();
        }
    }

    fn commit(&self, semi_size: usize) {
        self.commit_semi_space(self.semispaces[0], 0, semi_size);
        self.commit_semi_space(self.semispaces[1], 0, semi_size);
    }

    pub fn object_size(&self) -> usize {
        let protected = self.protected.lock();
        let start = self.to_total().start();
        protected.top.offset_from(start)
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn reset_after_full_gc(&self, vm: &VM) {
        self.unprotect_from();
        self.swap_semi();
        self.protect_from();

        self.make_pages_iterable(vm, self.to_committed().start());

        let mut protected = self.protected.lock();
        protected.age_marker = protected.top;
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

    pub fn protect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            let from_space = self.from_committed();
            os::protect(from_space.start, from_space.size(), MemoryPermission::None);
        }
    }

    pub fn age_marker(&self) -> Address {
        let protected = self.protected.lock();
        protected.age_marker
    }

    pub fn swap_semi(&self) {
        self.swap_indices();

        let to_committed = self.to_committed();
        fill_region(get_vm(), to_committed.start(), to_committed.end());

        let mut protected = self.protected.lock();
        protected.reset_alloc(to_committed);
    }

    pub fn reset_after_minor_gc(&self, vm: &VM, top: Address, current_limit: Address) {
        self.make_pages_iterable(vm, current_limit);

        let mut protected = self.protected.lock();
        protected.set_top_after_minor(top, current_limit);
    }

    pub fn bump_alloc(&self, vm: &VM, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        protected.alloc(vm, self, size)
    }

    pub fn resize_after_gc(&self, young_size: usize) {
        let new_semi_size = young_size / 2;
        assert_eq!(new_semi_size % PAGE_SIZE, 0);
        let old_semi_size = self.current_size();
        self.set_current_size(new_semi_size);

        self.commit_semi_space(self.semispaces[0], old_semi_size, new_semi_size);
        self.commit_semi_space(self.semispaces[1], old_semi_size, new_semi_size);

        let mut protected = self.protected.lock();
        assert!(protected.top <= protected.current_limit);
        protected.current_limit = self.to_committed().end();
        protected.limit = self.to_committed().end();
        fill_region(get_vm(), protected.top, protected.limit);
    }

    fn commit_semi_space(&self, space: Region, old_size: usize, new_size: usize) {
        assert!(is_page_aligned(new_size));

        if old_size == new_size {
            return;
        }

        if old_size < new_size {
            let size = new_size - old_size;
            let start = space.start().offset(old_size);
            os::commit_at(start, size, MemoryPermission::ReadWrite);
        } else {
            assert!(new_size < old_size);
            let size = old_size - new_size;
            let start = space.start().offset(new_size);
            os::discard(start, size);
        }
    }

    pub fn from_committed(&self) -> Region {
        let size = self.current_size();
        self.from_total().start().region_start(size)
    }

    pub fn from_total(&self) -> Region {
        self.semispaces[self.from_index()]
    }

    fn from_index(&self) -> usize {
        self.from_index.load(Ordering::Relaxed)
    }

    pub fn to_committed(&self) -> Region {
        let size = self.current_size();
        self.to_total().start().region_start(size)
    }

    pub fn to_total(&self) -> Region {
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
}

impl GenerationAllocator for YoungGen {
    fn allocate(&self, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        protected.alloc(get_vm(), self, size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct YoungGenProtected {
    start: Address,
    top: Address,
    current_limit: Address,
    limit: Address,
    age_marker: Address,
}

impl YoungGenProtected {
    fn alloc(&mut self, vm: &VM, young: &YoungGen, size: usize) -> Option<Address> {
        if let Some(address) = self.raw_alloc(vm, young, size) {
            return Some(address);
        }

        if self.current_limit < self.limit {
            fill_region_with(vm, self.top, self.current_limit, false);
            let page = Page::from_address(self.current_limit);
            page.initialize_iterable_header(vm);
            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            assert!(self.current_limit <= self.limit);
            fill_region_with(vm, self.top, self.current_limit, false);
            let result = self.raw_alloc(vm, young, size);
            assert!(result.is_some());
            result
        } else {
            None
        }
    }

    fn raw_alloc(&mut self, vm: &VM, _young: &YoungGen, size: usize) -> Option<Address> {
        let next = self.top.offset(size);

        if next <= self.current_limit {
            let result = self.top;
            self.top = next;
            fill_region_with(vm, self.top, self.current_limit, false);
            Some(result)
        } else {
            None
        }
    }

    fn reset_alloc(&mut self, region: Region) {
        self.start = region.start();
        self.top = region.start();
        self.current_limit = region.end();
        self.limit = region.end();
    }

    fn set_top_after_minor(&mut self, top: Address, current_limit: Address) {
        assert!(self.start <= top);
        assert!(top <= current_limit);
        assert!(current_limit <= self.limit);
        assert!(current_limit.is_page_aligned());
        self.top = top;
        self.current_limit = current_limit;
        self.age_marker = top;
    }
}
