use std::collections::HashSet;

use fixedbitset::FixedBitSet;
use parking_lot::{Mutex, MutexGuard};
use rand::Rng;

use crate::gc::freelist::FreeList;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::CommonOldGen;
use crate::gc::swiper::{PAGE_HEADER_SIZE, PAGE_SIZE};
use crate::gc::{fill_region, fill_region_with, is_page_aligned};
use crate::gc::{Address, GenerationAllocator, Region};
use crate::mem::ptr_width_usize;
use crate::os::{self, MemoryPermission};
use crate::vm::VM;

pub struct OldGen {
    total: Region,
    protected: Mutex<OldGenProtected>,

    crossing_map: CrossingMap,
    card_table: CardTable,
    config: SharedHeapConfig,
}

impl OldGen {
    pub fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
        config: SharedHeapConfig,
    ) -> OldGen {
        let total = Region::new(start, end);

        let old = OldGen {
            total: total.clone(),
            protected: Mutex::new(OldGenProtected::new(total)),

            crossing_map,
            card_table,
            config,
        };

        old
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn total_start(&self) -> Address {
        self.total.start
    }

    pub fn contains_slow(&self, addr: Address) -> bool {
        let protected = self.protected.lock();
        protected.contains(addr)
    }

    pub fn update_crossing(&self, object_start: Address, object_end: Address) {
        self.crossing_map
            .update(self.total.clone(), object_start, object_end);
    }

    pub fn protected(&self) -> MutexGuard<OldGenProtected> {
        self.protected.lock()
    }

    fn can_add_page(&self) -> bool {
        let mut config = self.config.lock();
        config.grow_old(PAGE_SIZE)
    }

    pub fn fill_alloc_page(&self) {
        let mut protected = self.protected.lock();
        protected.fill_alloc_page();
    }
}

impl CommonOldGen for OldGen {
    fn active_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.active_size()
    }

    fn committed_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.size
    }
}

impl GenerationAllocator for OldGen {
    fn allocate(&self, vm: &VM, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        protected.allocate(vm, self, size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

pub struct OldGenProtected {
    total: Region,
    size: usize,
    top: Address,
    current_limit: Address,
    pages: Vec<Page>,
    free_pages: FixedBitSet,
    num_pages: usize,
    freelist: FreeList,
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        assert!(is_page_aligned(total.size()));
        let num_pages = total.size() / PAGE_SIZE;
        let mut free_pages = FixedBitSet::with_capacity(num_pages);
        free_pages.set_range(.., true);

        OldGenProtected {
            total: total.clone(),
            size: 0,
            top: total.start(),
            current_limit: total.start(),
            pages: Vec::new(),
            free_pages,
            num_pages,
            freelist: FreeList::new(),
        }
    }

    pub fn pages(&self) -> Vec<Page> {
        self.pages.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.start <= addr && addr < self.top
    }

    pub fn fill_alloc_page(&mut self) {
        self.top = self.current_limit;
    }

    pub fn allocate(&mut self, vm: &VM, old: &OldGen, size: usize) -> Option<Address> {
        if let Some(address) = self.raw_alloc(size) {
            fill_region_with(vm, self.top, self.current_limit, false);
            old.update_crossing(self.top, self.current_limit);
            return Some(address);
        }

        fill_region_with(vm, self.top, self.current_limit, false);
        old.update_crossing(self.top, self.current_limit);

        if !old.can_add_page() {
            return None;
        }

        if let Some(page) = self.allocate_page(vm) {
            self.pages.push(page);
            self.pages.sort();

            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            let result = self.raw_alloc(size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(vm, self.top, self.current_limit);
            old.update_crossing(self.top, self.current_limit);

            result
        } else {
            None
        }
    }

    pub fn commit_pages(&mut self, pages: &[Page]) {
        let previous_page_set: HashSet<Page> = HashSet::from_iter(self.pages.iter().cloned());

        for page in pages {
            if !previous_page_set.contains(page) {
                os::commit_at(page.start(), page.size(), MemoryPermission::ReadWrite);
            }
        }
    }

    pub fn reset_after_gc(&mut self, pages: Vec<Page>, top: Address, current_limit: Address) {
        self.free_pages.set_range(.., true);

        let previous_pages = std::mem::replace(&mut self.pages, pages);
        let page_set: HashSet<Page> = HashSet::from_iter(self.pages.iter().cloned());

        for page in &self.pages {
            let page_idx = page.start().offset_from(self.total.start()) / PAGE_SIZE;
            assert!(self.free_pages.contains(page_idx));
            self.free_pages.set(page_idx, false);
            assert!(!self.free_pages.contains(page_idx));
        }

        for page in previous_pages {
            if !page_set.contains(&page) {
                os::discard(page.start(), page.size());
            }
        }

        self.top = top;
        self.current_limit = current_limit;
    }

    pub fn free_page(&mut self, page: Page) {
        let idx = self
            .pages
            .iter()
            .position(|&p| p == page)
            .expect("missing page");
        self.pages.swap_remove(idx);
        os::discard(page.start(), page.size());
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.total.start())
    }

    fn allocate_page(&mut self, vm: &VM) -> Option<Page> {
        if let Some(page_idx) = self.select_free_page() {
            let page_start = self.total.start().offset(page_idx * PAGE_SIZE);
            assert!(self.total.contains(page_start));
            let page = Page::from_address(page_start);
            assert!(self.free_pages.contains(page_idx));
            self.free_pages.set(page_idx, false);
            os::commit_at(page.start(), PAGE_SIZE, MemoryPermission::ReadWrite);
            page.initialize_header();
            fill_region(vm, page.object_area_start(), page.object_area_end());
            Some(page)
        } else {
            None
        }
    }

    fn select_free_page(&mut self) -> Option<usize> {
        let mut rng = rand::thread_rng();

        for _ in 0..3 {
            let page_idx = rng.gen_range(0..self.num_pages);

            if self.free_pages.contains(page_idx) {
                return Some(page_idx);
            }
        }

        if let Some(first_page) = self.free_pages.ones().next() {
            Some(first_page)
        } else {
            None
        }
    }

    fn raw_alloc(&mut self, size: usize) -> Option<Address> {
        let next = self.top.offset(size);

        if next <= self.current_limit {
            let result = self.top;
            self.top = next;
            Some(result)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Page(Address);

impl Page {
    pub fn new(start: Address) -> Page {
        Page(start)
    }

    pub fn from_address(value: Address) -> Page {
        let page_start = value.to_usize() & !(PAGE_SIZE - 1);
        Page::new(page_start.into())
    }

    pub fn initialize_header(&self) {
        unsafe {
            let header = std::slice::from_raw_parts_mut(
                self.start().to_mut_ptr::<usize>(),
                PAGE_HEADER_SIZE / ptr_width_usize(),
            );

            header.fill(0xDEAD2BAD);
        }
    }

    pub fn area(&self) -> Region {
        Region::new(self.start(), self.end())
    }

    pub fn start(&self) -> Address {
        self.0
    }

    pub fn end(&self) -> Address {
        self.start().offset(PAGE_SIZE)
    }

    pub fn size(&self) -> usize {
        PAGE_SIZE
    }

    pub fn object_area(&self) -> Region {
        Region::new(self.object_area_start(), self.object_area_end())
    }

    pub fn object_area_start(&self) -> Address {
        self.start().offset(PAGE_HEADER_SIZE)
    }

    pub fn object_area_end(&self) -> Address {
        self.end()
    }
}
