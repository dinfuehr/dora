use std::collections::HashSet;

use parking_lot::{Mutex, MutexGuard};

use crate::gc::fill_region;
use crate::gc::freelist::FreeList;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::CommonOldGen;
use crate::gc::swiper::{PAGE_HEADER_SIZE, PAGE_SIZE};
use crate::gc::{Address, GenerationAllocator, Region};
use crate::mem::ptr_width_usize;
use crate::os::{self, MemoryPermission};
use crate::vm::get_vm;

pub struct OldGen {
    total: Region,
    protected: Mutex<OldGenProtected>,

    crossing_map: CrossingMap,
    _card_table: CardTable,
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
            _card_table: card_table,
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

    fn add_page(&self, page_start: Address) -> Option<Page> {
        assert!(page_start.is_page_aligned());
        let page_end = page_start.offset(PAGE_SIZE);

        if page_end > self.total.end() {
            return None;
        }

        os::commit_at(page_start, PAGE_SIZE, MemoryPermission::ReadWrite);
        Some(Page::new(page_start))
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
    fn allocate(&self, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();

        if let Some(address) = protected.raw_alloc(size) {
            fill_region(get_vm(), protected.top, protected.current_limit);
            self.update_crossing(protected.top, protected.current_limit);
            return Some(address);
        }

        fill_region(get_vm(), protected.top, protected.current_limit);
        self.update_crossing(protected.top, protected.current_limit);

        if !self.can_add_page() {
            return None;
        }

        if let Some(page) = self.add_page(protected.current_limit) {
            protected.pages.push(page);

            page.initialize_header();

            protected.top = page.object_area_start();
            protected.current_limit = page.object_area_end();
            let result = protected.raw_alloc(size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(get_vm(), protected.top, protected.current_limit);
            self.update_crossing(protected.top, protected.current_limit);

            result
        } else {
            None
        }
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

pub struct OldGenProtected {
    pub total: Region,
    pub size: usize,
    pub top: Address,
    pub current_limit: Address,
    pub pages: Vec<Page>,
    pub freelist: FreeList,
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        OldGenProtected {
            total: total.clone(),
            size: 0,
            top: total.start(),
            current_limit: total.start(),
            pages: Vec::new(),
            freelist: FreeList::new(),
        }
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.start <= addr && addr < self.top
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
        let previous_pages = std::mem::replace(&mut self.pages, pages);
        let page_set: HashSet<Page> = HashSet::from_iter(self.pages.iter().cloned());

        for page in previous_pages {
            if !page_set.contains(&page) {
                os::discard(page.start(), page.size());
            }
        }

        self.top = top;
        self.current_limit = current_limit;
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.total.start())
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
