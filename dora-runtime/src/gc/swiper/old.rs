use fixedbitset::FixedBitSet;
use parking_lot::{Mutex, MutexGuard};
use rand::Rng;

use crate::gc::freelist::FreeList;
use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::{CommonOldGen, RegularPage, PAGE_SIZE};
use crate::gc::{fill_region, fill_region_with, is_page_aligned};
use crate::gc::{Address, GenerationAllocator, Region};
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
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Region> {
        let mut protected = self.protected.lock();
        protected.allocate(vm, self, false, min_size, max_size)
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
    pages: Vec<RegularPage>,
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

    pub fn pages(&self) -> Vec<RegularPage> {
        self.pages.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.start <= addr && addr < self.top
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
        old: &OldGen,
        is_gc: bool,
        min_size: usize,
        max_size: usize,
    ) -> Option<Region> {
        if let Some(region) = self.raw_alloc(min_size, max_size) {
            fill_region_with(vm, self.top, self.current_limit, false);
            old.update_crossing(self.top, self.current_limit);
            return Some(region);
        }

        fill_region_with(vm, self.top, self.current_limit, false);
        old.update_crossing(self.top, self.current_limit);

        let free_space = self.freelist.alloc(min_size);

        if free_space.is_non_null() {
            self.top = free_space.addr();
            self.current_limit = self.top.offset(free_space.size());

            let region = self
                .raw_alloc(min_size, max_size)
                .expect("allocation failed");

            fill_region_with(vm, self.top, self.current_limit, false);
            old.update_crossing(self.top, self.current_limit);
            return Some(region);
        }

        if !is_gc && !old.can_add_page() {
            return None;
        }

        if let Some(page) = self.allocate_page(vm) {
            self.pages.push(page);
            self.pages.sort();
            self.size += PAGE_SIZE;

            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            let result = self.raw_alloc(min_size, max_size);
            assert!(result.is_some());

            // Make rest of page iterable.
            fill_region(vm, self.top, self.current_limit);
            old.update_crossing(self.top, self.current_limit);

            result
        } else {
            None
        }
    }

    pub fn free_page(&mut self, page: RegularPage) {
        let idx = self
            .pages
            .iter()
            .position(|&p| p == page)
            .expect("missing page");
        self.pages.swap_remove(idx);
        self.size -= PAGE_SIZE;
        let page_idx = page.address().offset_from(self.total.start()) / PAGE_SIZE;
        self.free_pages.insert(page_idx);
        os::discard(page.address(), page.size());
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.total.start())
    }

    fn allocate_page(&mut self, vm: &VM) -> Option<RegularPage> {
        if let Some(page_idx) = self.select_free_page() {
            let page_start = self.total.start().offset(page_idx * PAGE_SIZE);
            assert!(self.total.contains(page_start));
            assert!(self.free_pages.contains(page_idx));
            self.free_pages.set(page_idx, false);
            os::commit_at(page_start, PAGE_SIZE, MemoryPermission::ReadWrite);
            let page = RegularPage::setup(page_start, false, false);
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
