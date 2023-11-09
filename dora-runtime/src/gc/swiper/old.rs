use parking_lot::{Mutex, MutexGuard};

use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::CommonOldGen;
use crate::gc::swiper::PAGE_SIZE;
use crate::gc::{Address, GenerationAllocator, Region};
use crate::os::{self, MemoryPermission};

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

pub struct OldGenProtected {
    pub total: Region,
    pub size: usize,
    pub top: Address,
    pub current_limit: Address,
    pub pages: Vec<Address>,
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        OldGenProtected {
            total: total.clone(),
            size: 0,
            top: total.start(),
            current_limit: total.start(),
            pages: Vec::new(),
        }
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.start <= addr && addr < self.top
    }

    pub fn active_region(&self) -> Region {
        Region::new(self.total.start(), self.top)
    }

    pub fn commit_single_region(&mut self, top: Address, old_top: Address) {
        let top_aligned = top.align_region_up();
        let old_top_aligned = old_top.align_region_up();

        if top_aligned > old_top_aligned {
            let size = top_aligned.offset_from(old_top_aligned);
            os::commit_at(old_top_aligned, size, MemoryPermission::ReadWrite);
        }
    }

    pub fn update_single_region(&mut self, top: Address) {
        self.top = top;
        self.current_limit = top.align_region_up();
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.total.start())
    }

    fn alloc(&mut self, config: &SharedHeapConfig, size: usize) -> Option<Address> {
        assert!(self.top <= self.current_limit);

        if let Some(address) = self.raw_alloc(size) {
            return Some(address);
        }

        {
            let mut config = config.lock();

            if !config.grow_old(PAGE_SIZE) {
                return None;
            }
        }

        if !self.add_page() {
            return None;
        }

        self.raw_alloc(size)
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

    fn add_page(&mut self) -> bool {
        let page_start = self.current_limit;
        let page_end = page_start.offset(PAGE_SIZE);

        if page_end > self.total.end() {
            return false;
        }

        os::commit_at(self.current_limit, PAGE_SIZE, MemoryPermission::ReadWrite);
        self.current_limit = page_end;
        self.pages.push(page_start);
        true
    }
}

impl GenerationAllocator for OldGen {
    fn allocate(&self, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        protected.alloc(&self.config, size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}
