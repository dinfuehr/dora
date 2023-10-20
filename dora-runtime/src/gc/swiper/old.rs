use parking_lot::{Mutex, MutexGuard};
use std::cmp::min;

use crate::gc::swiper::card::CardTable;
use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::crossing::CrossingMap;
use crate::gc::swiper::CommonOldGen;
use crate::gc::swiper::REGION_SIZE;
use crate::gc::{Address, Region};
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

    pub fn alloc(&self, size: usize) -> Address {
        let mut protected = self.protected.lock();
        protected.alloc(&self.config, size)
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

    pub fn dump_regions(&self) {
        let protected = self.protected.lock();
        protected.dump_regions();
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
}

impl OldGenProtected {
    fn new(total: Region) -> OldGenProtected {
        OldGenProtected {
            total: total.clone(),
            size: 0,
            top: total.start(),
            current_limit: total.start(),
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

    pub fn alloc(&mut self, config: &SharedHeapConfig, size: usize) -> Address {
        assert!(self.top <= self.current_limit);
        let ptr = self.pure_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        {
            let mut config = config.lock();

            if !config.grow_old(REGION_SIZE) {
                return Address::null();
            }
        }

        if !self.extend(REGION_SIZE) {
            return Address::null();
        }

        self.pure_alloc(size)
    }

    fn pure_alloc(&mut self, size: usize) -> Address {
        let next = self.top.offset(size);

        if next <= self.current_limit {
            let result = self.top;
            self.top = next;
            result
        } else {
            Address::null()
        }
    }

    fn extend(&mut self, size: usize) -> bool {
        let new_limit = self.current_limit.offset(size);

        if new_limit > self.total.end() {
            return false;
        }

        os::commit_at(self.current_limit, size, MemoryPermission::ReadWrite);
        self.current_limit = new_limit;
        true
    }

    pub fn dump_regions(&self) {
        println!("OLD gen: {}-{}", self.total, self.top);
    }
}

#[derive(Clone)]
pub struct OldGenRegion {
    // Region boundaries. NOT necessarily page aligned!
    total: Region,

    // Maximum boundaries of mappings. Page aligned.
    total_mapping: Region,

    // Next object in this region is allocated here.
    top: Address,

    // Memory is mapped until here
    mapping_top: Address,
}

impl OldGenRegion {
    fn single(region: Region) -> OldGenRegion {
        assert!(region.start.is_page_aligned());

        OldGenRegion {
            total: region.clone(),
            top: region.start,
            total_mapping: region.clone(),
            mapping_top: region.start,
        }
    }

    pub fn new(
        total: Region,
        top: Address,
        total_mapping: Region,
        mapping_top: Address,
    ) -> OldGenRegion {
        assert!(total.valid_top(top));
        assert!(total_mapping.valid_top(mapping_top));

        OldGenRegion {
            total,
            total_mapping,
            top,
            mapping_top,
        }
    }

    pub fn start(&self) -> Address {
        self.total.start
    }

    pub fn top(&self) -> Address {
        self.top
    }

    #[allow(dead_code)]
    pub fn size(&self) -> usize {
        self.total.size()
    }

    pub fn active_size(&self) -> usize {
        self.top.offset_from(self.total.start)
    }

    pub fn active_region(&self) -> Region {
        Region::new(self.total.start, self.top)
    }

    pub fn total_region(&self) -> Region {
        self.total.clone()
    }

    pub fn committed_region(&self) -> Region {
        Region::new(self.mapping_start(), self.mapping_end())
    }

    fn mapping_start(&self) -> Address {
        self.total_mapping.start
    }

    fn mapping_end(&self) -> Address {
        self.mapping_top
    }

    #[allow(dead_code)]
    fn mapping_size(&self) -> usize {
        self.mapping_end().offset_from(self.mapping_start())
    }

    fn pure_alloc(&mut self, size: usize) -> Option<Address> {
        let new_top = self.top.offset(size);
        debug_assert!(self.top <= self.mapping_top);
        debug_assert!(self.top <= self.total.end);

        if new_top <= min(self.mapping_top, self.total.end) {
            let addr = self.top;
            self.top = new_top;
            return Some(addr);
        }

        None
    }

    fn extend(&mut self, size: usize) -> bool {
        let new_mapping_top = self.mapping_top.offset(size);

        if new_mapping_top <= self.total_mapping.end {
            os::commit_at(self.mapping_top, size, MemoryPermission::ReadWrite);
            self.mapping_top = new_mapping_top;

            true
        } else {
            false
        }
    }
}
