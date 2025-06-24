use parking_lot::Mutex;

use crate::gc::swiper::{LargePage, RegularPage, SharedHeapConfig, align_page_up, is_page_aligned};
use crate::gc::{Address, Region, fill_region};
use crate::mem::is_os_page_aligned;
use crate::os::{self, MemoryPermission};
use crate::vm::VM;

use super::PAGE_SIZE;

pub struct Heap {
    total: Region,
    pages: usize,
    config: SharedHeapConfig,
    protected: Mutex<MixedHeapProtected>,
}

impl Heap {
    pub fn new(total: Region, config: SharedHeapConfig) -> Heap {
        assert!(total.start().is_page_aligned());
        assert!(total.end().is_page_aligned());
        assert!(is_page_aligned(total.size()));
        let pages = total.size() / PAGE_SIZE;

        Heap {
            total,
            pages,
            config,
            protected: Mutex::new(MixedHeapProtected {
                elements: vec![total],
                committed_sizes: CommittedSizes {
                    young: 0,
                    old: 0,
                    large: 0,
                },
            }),
        }
    }

    pub fn pages(&self) -> usize {
        self.pages
    }

    pub fn start_address(&self) -> Address {
        self.total.start()
    }

    pub fn alloc_large_page(&self, object_size: usize) -> Option<LargePage> {
        let (committed_size, _) = LargePage::compute_sizes(object_size);

        let mut config = self.config.lock();

        if !config.grow_old(committed_size) {
            return None;
        }

        self.protected.lock().alloc_large_page(object_size)
    }

    pub fn free_large_page(&self, page: LargePage) {
        self.protected.lock().free_large_page(page);
    }

    pub fn alloc_regular_old_page(&self, vm: &VM) -> Option<RegularPage> {
        let mut config = self.config.lock();

        if !config.grow_old(PAGE_SIZE) {
            return None;
        }

        self.protected.lock().alloc_regular_page(vm, false, false)
    }

    pub fn free_regular_old_page(&self, page: RegularPage) {
        self.protected.lock().free_regular_page(page, false)
    }

    pub fn alloc_regular_young_page(&self, vm: &VM) -> Option<RegularPage> {
        self.protected.lock().alloc_regular_page(vm, true, false)
    }

    pub fn free_regular_young_page(&self, page: RegularPage) {
        self.protected.lock().free_regular_page(page, true)
    }

    pub fn merge_free_regions(&self) {
        self.protected.lock().merge_free_regions();
    }

    pub fn promote_page(&self, _page: RegularPage) {
        let mut protected = self.protected.lock();
        protected.committed_sizes.young -= PAGE_SIZE;
        protected.committed_sizes.old += PAGE_SIZE;
    }

    pub fn committed_size(&self) -> usize {
        self.protected.lock().committed_sizes.total()
    }

    pub fn committed_sizes(&self) -> CommittedSizes {
        let protected = self.protected.lock();
        protected.committed_sizes.clone()
    }
}

#[derive(Clone)]
pub struct CommittedSizes {
    pub young: usize,
    pub old: usize,
    pub large: usize,
}

impl CommittedSizes {
    fn total(&self) -> usize {
        self.young + self.old + self.large
    }
}

struct MixedHeapProtected {
    elements: Vec<Region>,
    committed_sizes: CommittedSizes,
}

impl MixedHeapProtected {
    fn alloc_large_page(&mut self, object_size: usize) -> Option<LargePage> {
        let (committed_size, reserved_size) = LargePage::compute_sizes(object_size);

        if let Some(region) = self.alloc_pages(reserved_size) {
            os::commit_at(region.start(), committed_size, MemoryPermission::ReadWrite);
            let page = LargePage::setup(region.start(), committed_size);
            self.committed_sizes.large += committed_size;
            Some(page)
        } else {
            None
        }
    }

    fn alloc_regular_page(
        &mut self,
        vm: &VM,
        is_young: bool,
        is_readonly: bool,
    ) -> Option<RegularPage> {
        if let Some(region) = self.alloc_pages(PAGE_SIZE) {
            os::commit_at(region.start(), PAGE_SIZE, MemoryPermission::ReadWrite);
            let page = RegularPage::setup(region.start(), is_young, is_readonly);
            fill_region(vm, page.object_area_start(), page.object_area_end());
            if is_young {
                self.committed_sizes.young += PAGE_SIZE;
            } else {
                self.committed_sizes.old += PAGE_SIZE;
            }
            Some(page)
        } else {
            None
        }
    }

    fn alloc_pages(&mut self, size: usize) -> Option<Region> {
        assert!(is_page_aligned(size));
        let len = self.elements.len();

        for i in 0..len {
            if self.elements[i].size() < size {
                continue;
            }

            let range = self.elements[i];
            let addr = range.start;

            if range.size() == size {
                self.elements.remove(i);
            } else {
                self.elements[i] = Region::new(range.start.offset(size), range.end);
            }

            return Some(addr.region_start(size));
        }

        None
    }

    fn free_large_page(&mut self, page: LargePage) {
        let committed_size = page.committed_size();
        assert!(is_os_page_aligned(committed_size));
        let reserved_size = align_page_up(committed_size);
        os::discard(page.address(), committed_size);
        self.elements
            .push(page.address().region_start(reserved_size));
        self.committed_sizes.large -= committed_size;
    }

    fn free_regular_page(&mut self, page: RegularPage, is_young: bool) {
        os::discard(page.address(), PAGE_SIZE);
        self.elements.push(page.address().region_start(PAGE_SIZE));

        if is_young {
            self.committed_sizes.young -= PAGE_SIZE;
        } else {
            self.committed_sizes.old -= PAGE_SIZE;
        }
    }

    fn merge_free_regions(&mut self) {
        self.elements
            .sort_unstable_by(|lhs, rhs| lhs.start.to_usize().cmp(&rhs.start.to_usize()));

        let len = self.elements.len();
        let mut last_element = 0;

        for i in 1..len {
            if self.elements[last_element].end == self.elements[i].start {
                self.elements[last_element].end = self.elements[i].end;
            } else {
                last_element += 1;
                self.elements[last_element] = self.elements[i];
            }
        }

        self.elements.truncate(last_element + 1);
    }
}
