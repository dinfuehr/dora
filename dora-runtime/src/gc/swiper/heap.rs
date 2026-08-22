use parking_lot::Mutex;

use crate::gc::swiper::{LargePage, RegularPage, SharedHeapConfig, align_page_up, is_page_aligned};
use crate::gc::{Address, Region, fill_region};
use crate::mem::is_os_page_aligned;
use crate::os::{self, MemoryPermission};
use crate::runtime::Runtime;

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
                free_regions: FreeRegions::new(total),
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

    pub fn total(&self) -> Region {
        self.total
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

    pub fn alloc_regular_old_page(&self, rt: &Runtime) -> Option<RegularPage> {
        let mut config = self.config.lock();

        if !config.grow_old(PAGE_SIZE) {
            return None;
        }

        self.protected.lock().alloc_regular_page(rt, false, false)
    }

    pub fn free_regular_old_page(&self, page: RegularPage) {
        self.protected.lock().free_regular_page(page, false)
    }

    pub fn alloc_regular_young_page(&self, rt: &Runtime) -> Option<RegularPage> {
        self.protected.lock().alloc_regular_page(rt, true, false)
    }

    pub fn free_regular_young_page(&self, page: RegularPage) {
        self.protected.lock().free_regular_page(page, true)
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
    free_regions: FreeRegions,
    committed_sizes: CommittedSizes,
}

impl MixedHeapProtected {
    fn alloc_large_page(&mut self, object_size: usize) -> Option<LargePage> {
        let (committed_size, reserved_size) = LargePage::compute_sizes(object_size);

        if let Some(region) = self.free_regions.alloc(reserved_size) {
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
        rt: &Runtime,
        is_young: bool,
        is_readonly: bool,
    ) -> Option<RegularPage> {
        if let Some(region) = self.free_regions.alloc(PAGE_SIZE) {
            os::commit_at(region.start(), PAGE_SIZE, MemoryPermission::ReadWrite);
            let page = RegularPage::setup(region.start(), is_young, is_readonly);
            fill_region(rt, page.object_area_start(), page.object_area_end());
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

    fn free_large_page(&mut self, page: LargePage) {
        let committed_size = page.committed_size();
        assert!(is_os_page_aligned(committed_size));
        let reserved_size = align_page_up(committed_size);
        os::discard(page.address(), committed_size);
        self.free_regions
            .add(page.address().region_start(reserved_size));
        self.committed_sizes.large -= committed_size;
    }

    fn free_regular_page(&mut self, page: RegularPage, is_young: bool) {
        os::discard(page.address(), PAGE_SIZE);
        self.free_regions
            .add(page.address().region_start(PAGE_SIZE));

        if is_young {
            self.committed_sizes.young -= PAGE_SIZE;
        } else {
            self.committed_sizes.old -= PAGE_SIZE;
        }
    }
}

struct FreeRegions {
    elements: Vec<Region>,
}

impl FreeRegions {
    fn new(total: Region) -> FreeRegions {
        FreeRegions {
            elements: vec![total],
        }
    }

    fn alloc(&mut self, size: usize) -> Option<Region> {
        assert!(is_page_aligned(size));

        for idx in 0..self.elements.len() {
            let region = self.elements[idx];

            if region.size() < size {
                continue;
            }

            let result = region.start.region_start(size);

            if region.size() == size {
                self.elements.remove(idx);
            } else {
                self.elements[idx] = Region::new(region.start.offset(size), region.end);
            }

            return Some(result);
        }

        None
    }

    fn add(&mut self, region: Region) {
        assert!(region.start < region.end);

        let idx = self
            .elements
            .partition_point(|element| element.start < region.start);

        if idx > 0 {
            assert!(self.elements[idx - 1].end <= region.start);
        }

        if idx < self.elements.len() {
            assert!(region.end <= self.elements[idx].start);
        }

        let merge_left = idx > 0 && self.elements[idx - 1].end == region.start;
        let merge_right = idx < self.elements.len() && region.end == self.elements[idx].start;

        match (merge_left, merge_right) {
            (true, true) => {
                self.elements[idx - 1].end = self.elements[idx].end;
                self.elements.remove(idx);
            }
            (true, false) => self.elements[idx - 1].end = region.end,
            (false, true) => self.elements[idx].start = region.start,
            (false, false) => self.elements.insert(idx, region),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn region(start: usize, end: usize) -> Region {
        Region::new(
            Address::from(start * PAGE_SIZE),
            Address::from(end * PAGE_SIZE),
        )
    }

    fn empty_free_regions() -> FreeRegions {
        let mut free_regions = FreeRegions::new(region(0, 4));
        assert_eq!(free_regions.alloc(4 * PAGE_SIZE), Some(region(0, 4)));
        free_regions
    }

    #[test]
    fn add_keeps_regions_sorted() {
        let mut free_regions = empty_free_regions();

        free_regions.add(region(2, 3));
        free_regions.add(region(0, 1));

        assert_eq!(free_regions.elements, vec![region(0, 1), region(2, 3)]);
    }

    #[test]
    fn add_merges_with_left_region() {
        let mut free_regions = empty_free_regions();

        free_regions.add(region(0, 1));
        free_regions.add(region(1, 2));

        assert_eq!(free_regions.elements, vec![region(0, 2)]);
    }

    #[test]
    fn add_merges_with_right_region() {
        let mut free_regions = empty_free_regions();

        free_regions.add(region(1, 2));
        free_regions.add(region(0, 1));

        assert_eq!(free_regions.elements, vec![region(0, 2)]);
    }

    #[test]
    fn add_merges_with_both_regions() {
        let mut free_regions = empty_free_regions();

        free_regions.add(region(0, 1));
        free_regions.add(region(2, 3));
        free_regions.add(region(1, 2));

        assert_eq!(free_regions.elements, vec![region(0, 3)]);
    }
}
