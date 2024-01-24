use parking_lot::Mutex;

use crate::gc::swiper::{align_page_up, is_page_aligned, LargePage, SharedHeapConfig};
use crate::gc::Region;
use crate::mem::is_os_page_aligned;
use crate::os::{self, MemoryPermission};

pub struct MixedHeap {
    total: Region,
    config: SharedHeapConfig,
    protected: Mutex<MixedHeapProtected>,
}

impl MixedHeap {
    pub fn new(total: Region, config: SharedHeapConfig) -> MixedHeap {
        MixedHeap {
            total,
            config,
            protected: Mutex::new(MixedHeapProtected {
                elements: vec![total],
                committed_size: 0,
            }),
        }
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

    pub fn merge_free_regions(&self) {
        self.protected.lock().merge_free_regions();
    }

    pub fn committed_size(&self) -> usize {
        self.protected.lock().committed_size
    }
}

struct MixedHeapProtected {
    elements: Vec<Region>,
    committed_size: usize,
}

impl MixedHeapProtected {
    fn alloc_large_page(&mut self, object_size: usize) -> Option<LargePage> {
        let (committed_size, reserved_size) = LargePage::compute_sizes(object_size);

        if let Some(region) = self.alloc_pages(reserved_size) {
            os::commit_at(region.start(), committed_size, MemoryPermission::ReadWrite);
            let page = LargePage::setup(region.start(), committed_size);
            self.committed_size += committed_size;
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
        self.committed_size -= committed_size;
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
