use parking_lot::Mutex;

use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::LargePage;
use crate::gc::{align_page_up, is_page_aligned, Address, Region};
use crate::mem;
use crate::os::{self, MemoryPermission};

pub struct LargeSpace {
    total: Region,
    space: Mutex<LargeSpaceProtected>,
    config: SharedHeapConfig,
}

impl LargeSpace {
    pub fn new(start: Address, end: Address, config: SharedHeapConfig) -> LargeSpace {
        let total = Region::new(start, end);
        assert!(is_page_aligned(total.size()));
        assert!(start.is_page_aligned());
        assert!(end.is_page_aligned());

        LargeSpace {
            total,
            space: Mutex::new(LargeSpaceProtected::new(start, end)),
            config,
        }
    }

    pub fn alloc(&self, object_size: usize) -> Option<Address> {
        let (committed_size, _) = LargePage::compute_sizes(object_size);

        let mut space = self.space.lock();
        let mut config = self.config.lock();

        if !config.grow_old(committed_size) {
            return None;
        }

        space.allocate_object(object_size)
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn head(&self) -> Option<LargePage> {
        let space = self.space.lock();
        space.head
    }

    pub fn iterate_pages<F>(&self, f: F)
    where
        F: FnMut(LargePage),
    {
        let mut space = self.space.lock();
        space.visit_pages(f);
    }

    pub fn remove_pages<F>(&self, f: F)
    where
        F: FnMut(LargePage) -> bool,
    {
        let mut space = self.space.lock();
        space.remove_pages(f);
    }

    pub fn committed_size(&self) -> usize {
        let space = self.space.lock();
        space.committed_size()
    }
}

struct LargeSpaceProtected {
    elements: Vec<Region>,
    head: Option<LargePage>,
    committed_size: usize,
}

impl LargeSpaceProtected {
    fn new(start: Address, end: Address) -> LargeSpaceProtected {
        LargeSpaceProtected {
            elements: vec![Region::new(start, end)],
            head: None,
            committed_size: 0,
        }
    }

    fn committed_size(&self) -> usize {
        self.committed_size
    }

    fn allocate_object(&mut self, object_size: usize) -> Option<Address> {
        let (committed_size, reserved_size) = LargePage::compute_sizes(object_size);

        if let Some(region) = self.alloc_pages(reserved_size) {
            os::commit_at(region.start(), committed_size, MemoryPermission::ReadWrite);
            let page = LargePage::setup(region.start(), committed_size);
            self.append_page(page);
            self.committed_size += committed_size;
            Some(page.object_address())
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

    fn free_page(&mut self, page: LargePage) {
        let committed_size = page.committed_size();
        assert!(mem::is_os_page_aligned(committed_size));
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

    fn append_page(&mut self, page: LargePage) {
        if let Some(old_head) = self.head {
            old_head.set_prev_page(Some(page));
        }

        page.set_next_page(self.head);
        page.set_prev_page(None);

        self.head = Some(page);
    }

    fn visit_pages<F>(&mut self, mut f: F)
    where
        F: FnMut(LargePage),
    {
        let mut current_page_iter = self.head;

        while current_page_iter.is_some() {
            let page = current_page_iter.expect("missing page");
            f(page);
            current_page_iter = page.next_page();
        }
    }

    fn remove_pages<F>(&mut self, mut f: F)
    where
        F: FnMut(LargePage) -> bool,
    {
        let mut current_page_iter = self.head;
        let mut prev: Option<LargePage> = None;
        let mut freed = false;

        while current_page_iter.is_some() {
            let page = current_page_iter.expect("missing page");
            let next = page.next_page();
            let remove = f(page);

            if remove {
                freed = true;
                self.free_page(page);
            } else {
                if let Some(prev) = prev {
                    prev.set_next_page(Some(page));
                } else {
                    // Our new head.
                    self.head = current_page_iter;
                }

                page.set_prev_page(prev);
                prev = Some(page);
            }

            current_page_iter = next;
        }

        if let Some(prev) = prev {
            prev.set_next_page(None);
        } else {
            // No large objects left.
            self.head = None;
        }

        if freed {
            self.merge_free_regions();
        }
    }
}
