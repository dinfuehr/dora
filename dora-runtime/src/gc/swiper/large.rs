use parking_lot::Mutex;

use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::LargePage;
use crate::gc::{is_page_aligned, Address, Region};

use super::heap::MixedHeap;

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
            space: Mutex::new(LargeSpaceProtected { head: None }),
            config,
        }
    }

    pub fn alloc(&self, mixed_heap: &MixedHeap, object_size: usize) -> Option<Address> {
        if let Some(page) = mixed_heap.alloc_large_page(object_size) {
            self.space.lock().append_page(page);
            Some(page.object_address())
        } else {
            None
        }
    }

    pub fn iterate_pages<F>(&self, f: F)
    where
        F: FnMut(LargePage),
    {
        let mut space = self.space.lock();
        space.visit_pages(f);
    }

    pub fn remove_pages<F>(&self, mixed_heap: &MixedHeap, f: F)
    where
        F: FnMut(LargePage) -> bool,
    {
        let mut space = self.space.lock();
        space.remove_pages(mixed_heap, f);
    }
}

struct LargeSpaceProtected {
    head: Option<LargePage>,
}

impl LargeSpaceProtected {
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

    fn remove_pages<F>(&mut self, mixed_heap: &MixedHeap, mut f: F)
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
                mixed_heap.free_large_page(page);
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
            mixed_heap.merge_free_regions();
        }
    }
}
