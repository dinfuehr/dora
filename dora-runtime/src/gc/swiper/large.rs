use parking_lot::Mutex;

use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::LargePage;
use crate::gc::Address;

use super::heap::Heap;

pub struct LargeSpace {
    space: Mutex<LargeSpaceProtected>,
    config: SharedHeapConfig,
}

impl LargeSpace {
    pub fn new(config: SharedHeapConfig) -> LargeSpace {
        LargeSpace {
            space: Mutex::new(LargeSpaceProtected { head: None }),
            config,
        }
    }

    pub fn alloc(&self, heap: &Heap, object_size: usize) -> Option<Address> {
        if let Some(page) = heap.alloc_large_page(object_size) {
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

    pub fn remove_pages<F>(&self, heap: &Heap, merge_if_necessary: bool, f: F)
    where
        F: FnMut(LargePage) -> bool,
    {
        let mut space = self.space.lock();
        space.remove_pages(heap, merge_if_necessary, f);
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

    fn remove_pages<F>(&mut self, heap: &Heap, merge_if_necessary: bool, mut f: F)
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
                heap.free_large_page(page);
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

        if freed && merge_if_necessary {
            heap.merge_free_regions();
        }
    }
}
