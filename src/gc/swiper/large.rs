use std::mem::size_of;
use std::ptr;
use std::sync::Mutex;

use gc::arena;
use gc::Address;
use gc::swiper::Region;
use gc::swiper::LARGE_OBJECT_SIZE;
use mem;

pub struct LargeSpace {
    total: Region,
    ranges: Mutex<Ranges>,
}

impl LargeSpace {
    pub fn new(start: Address, end: Address) -> LargeSpace {
        LargeSpace {
            total: Region::new(start, end),
            ranges: Mutex::new(Ranges::new(start, end)),
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        let size = mem::page_align(size_of::<LargeAlloc>() + size);

        let mut ranges = self.ranges.lock().unwrap();

        if let Some(range) = ranges.alloc(size) {
            arena::commit(range.start, range.size(), false).expect("couldn't commit large object.");
            range.start.to_ptr()

        } else {
            ptr::null()
        }
    }

    pub fn free(&self, ptr: Address, size: usize) {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        debug_assert!(mem::is_page_aligned(ptr.to_usize()));
        let size = mem::page_align(size_of::<LargeAlloc>() + size);

        let mut ranges = self.ranges.lock().unwrap();
        debug_assert!(!ranges.contains(ptr));
        ranges.free(ptr, size);
        ranges.merge();
    }
}

struct LargeAlloc {
    prev: Address,
    next: Address,
    size: usize,
}

struct Ranges {
    elements: Vec<Range>,
}

impl Ranges {
    fn new(start: Address, end: Address) -> Ranges {
        Ranges {
            elements: vec![Range::new(start, end)],
        }
    }

    fn alloc(&mut self, size: usize) -> Option<Range> {
        debug_assert!(mem::is_page_aligned(size));
        let len = self.elements.len();

        for i in 0..len {
            if self.elements[i].size() >= size {
                let range = self.elements[i];
                let alloc = Range::new(range.start, range.start.offset(size));

                if range.size() == size {
                    self.elements.remove(i);
                } else {
                    self.elements[i] = Range::new(range.start.offset(size), range.end);
                }

                return Some(alloc);
            }
        }

        None
    }

    fn free(&mut self, ptr: Address, size: usize) {
        debug_assert!(mem::is_page_aligned(size));
        self.elements.push(Range::new(ptr, ptr.offset(size)));
    }

    fn merge(&mut self) {
        self.elements.sort_unstable_by(|lhs, rhs| lhs.start.to_usize().cmp(&rhs.start.to_usize()));

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

        self.elements.truncate(last_element+1);
    }

    fn contains(&self, ptr: Address) -> bool {
        for element in &self.elements {
            if element.contains(ptr) {
                return true;
            }
        }

        false
    }
}

#[derive(Copy, Clone)]
struct Range {
    start: Address,
    end: Address,
}

impl Range {
    fn new(start: Address, end: Address) -> Range {
        assert!(end > start);

        Range {
            start: start,
            end: end,
        }
    }

    fn contains(&self, ptr: Address) -> bool {
        self.start <= ptr && ptr < self.end
    }

    fn size(&self) -> usize {
        self.end.offset_from(self.start)
    }
}