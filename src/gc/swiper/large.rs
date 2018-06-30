use std::mem::size_of;
use std::ptr;
use std::sync::Mutex;

use gc::arena;
use gc::Address;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::LARGE_OBJECT_SIZE;
use gc::swiper::Region;
use mem;
use object::Obj;

pub struct LargeSpace {
    total: Region,
    space: Mutex<LargeSpaceProtected>,
    crossing_map: CrossingMap,
    card_table: CardTable,
}

impl LargeSpace {
    pub fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
    ) -> LargeSpace {
        LargeSpace {
            total: Region::new(start, end),
            space: Mutex::new(LargeSpaceProtected::new(start, end)),
            crossing_map: crossing_map,
            card_table: card_table,
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        let loh_size = size_of::<LargeAlloc>();
        let size = mem::page_align(loh_size + size);
        let mut space = self.space.lock().unwrap();

        if let Some(range) = space.alloc_large_object(size) {
            arena::commit(range.start, range.size(), false);
            space.append_large_alloc(range.start, range.size());

            let object_start = range.start.offset(loh_size);
            object_start.to_ptr()
        } else {
            ptr::null()
        }
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn contains_object(&self, addr: Address) -> bool {
        let space = self.space.lock().unwrap();
        let mut current = space.head;

        while !current.is_null() {
            let loh = unsafe { &mut *current.to_mut_ptr::<LargeAlloc>() };
            let obj_address = current.offset(size_of::<LargeAlloc>());

            if addr == obj_address {
                return true;
            }

            current = loh.next;
        }

        false
    }

    pub fn visit<F>(&self, mut f: F)
    where
        F: FnMut(&mut Obj) -> bool,
    {
        let mut space = self.space.lock().unwrap();

        let mut current = space.head;

        while !current.is_null() {
            let loh = unsafe { &mut *current.to_mut_ptr::<LargeAlloc>() };
            let obj_address = current.offset(size_of::<LargeAlloc>());
            let obj = unsafe { &mut *obj_address.to_mut_ptr::<Obj>() };

            let keep = f(obj);

            if keep {
                current = loh.next;
            } else {
                if loh.prev.is_null() {
                    space.head = loh.next;
                } else {
                    let pred = unsafe { &mut *loh.prev.to_mut_ptr::<LargeAlloc>() };
                    pred.next = loh.next;
                }

                if loh.next.is_null() {
                    space.tail = loh.prev;
                } else {
                    let succ = unsafe { &mut *loh.next.to_mut_ptr::<LargeAlloc>() };
                    succ.next = loh.prev;
                }

                let next = loh.next;
                space.free_large_object(current, loh.size);
                current = next;
            }
        }
    }
}

struct LargeAlloc {
    prev: Address,
    next: Address,
    size: usize,
}

struct LargeSpaceProtected {
    elements: Vec<Range>,
    head: Address,
    tail: Address,
}

impl LargeSpaceProtected {
    fn new(start: Address, end: Address) -> LargeSpaceProtected {
        LargeSpaceProtected {
            elements: vec![Range::new(start, end)],
            head: Address::null(),
            tail: Address::null(),
        }
    }

    fn alloc_large_object(&mut self, size: usize) -> Option<Range> {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
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

    fn free_large_object(&mut self, ptr: Address, size: usize) {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        debug_assert!(mem::is_page_aligned(ptr.to_usize()));

        // forget memoy content but keep memor address space
        // reserved
        arena::forget(ptr, size);

        self.free(ptr, size);
        self.merge();
    }

    fn free(&mut self, ptr: Address, size: usize) {
        debug_assert!(mem::is_page_aligned(size));
        self.elements.push(Range::new(ptr, ptr.offset(size)));
    }

    fn merge(&mut self) {
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

    fn contains(&self, ptr: Address) -> bool {
        for element in &self.elements {
            if element.contains(ptr) {
                return true;
            }
        }

        false
    }

    fn append_large_alloc(&mut self, addr: Address, size: usize) {
        if !self.tail.is_null() {
            let old_tail = unsafe { &mut *self.tail.to_mut_ptr::<LargeAlloc>() };
            old_tail.next = addr;
        } else if self.head.is_null() {
            self.head = addr;
        }

        let new_tail = unsafe { &mut *addr.to_mut_ptr::<LargeAlloc>() };
        new_tail.prev = self.tail;
        new_tail.next = Address::null();
        new_tail.size = size;

        self.tail = addr;
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
