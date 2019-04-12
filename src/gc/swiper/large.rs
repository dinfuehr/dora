use parking_lot::Mutex;
use std::mem::size_of;

use gc::arena;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::LARGE_OBJECT_SIZE;
use gc::{Address, Region};
use mem;

pub struct LargeSpace {
    total: Region,
    space: Mutex<LargeSpaceProtected>,
    config: SharedHeapConfig,
}

impl LargeSpace {
    pub fn new(start: Address, end: Address, config: SharedHeapConfig) -> LargeSpace {
        LargeSpace {
            total: Region::new(start, end),
            space: Mutex::new(LargeSpaceProtected::new(start, end)),
            config: config,
        }
    }

    pub fn alloc(&self, size: usize) -> Address {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        let size = mem::page_align(size_of::<LargeAlloc>() + size);

        let mut space = self.space.lock();
        let mut config = self.config.lock();

        if !config.grow_old(size) {
            return Address::null();
        }

        space.alloc(size)
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn head(&self) -> Address {
        let space = self.space.lock();
        space.head
    }

    pub fn remove_head(&self) -> Address {
        let mut space = self.space.lock();
        let head = space.head;
        space.head = Address::null();
        head
    }

    pub fn append_chain(
        &self,
        head: Address,
        tail: Address,
        freed: usize,
        mut free_regions: Vec<Region>,
    ) {
        let mut space = self.space.lock();

        let old_head = space.head;
        space.head = head;

        if old_head.is_non_null() {
            let old_head = LargeAlloc::from_address(old_head);
            old_head.prev = tail;

            let tail = LargeAlloc::from_address(tail);
            tail.next = old_head.address();
        }

        space.committed_size -= freed;
        space.elements.append(&mut free_regions);
        space.merge();
    }

    pub fn visit_objects<F>(&self, f: F)
    where
        F: FnMut(Address),
    {
        let mut space = self.space.lock();
        space.visit_objects(f);
    }

    pub fn remove_objects<F>(&self, f: F)
    where
        F: FnMut(Address) -> bool,
    {
        let mut space = self.space.lock();
        space.remove_objects(f);
    }

    pub fn committed_size(&self) -> usize {
        let space = self.space.lock();
        space.committed_size()
    }
}

pub struct LargeAlloc {
    pub prev: Address,
    pub next: Address,
    pub size: usize,
}

impl LargeAlloc {
    #[inline(always)]
    pub fn from_address(addr: Address) -> &'static mut LargeAlloc {
        unsafe { &mut *addr.to_mut_ptr::<LargeAlloc>() }
    }

    #[inline(always)]
    pub fn address(&self) -> Address {
        Address::from_ptr(self as *const _)
    }

    #[inline(always)]
    pub fn object_address(&self) -> Address {
        let addr = Address::from_ptr(self as *const _);
        addr.offset(size_of::<LargeAlloc>())
    }
}

struct LargeSpaceProtected {
    elements: Vec<Region>,
    head: Address,
    committed_size: usize,
}

impl LargeSpaceProtected {
    fn new(start: Address, end: Address) -> LargeSpaceProtected {
        LargeSpaceProtected {
            elements: vec![Region::new(start, end)],
            head: Address::null(),
            committed_size: 0,
        }
    }

    fn committed_size(&self) -> usize {
        self.committed_size
    }

    fn alloc(&mut self, size: usize) -> Address {
        debug_assert!(mem::is_page_aligned(size));
        let len = self.elements.len();

        for i in 0..len {
            if self.elements[i].size() >= size {
                let range = self.elements[i];
                let addr = range.start;

                if range.size() == size {
                    self.elements.remove(i);
                } else {
                    self.elements[i] = Region::new(range.start.offset(size), range.end);
                }

                arena::commit(addr, size, false);
                self.append_large_alloc(addr, size);
                self.committed_size += size;

                return addr.offset(size_of::<LargeAlloc>());
            }
        }

        Address::null()
    }

    fn free(&mut self, ptr: Address, size: usize) {
        debug_assert!(mem::is_page_aligned(size));
        arena::forget(ptr, size);
        self.elements.push(ptr.region_start(size));
        self.committed_size -= size;
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
        if !self.head.is_null() {
            let old_head = LargeAlloc::from_address(self.head);
            old_head.prev = addr;
        }

        let new_head = LargeAlloc::from_address(addr);
        new_head.next = self.head;
        new_head.prev = Address::null();
        new_head.size = size;

        self.head = addr;
    }

    fn visit_objects<F>(&mut self, mut f: F)
    where
        F: FnMut(Address),
    {
        let mut addr = self.head;

        while !addr.is_null() {
            let large_alloc = LargeAlloc::from_address(addr);
            f(large_alloc.object_address());
            addr = large_alloc.next;
        }
    }

    fn remove_objects<F>(&mut self, mut f: F)
    where
        F: FnMut(Address) -> bool,
    {
        let mut addr = self.head;
        let mut prev = Address::null();
        let mut freed = false;

        while !addr.is_null() {
            let large_alloc = LargeAlloc::from_address(addr);
            let next = large_alloc.next;
            let remove = f(large_alloc.object_address());

            if remove {
                freed = true;
                let size = large_alloc.size;
                self.free(addr, size);
            } else {
                if prev.is_null() {
                    // Our new head
                    self.head = addr;
                } else {
                    // Change predecessor
                    let prev_large_alloc = LargeAlloc::from_address(prev);
                    prev_large_alloc.next = addr;
                }

                large_alloc.prev = prev;
                prev = addr;
            }

            addr = next;
        }

        if prev.is_null() {
            // No large objects left
            self.head = Address::null();
        } else {
            // Set next to null for last allocation
            let prev_large_alloc = LargeAlloc::from_address(prev);
            prev_large_alloc.next = Address::null();
        }

        if freed {
            self.merge();
        }
    }
}
