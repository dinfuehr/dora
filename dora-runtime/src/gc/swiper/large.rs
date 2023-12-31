use parking_lot::Mutex;
use std::mem::size_of;

use crate::gc::swiper::controller::SharedHeapConfig;
use crate::gc::swiper::LARGE_OBJECT_SIZE;
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

    pub fn alloc(&self, size: usize) -> Option<Address> {
        debug_assert!(size >= LARGE_OBJECT_SIZE);
        let committed_size = mem::os_page_align_up(size_of::<LargeAlloc>() + size);

        let mut space = self.space.lock();
        let mut config = self.config.lock();

        if !config.grow_old(committed_size) {
            return None;
        }

        space.alloc(committed_size)
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

    fn alloc(&mut self, committed_size: usize) -> Option<Address> {
        assert!(mem::is_os_page_aligned(committed_size));
        let reserved_size = align_page_up(committed_size);

        if let Some(address) = self.alloc_pages(reserved_size) {
            assert!(address.is_page_aligned());
            os::commit_at(address, committed_size, MemoryPermission::ReadWrite);
            self.append_large_alloc(address, committed_size);
            self.committed_size += committed_size;
            Some(address.offset(size_of::<LargeAlloc>()))
        } else {
            None
        }
    }

    fn alloc_pages(&mut self, size: usize) -> Option<Address> {
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

            return Some(addr);
        }

        None
    }

    fn free(&mut self, ptr: Address, committed_size: usize) {
        assert!(mem::is_os_page_aligned(committed_size));
        let reserved_size = align_page_up(committed_size);
        os::discard(ptr, committed_size);
        self.elements.push(ptr.region_start(reserved_size));
        self.committed_size -= committed_size;
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
