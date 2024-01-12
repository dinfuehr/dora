use parking_lot::Mutex;

use crate::gc::swiper::{RegularPage, LARGE_OBJECT_SIZE};
use crate::gc::{fill_region_with, is_page_aligned, Address, Region, PAGE_SIZE};
use crate::os::{self, MemoryPermission, Reservation};
use crate::vm::VM;

pub struct ReadOnlySpace {
    total: Region,

    allocate: Mutex<ReadOnlySpaceProtected>,
    reservation: Reservation,
}

impl ReadOnlySpace {
    pub fn new(maximum_size: usize) -> ReadOnlySpace {
        assert!(is_page_aligned(maximum_size));

        let reservation = os::reserve_align(maximum_size, PAGE_SIZE, false);
        let space_start = reservation.start();
        let space_end = space_start.offset(maximum_size);

        ReadOnlySpace {
            total: Region::new(space_start, space_end),

            allocate: Mutex::new(ReadOnlySpaceProtected {
                top: space_start,
                current_limit: space_start,
                limit: space_end,
            }),
            reservation,
        }
    }

    pub fn alloc(&self, vm: &VM, size: usize) -> Option<Address> {
        let mut protected = self.allocate.lock();
        protected.alloc(vm, size)
    }

    pub fn pages(&self) -> Vec<RegularPage> {
        let protected = self.allocate.lock();
        let mut pages = Vec::new();

        let mut curr = self.total.start();
        while curr < protected.current_limit {
            let page = RegularPage::from_address(curr);
            pages.push(page);
            curr = page.end();
        }
        assert_eq!(curr, protected.current_limit);

        pages
    }
}

struct ReadOnlySpaceProtected {
    top: Address,
    current_limit: Address,
    limit: Address,
}

impl ReadOnlySpaceProtected {
    fn alloc(&mut self, vm: &VM, size: usize) -> Option<Address> {
        assert!(size < LARGE_OBJECT_SIZE);

        if let Some(address) = self.raw_alloc(vm, size) {
            return Some(address);
        }

        self.allocate_page(vm);
        self.raw_alloc(vm, size)
    }

    fn raw_alloc(&mut self, vm: &VM, size: usize) -> Option<Address> {
        if self.top.offset(size) <= self.current_limit {
            let alloc_start = self.top;
            let alloc_end = alloc_start.offset(size);
            self.top = alloc_end;
            fill_region_with(vm, self.top, self.current_limit, false);
            Some(alloc_start)
        } else {
            None
        }
    }

    fn allocate_page(&mut self, vm: &VM) {
        if self.current_limit < self.limit {
            fill_region_with(vm, self.top, self.current_limit, false);
            let page_start = self.current_limit;
            os::commit_at(page_start, PAGE_SIZE, MemoryPermission::ReadWrite);
            let page = RegularPage::setup(page_start, false, true);
            self.top = page.object_area_start();
            self.current_limit = page.object_area_end();
            assert!(self.current_limit <= self.limit);
        }
    }
}
