use parking_lot::Mutex;

use crate::gc::{Address, Region, M, PAGE_SIZE};
use crate::os::{self, MemoryPermission, Reservation};

pub struct MetaSpace {
    total: Region,

    allocate: Mutex<Address>,
    reservation: Reservation,
}

const TOTAL_SIZE: usize = 4 * M;

impl MetaSpace {
    pub fn new() -> MetaSpace {
        let reservation = os::reserve_align(TOTAL_SIZE, PAGE_SIZE, false);
        let space_start = reservation.start();
        let space_end = space_start.offset(TOTAL_SIZE);

        os::commit_at(space_start, TOTAL_SIZE, MemoryPermission::ReadWrite);

        MetaSpace {
            total: Region::new(space_start, space_end),

            allocate: Mutex::new(space_start),
            reservation,
        }
    }

    pub fn start(&self) -> Address {
        self.total.start()
    }

    pub fn size(&self) -> usize {
        let top = self.allocate.lock();
        top.offset_from(self.start())
    }

    pub fn alloc(&self, size: usize, align: usize) -> Address {
        let mut top = self.allocate.lock();

        let rest = top.to_usize() % align;

        if rest != 0 {
            *top = top.offset(align - rest);
        }

        assert_eq!(top.to_usize() % align, 0);

        if top.offset(size) <= self.total.end() {
            let result = *top;
            *top = top.offset(size);
            result
        } else {
            panic!("OOM in meta space.");
        }
    }
}
