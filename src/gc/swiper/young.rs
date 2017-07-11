use std::sync::atomic::AtomicUsize;

use ctxt::SemContext;
use gc::Address;
use gc::Collector;

struct YoungGen {
    total: Region,
    from: Chunk,
    to: Chunk,
}

impl YoungGen {
    fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = (young_end.to_usize() - young_start.to_usize()) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            from: Chunk::new(young_start, half_address),
            to: Chunk::new(half_address, young_end),
        }
    }
}

impl Collector for YoungGen {
    fn alloc(&self, _: &SemContext, _: usize) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, _: &SemContext) {
        unimplemented!();
    }
}

struct Region {
    start: Address,
    end: Address,
}

impl Region {
    fn new(start: Address, end: Address) -> Region {
        Region {
            start: start,
            end: end,
        }
    }

    fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }
}

struct Chunk {
    start: Address,
    next: AtomicUsize,
    uncommitted: Address,
    end: Address,
}

impl Chunk {
    fn new(start: Address, end: Address) -> Chunk {
        Chunk {
            start: start,
            next: AtomicUsize::new(start.to_usize()),
            uncommitted: start,
            end: end,
        }
    }

    fn empty() -> Chunk {
        Chunk {
            start: Address::null(),
            next: AtomicUsize::new(0),
            uncommitted: Address::null(),
            end: Address::null(),
        }
    }

    fn committed_size(&self) -> usize {
        self.uncommitted.to_usize() - self.start.to_usize()
    }

    fn uncommitted_size(&self) -> usize {
        self.end.to_usize() - self.uncommitted.to_usize()
    }
}
