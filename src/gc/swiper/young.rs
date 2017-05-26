use std::ptr;

struct YoungGen {
    total: Region,
    from: Chunk,
    to: Chunk,
}

impl YoungGen {
    fn new(young_start: *const u8, young_end: *const u8) -> YoungGen {
        let half_size = (young_end as usize - young_start as usize) / 2;
        let half_address = unsafe { young_start.offset(half_size as isize) };

        YoungGen {
            total: Region::new(young_start, young_end),
            from: Chunk::new(young_start, half_address),
            to: Chunk::new(half_address,  young_end),
        }
    }
}

struct Region {
    start: *const u8,
    end: *const u8,
}

impl Region {
    fn new(start: *const u8, end: *const u8) -> Region {
        Region {
            start: start,
            end: end,
        }
    }

    fn size(&self) -> usize {
        self.end as usize - self.start as usize
    }
}

struct Chunk {
    start: *const u8,
    uncommitted: *const u8,
    end: *const u8,
}

impl Chunk {
    fn new(start: *const u8, end: *const u8) -> Chunk {
        Chunk {
            start: start,
            uncommitted: start,
            end: end,
        }
    }

    fn empty() -> Chunk {
        Chunk {
            start: ptr::null(),
            uncommitted: ptr::null(),
            end: ptr::null(),
        }
    }

    fn committed_size(&self) -> usize {
        self.uncommitted as usize - self.start as usize
    }

    fn uncommitted_size(&self) -> usize {
        self.end as usize - self.uncommitted as usize
    }
}