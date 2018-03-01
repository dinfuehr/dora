use std::ptr;

use gc::Address;
use mem;
use os;

/// Chunk is a contiguous space of memory allocated with `mmap`.
/// Deallocation happens in the destructor with `munmap`.
pub struct Chunk {
    start: Address,
    end: Address,

    next: Address,
}

impl Chunk {
    pub fn new(size: usize, prot: os::ProtType) -> Chunk {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, prot);

        if ptr.is_null() {
            panic!("could not allocate chunk of size {} bytes", size);
        }

        let addr = Address::from_ptr(ptr);

        Chunk {
            start: addr,
            end: addr.offset(size),
            next: addr,
        }
    }

    pub fn alloc(&mut self, size: usize) -> *const u8 {
        let size = mem::align_usize(size, 64);

        if self.end.offset_from(self.next) > size {
            let next = self.next.offset(size);
            let addr = self.next;
            self.next = next;

            addr.to_ptr()
        } else {
            ptr::null()
        }
    }

    pub fn size(&self) -> usize {
        self.end.offset_from(self.start)
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.start <= addr && addr < self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_size() {
        let mut chunk = Chunk {
            start: 10.into(),
            end: 14.into(),
            next: 0.into(),
        };

        assert_eq!(4, chunk.size());

        chunk.end = 20.into();
        assert_eq!(10, chunk.size());

        drop(chunk);
    }

    #[test]
    fn test_chunk_contains() {
        let mut chunk = Chunk {
            start: 10.into(),
            end: 14.into(),
            next: 0.into(),
        };

        assert_eq!(false, chunk.contains(9.into()));
        assert_eq!(true, chunk.contains(10.into()));
        assert_eq!(true, chunk.contains(13.into()));
        assert_eq!(false, chunk.contains(14.into()));
        assert_eq!(false, chunk.contains(15.into()));

        chunk.end = 20.into();
        assert_eq!(true, chunk.contains(19.into()));
        assert_eq!(false, chunk.contains(20.into()));

        drop(chunk);
    }
}
