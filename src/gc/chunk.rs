use std::ptr;

use mem;
use os;

/// Chunk is a contiguous space of memory allocated with `mmap`.
/// Deallocation happens in the destructor with `munmap`.
pub struct Chunk {
    start: *const u8,
    end: *const u8,

    next: *const u8,
}

impl Chunk {
    pub fn new(size: usize, prot: os::ProtType) -> Chunk {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, prot);

        if ptr.is_null() {
            panic!("could not allocate chunk of size {} bytes", size);
        }

        Chunk {
            start: ptr,
            end: unsafe { ptr.offset(size as isize) },
            next: ptr,
        }
    }

    pub fn alloc(&mut self, size: usize) -> *const u8 {
        let size = mem::align_usize(size, 64);

        if self.end as usize - self.next as usize > size {
            let next = unsafe { self.next.offset(size as isize) };
            let addr = self.next;
            self.next = next;

            addr
        } else {
            ptr::null()
        }
    }

    pub fn size(&self) -> usize {
        self.end as usize - self.start as usize
    }

    pub fn includes(&self, ptr: *const u8) -> bool {
        self.start <= ptr && ptr < self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_size() {
        let mut chunk = Chunk {
            start: 10 as *const u8,
            end: 14 as *const u8,
            next: 0 as *const u8,
        };

        assert_eq!(4, chunk.size());

        chunk.end = 20 as *const u8;
        assert_eq!(10, chunk.size());

        drop(chunk);
    }

    #[test]
    fn test_chunk_includes() {
        let mut chunk = Chunk {
            start: 10 as *const u8,
            end: 14 as *const u8,
            next: 0 as *const u8,
        };

        assert_eq!(false, chunk.includes(9 as *const u8));
        assert_eq!(true, chunk.includes(10 as *const u8));
        assert_eq!(true, chunk.includes(13 as *const u8));
        assert_eq!(false, chunk.includes(14 as *const u8));
        assert_eq!(false, chunk.includes(15 as *const u8));

        chunk.end = 20 as *const u8;
        assert_eq!(true, chunk.includes(19 as *const u8));
        assert_eq!(false, chunk.includes(20 as *const u8));

        drop(chunk);
    }
}
