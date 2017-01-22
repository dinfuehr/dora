use std::cmp::max;
use std::ptr;

use mem;
use os;

pub struct SpaceConfig {
    pub prot: os::ProtType,
    pub chunk_size: usize,
    pub limit: usize,
}

pub struct Space {
    config: SpaceConfig,
    chunks: Vec<Chunk>,
    size: usize,
}

impl Space {
    pub fn new(config: SpaceConfig) -> Space {
        let mut code = Space {
            config: config,
            chunks: Vec::new(),
            size: 0,
        };

        code.add_chunk(0);

        code
    }

    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let size = mem::align_usize(size, 64);
        let mut ptr = self.chunks.last_mut().unwrap().alloc(size);

        if ptr.is_null() {
            self.add_chunk(size);
            ptr = self.chunks.last_mut().unwrap().alloc(size);
        }

        ptr as *mut u8
    }

    pub fn add_chunk(&mut self, size: usize) {
        let size = max(size, self.config.chunk_size);
        let size = mem::align_usize(size, os::page_size() as usize);

        if self.size + size > self.config.limit {
            panic!("code space full");
        }

        let chunk = Chunk::new(size, self.config.prot);
        self.size += chunk.size();

        self.chunks.push(chunk);
    }

    pub fn includes(&self, ptr: *const u8) -> bool {
        for chunk in &self.chunks {
            if chunk.includes(ptr) {
                return true;
            }
        }

        false
    }
}

/// Chunk is a contiguous space of memory allocated with `mmap`.
/// Deallocation happens in the destructor with `munmap`.
struct Chunk {
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