use std::cmp::max;
use std::ptr;

const DEFAULT_CHUNK_SIZE: usize = 8 * 1024;
const DEFAULT_CODE_SPACE_LIMIT: usize = 128 * 1024;

use mem;
use os;

pub struct CodeSpace {
    chunks: Vec<Chunk>,
    size: usize,
}

impl CodeSpace {
    pub fn new() -> CodeSpace {
        let mut code = CodeSpace {
            chunks: Vec::new(),
            size: 0,
        };

        code.add_chunk(DEFAULT_CHUNK_SIZE);

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
        let size = max(size, DEFAULT_CHUNK_SIZE);
        let size = mem::align_usize(size, os::page_size() as usize);

        if self.size + size > DEFAULT_CODE_SPACE_LIMIT {
            panic!("code space full");
        }

        let chunk = Chunk::new(size, os::Executable);
        self.size += chunk.size();

        self.chunks.push(chunk);
    }
}

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
}