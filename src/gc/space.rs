use std::cmp::max;

use gc::Address;
use gc::chunk::Chunk;
use mem;
use os;

/// Configuration for a space.
/// This makes it possible to use `Space` both for the
/// code space and the permanent space.
pub struct SpaceConfig {
    pub prot: os::ProtType,
    pub chunk_size: usize,
    pub limit: usize,
    pub align: usize,
}

/// Non-contiguous space of memory. Used for permanent space
/// and code space.
pub struct Space {
    name: &'static str,
    config: SpaceConfig,
    chunks: Vec<Chunk>,
    size: usize,
}

impl Space {
    /// initializes `Space` and allocates the first chunk immediately.
    pub fn new(config: SpaceConfig, name: &'static str) -> Space {
        let mut code = Space {
            name: name,
            config: config,
            chunks: Vec::new(),
            size: 0,
        };

        code.add_chunk(0);

        code
    }

    /// allocate memory in this space. This first tries to allocate space
    /// in the current chunk. If this fails a new chunk is allocated.
    /// Doesn't use a freelist right now so memory at the end of a chunk
    /// is probably lost.
    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let size = mem::align_usize(size, self.config.align);
        let mut ptr = self.chunks.last_mut().unwrap().alloc(size);

        if ptr.is_null() {
            self.add_chunk(size);
            ptr = self.chunks.last_mut().unwrap().alloc(size);
        }

        ptr as *mut u8
    }

    /// adds another chunk to the space. Checks if allocation of chunk
    /// would violate the size limit for this space.
    pub fn add_chunk(&mut self, size: usize) {
        let size = max(size, self.config.chunk_size);
        let size = mem::align_usize(size, os::page_size() as usize);

        if self.size + size > self.config.limit {
            panic!("{} space full", self.name);
        }

        let chunk = Chunk::new(size, self.config.prot);
        self.size += chunk.size();

        self.chunks.push(chunk);
    }

    pub fn contains(&self, addr: Address) -> bool {
        for chunk in &self.chunks {
            if chunk.contains(addr) {
                return true;
            }
        }

        false
    }
}
