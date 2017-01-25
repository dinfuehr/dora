use std::cmp::max;
use std::ptr;

use gc::chunk::Chunk;
use mem;
use os;

pub struct SpaceConfig {
    pub prot: os::ProtType,
    pub chunk_size: usize,
    pub limit: usize,
    pub align: usize,
}

pub struct Space {
    name: &'static str,
    config: SpaceConfig,
    chunks: Vec<Chunk>,
    size: usize,
}

impl Space {
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

    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let size = mem::align_usize(size, self.config.align);
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
            panic!("{} space full", self.name);
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
