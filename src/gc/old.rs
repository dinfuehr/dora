use gc::chunk::Chunk;
use gc::CHUNK_SIZE;
use os::Writable;

pub struct OldSpace {
    chunks: Vec<Chunk>,
}

impl OldSpace {
    pub fn new() -> OldSpace {
        OldSpace { chunks: vec![Chunk::new(CHUNK_SIZE, Writable)] }
    }
}
