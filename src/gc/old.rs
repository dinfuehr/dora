use gc::chunk::Chunk;

pub struct OldSpace {
    chunks: Vec<Chunk>,
}