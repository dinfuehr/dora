use fixedbitset::FixedBitSet;
use parking_lot::Mutex;
use std::cmp;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::{arena, Address, Region};

// Choose 128K as chunk size for now
const CHUNK_SIZE_BITS: usize = 17;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_BITS;

// experimental implementation of an Old generation consisting of chunks
// goal is to reduce work for full GCs
pub struct OldGen {
    // total size of old generation
    total: Region,

    chunks: Vec<Chunk>,

    prot: Mutex<OldGenProtected>,

    crossing_map: CrossingMap,
    card_table: CardTable,
    config: SharedHeapConfig,
}

impl OldGen {
    pub fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
        config: SharedHeapConfig,
    ) -> OldGen {
        assert!(start.is_chunk_aligned());

        let total = Region::new(start, end);
        let num_chunks = total.size() / CHUNK_SIZE;

        let mut chunks = Vec::with_capacity(num_chunks);

        for i in 0..num_chunks {
            let chunk_start = start.offset(i * CHUNK_SIZE);
            chunks.push(Chunk::new(chunk_start));
        }

        let prot = Mutex::new(OldGenProtected {
            used_chunks: ChunkSet::empty(num_chunks),
            free_chunks: ChunkSet::full(num_chunks),
        });

        OldGen {
            total: total,
            chunks: chunks,
            prot: prot,
            crossing_map: crossing_map,
            card_table: card_table,
            config: config,
        }
    }

    fn add_chunk(&self) -> bool {
        {
            let mut config = self.config.lock();
            if !config.grow_old(CHUNK_SIZE) {
                return false;
            }
        }

        {
            let mut prot = self.prot.lock();

            if let Some(chunk) = prot.free_chunks.remove_leftmost() {
                prot.used_chunks.add(chunk);
                arena::commit(chunk_addr(chunk, self.total.start), CHUNK_SIZE, false);
                return true;
            }
        }

        false
    }

    fn free_chunk(&self, chunk: ChunkId) {
        {
            let mut prot = self.prot.lock();
            assert!(prot.used_chunks.remove(chunk));
            prot.free_chunks.add(chunk);
            arena::discard(chunk_addr(chunk, self.total.start), CHUNK_SIZE);
        }

        {
            let mut config = self.config.lock();
            config.shrink_old(CHUNK_SIZE);
        }
    }
}

fn chunk_addr(chunk: ChunkId, start: Address) -> Address {
    start.offset(chunk.to_usize() * CHUNK_SIZE)
}

struct OldGenProtected {
    // all used chunks
    used_chunks: ChunkSet,

    // completely free chunks (no live objects)
    free_chunks: ChunkSet,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct ChunkId(usize);

impl ChunkId {
    fn to_usize(self) -> usize {
        self.0
    }
}

struct Chunk {
    // chunk boundaries
    region: Region,

    // end of allocated area in chunk
    top: Address,

    // live objects in bytes
    live: usize,

    // state of chunk
    state: ChunkState,
}

impl Chunk {
    fn new(start: Address) -> Chunk {
        Chunk {
            region: start.region_start(CHUNK_SIZE),
            top: start,
            live: 0,
            state: ChunkState::Free,
        }
    }
}

// An array of chunks
struct ChunkSet {
    bounds: Option<ChunkSetBounds>,
    chunks: FixedBitSet,
}

#[derive(Copy, Clone)]
struct ChunkSetBounds {
    leftmost: usize,
    rightmost: usize,
}

impl ChunkSetBounds {
    fn new(leftmost: usize, rightmost: usize) -> ChunkSetBounds {
        ChunkSetBounds {
            leftmost: leftmost,
            rightmost: rightmost,
        }
    }

    fn tuple(&self) -> (usize, usize) {
        (self.leftmost, self.rightmost)
    }
}

impl ChunkSet {
    fn empty(num_chunks: usize) -> ChunkSet {
        assert!(num_chunks > 0);

        ChunkSet {
            bounds: None,
            chunks: FixedBitSet::with_capacity(num_chunks),
        }
    }

    fn full(num_chunks: usize) -> ChunkSet {
        assert!(num_chunks > 0);
        let mut chunks = FixedBitSet::with_capacity(num_chunks);
        chunks.insert_range(..);

        ChunkSet {
            bounds: Some(ChunkSetBounds::new(0, num_chunks - 1)),
            chunks: chunks,
        }
    }

    fn count(&self) -> usize {
        self.chunks.count_ones(..)
    }

    fn add(&mut self, chunk: ChunkId) {
        self.chunks.insert(chunk.to_usize());

        if let Some(bounds) = self.bounds {
            let (leftmost, rightmost) = bounds.tuple();
            let leftmost = cmp::min(leftmost, chunk.to_usize());
            let rightmost = cmp::max(rightmost, chunk.to_usize());
            self.bounds = Some(ChunkSetBounds::new(leftmost, rightmost));
        } else {
            let chunk = chunk.to_usize();
            self.bounds = Some(ChunkSetBounds::new(chunk, chunk));
        }
    }

    fn remove(&mut self, chunk: ChunkId) -> bool {
        let chunk = chunk.to_usize();

        if self.chunks[chunk] {
            self.chunks.set(chunk, false);

            let (leftmost, rightmost) = self.bounds.expect("bounds are empty").tuple();

            if leftmost == rightmost {
                self.bounds = None;
            } else {
                for i in leftmost + 1..rightmost {
                    if self.chunks[i] {
                        self.bounds = Some(ChunkSetBounds::new(i, rightmost));
                        return true;
                    }
                }

                debug_assert!(self.chunks[rightmost]);
                self.bounds = Some(ChunkSetBounds::new(rightmost, rightmost));
            }

            true
        } else {
            false
        }
    }

    fn remove_leftmost(&mut self) -> Option<ChunkId> {
        if let Some(bounds) = self.bounds {
            let leftmost = ChunkId(bounds.leftmost);
            assert!(self.remove(leftmost));
            Some(leftmost)
        } else {
            None
        }
    }
}

enum ChunkState {
    Free,
    Used,
}

impl Address {
    fn is_chunk_aligned(self) -> bool {
        (self.to_usize() & (CHUNK_SIZE - 1)) == 0
    }
}

#[cfg(test)]
mod tests {
    use super::{ChunkId, ChunkSet};

    #[test]
    fn test_empty() {
        let e = ChunkSet::empty(4);
        assert_eq!(e.count(), 0);
    }

    #[test]
    fn test_full() {
        let e = ChunkSet::full(4);
        assert_eq!(e.count(), 4);
    }

    #[test]
    fn test_add_and_remove() {
        let mut e = ChunkSet::empty(4);
        e.add(ChunkId(1));
        e.add(ChunkId(3));
        assert!(e.remove(ChunkId(1)));
        assert_eq!(e.remove_leftmost(), Some(ChunkId(3)));
        assert_eq!(e.count(), 0);
    }
}
