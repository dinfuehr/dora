use parking_lot::Mutex;
use fixedbitset::FixedBitSet;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::{arena, Address, Region};

// experimental implementation of an Old generation consisting of chunks
// goal is to reduce work for full GCs
struct SplitOldGen {
    // total size of old generation
    total: Region,

    chunks: Vec<Chunk>,

    prot: Mutex<SplitOldGenProtected>,

    crossing_map: CrossingMap,
    card_table: CardTable,
    config: SharedHeapConfig,
}

impl SplitOldGen {
    fn new(
        start: Address,
        end: Address,
        crossing_map: CrossingMap,
        card_table: CardTable,
        config: SharedHeapConfig,
    ) -> SplitOldGen {
        assert!(start.is_chunk_aligned());

        let total = Region::new(start, end);
        let num_chunks = total.size() / CHUNK_SIZE;

        let mut chunks = Vec::with_capacity(num_chunks);

        for i in 0..num_chunks {
            let chunk_start = start.offset(i * CHUNK_SIZE);
            chunks.push(Chunk::new(chunk_start));
        }

        let prot = Mutex::new(SplitOldGenProtected {
            used_chunks: ChunkSet::empty(num_chunks),
            free_chunks: ChunkSet::full(num_chunks),
        });

        SplitOldGen {
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

        let mut prot = self.prot.lock();

        if let Some(chunk) = prot.free_chunks.pick_leftmost() {
            prot.used_chunks.add(chunk);
            arena::commit(chunk_addr(chunk, self.total.start), CHUNK_SIZE, false);
            return true;
        }

        false
    }
}

fn chunk_addr(chunk: ChunkId, start: Address) -> Address {
    start.offset(chunk.to_usize() * CHUNK_SIZE)
}

struct SplitOldGenProtected {
    // all used chunks
    used_chunks: ChunkSet,

    // completely free chunks (no live objects)
    free_chunks: ChunkSet,
}

// Choose 128K as chunk size for now
const CHUNK_SIZE_BITS: usize = 17;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_BITS;

#[derive(Copy, Clone)]
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
    leftmost: usize,
    rightmost: usize,
    chunks: FixedBitSet,
}

impl ChunkSet {
    fn empty(num_chunks: usize) -> ChunkSet {
        assert!(num_chunks > 0);

        ChunkSet {
            leftmost: usize::max_value(),
            rightmost: usize::max_value(),
            chunks: FixedBitSet::with_capacity(num_chunks),
        }
    }

    fn full(num_chunks: usize) -> ChunkSet {
        assert!(num_chunks > 0);
        let mut chunks = FixedBitSet::with_capacity(num_chunks);
        chunks.insert_range(..);

        ChunkSet {
            leftmost: 0,
            rightmost: num_chunks - 1,
            chunks: chunks,
        }
    }

    fn add(&mut self, chunk: ChunkId) {
        self.chunks.insert(chunk.to_usize());
    }

    fn pick_leftmost(&mut self) -> Option<ChunkId> {
        if self.leftmost == usize::max_value() {
            return None;
        }

        let chunk = ChunkId(self.leftmost);
        debug_assert!(self.chunks[self.leftmost]);
        self.chunks.set(self.leftmost, false);

        if self.leftmost == self.rightmost {
            self.leftmost = usize::max_value();
            self.rightmost = usize::max_value();
        } else {
            for i in self.leftmost+1..self.rightmost {
                if self.chunks[i] {
                    self.leftmost = i;
                    return Some(chunk);
                }
            }

            debug_assert!(self.chunks[self.rightmost]);
            self.leftmost = self.rightmost;
        }

        Some(chunk)
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
