use fixedbitset::FixedBitSet;

use gc::swiper::card::CardTable;
use gc::swiper::controller::SharedHeapConfig;
use gc::swiper::crossing::CrossingMap;
use gc::{Address, Region};

// experimental implementation of an Old generation consisting of chunks
// goal is to reduce work for full GCs
struct SplitOldGen {
    // total size of old generation
    total: Region,

    chunks: Vec<Chunk>,

    // all used chunks
    used_chunks: ChunkSet,

    // completely free chunks (no live objects)
    free_chunks: ChunkSet,

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

        SplitOldGen {
            total: total,
            chunks: chunks,
            used_chunks: ChunkSet::empty(num_chunks),
            free_chunks: ChunkSet::full(num_chunks),
            crossing_map: crossing_map,
            card_table: card_table,
            config: config,
        }
    }
}

// Choose 128K as chunk size for now
const CHUNK_SIZE_BITS: usize = 17;
const CHUNK_SIZE: usize = 1 << CHUNK_SIZE_BITS;

struct ChunkId(usize);

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
    fn empty(chunks: usize) -> ChunkSet {
        assert!(chunks > 0);

        ChunkSet {
            leftmost: usize::max_value(),
            rightmost: usize::max_value(),
            chunks: FixedBitSet::with_capacity(chunks),
        }
    }

    fn full(chunks: usize) -> ChunkSet {
        assert!(chunks > 0);

        ChunkSet {
            leftmost: 0,
            rightmost: chunks - 1,
            chunks: FixedBitSet::with_capacity(chunks),
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
