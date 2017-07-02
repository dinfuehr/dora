const CARD_SIZE: usize = 128;
const CARD_SIZE_BITS: usize = 7;

pub struct CardTable {
    tbl: Vec<u8>,
}

impl CardTable {
    pub fn new(heap_size: usize) -> CardTable {
        CardTable { tbl: vec![0; heap_size / CARD_SIZE] }
    }
}
