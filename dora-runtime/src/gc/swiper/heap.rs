use crate::gc::Region;

pub struct MixedHeap {
    total: Region,
}

impl MixedHeap {
    pub fn new(total: Region) -> MixedHeap {
        MixedHeap { total }
    }
}
