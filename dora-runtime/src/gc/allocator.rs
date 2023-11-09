use crate::gc::{Address, Region};

pub trait GenerationAllocator {
    fn allocate(&self, size: usize) -> Option<Address>;
    fn free(&self, region: Region);
}
