use crate::gc::Region;
use crate::runtime::Runtime;

pub trait GenerationAllocator {
    fn allocate(&self, rt: &Runtime, min_size: usize, max_size: usize) -> Option<Region>;
    fn free(&self, region: Region);
}
