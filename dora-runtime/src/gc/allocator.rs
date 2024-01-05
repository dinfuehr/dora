use crate::gc::{Address, Region};
use crate::vm::VM;

pub trait GenerationAllocator {
    fn allocate(&self, vm: &VM, min_size: usize, max_size: usize) -> Option<Address>;
    fn free(&self, region: Region);
}
