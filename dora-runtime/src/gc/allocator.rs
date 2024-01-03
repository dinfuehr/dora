use crate::gc::{Address, Region};
use crate::vm::VM;

pub trait GenerationAllocator {
    fn allocate(&self, vm: &VM, size: usize) -> Option<Address>;
    fn free(&self, region: Region);
}
