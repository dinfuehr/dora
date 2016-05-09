use libc;

use mem::ptr::Ptr;

pub struct Gc {
    memory: Vec<Ptr>,
    allocated: usize,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            memory: Vec::new(),
            allocated: 0
        }
    }

    pub fn alloc(&mut self, size: usize) -> Ptr {
        let ptr = unsafe { libc::malloc(size) };
        let ptr = Ptr::new(ptr);

        println!("allocated = {} bytes ({} objects)", self.allocated, self.memory.len());

        self.memory.push(ptr);
        self.allocated += size;

        ptr
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        for mem in &self.memory {
            unsafe {
                libc::free(mem.raw());
            }
        }
    }
}
