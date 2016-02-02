use libc;

use mem::ptr::Ptr;

pub struct Gc {
    memory: Vec<Ptr>,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            memory: Vec::new()
        }
    }

    pub fn alloc(&mut self, size: usize) -> Ptr {
        let ptr = unsafe { libc::malloc(size) };
        let ptr = Ptr::new(ptr);

        self.memory.push(ptr);

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
