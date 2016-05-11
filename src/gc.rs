use libc;

use cpu::get_rootset;
use ctxt::get_ctxt;
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

        self.memory.push(ptr);
        self.allocated += size;

        println!("allocate {} bytes: {:x} (total: {} bytes, {} objects)", size,
            ptr.raw() as usize, self.allocated, self.memory.len());

        let ctxt = get_ctxt();
        println!("rootset = {:?}", get_rootset(ctxt));

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
