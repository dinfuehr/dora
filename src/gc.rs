use libc;
use std::ptr::write_bytes;

use class::Class;
use cpu::get_rootset;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::Obj;

pub struct Gc {
    memory: Vec<Ptr>,
    bytes_allocated: usize,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            memory: Vec::new(),
            bytes_allocated: 0
        }
    }

    pub fn alloc(&mut self, size: usize) -> Ptr {
        let ctxt = get_ctxt();

        // TODO: invoke collect() if it is necessary

        let ptr = unsafe { libc::malloc(size) };
        unsafe { write_bytes(ptr, 0, size); }
        let ptr = Ptr::new(ptr);

        self.memory.push(ptr);
        self.bytes_allocated += size;

        if ctxt.args.flag_gc_dump {
            println!("allocate {} bytes: {:x} (total: {} bytes, {} objects)", size,
                ptr.raw() as usize, self.bytes_allocated, self.memory.len());
        }

        ptr
    }

    pub fn collect(&mut self) {
        let ctxt = get_ctxt();
        let rootset = get_rootset(ctxt);
        // dump_rootset(&rootset);

        for &ptr in &self.memory {
            let obj = unsafe { &mut *(ptr.raw() as *mut Obj) };
            obj.header_mut().unmark();

            println!("unmark {:x}", ptr.raw() as usize);
        }

        mark(&rootset);
        sweep(self, ctxt.args.flag_gc_dump);
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

fn mark(rootset: &Vec<usize>) {
    for &root in rootset {
        mark_recursive(root);
    }
}

fn mark_recursive(ptr: usize) {
    if ptr == 0 { return; }
    let obj = unsafe { &mut *(ptr as *mut Obj) };

    if !obj.header().is_marked() {
        println!("mark {:x}", ptr);

        obj.header_mut().mark();
        let class = obj.header().class();

        for prop in &class.props {
            if prop.ty.reference_type() {
                let addr = ptr as isize + prop.offset as isize;
                let obj = unsafe { *(addr as *const usize) };

                if obj == 0 { return; }
                mark_recursive(obj);
            }
        }
    } else {
        println!("already marked {:x}", ptr);
    }
}

fn sweep(gc: &mut Gc, dump: bool) {
    let mut i = 0;

    while i < gc.memory.len() {
        let ptr = gc.memory[i];
        let obj = unsafe { &mut *(ptr.raw() as *mut Obj) };

        if !obj.header().is_marked() {
            if dump {
                println!("sweep {:x}", ptr.raw() as usize);
            }

            unsafe { libc::free(ptr.raw()) };

            let size = obj.header().class().size as usize;
            gc.bytes_allocated -= size;
            gc.memory.remove(i);
            continue;
        }

        i += 1;
    }
}
