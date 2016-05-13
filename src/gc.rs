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
        let dump = ctxt.args.flag_gc_dump;

        for &ptr in &self.memory {
            let obj = unsafe { &mut *(ptr.raw() as *mut Obj) };
            obj.header_mut().unmark();

            if ctxt.args.flag_gc_dump {
                println!("unmark {:x}", ptr.raw() as usize);
            }
        }

        mark_literals(dump);
        mark_rootset(&rootset, dump);
        sweep(self, dump);
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

fn mark_literals(dump: bool) {
    let ctxt = get_ctxt();
    let literals = ctxt.literals.lock().unwrap();

    for lit in literals.iter() {
        mark_recursive(lit.raw() as usize, dump);
    }
}

fn mark_rootset(rootset: &Vec<usize>, dump: bool) {
    for &root in rootset {
        mark_recursive(root, dump);
    }
}

fn mark_recursive(ptr: usize, dump: bool) {
    if ptr == 0 { return; }
    let obj = unsafe { &mut *(ptr as *mut Obj) };

    if !obj.header().is_marked() {
        if dump { println!("mark {:x}", ptr); }

        obj.header_mut().mark();
        let class = obj.header().class();

        for prop in &class.props {
            if prop.ty.reference_type() {
                let addr = ptr as isize + prop.offset as isize;
                let obj = unsafe { *(addr as *const usize) };

                if obj == 0 { return; }
                mark_recursive(obj, dump);
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

            let size = obj.size();

            unsafe {
                // TODO: make me optional
                write_bytes(ptr.raw() as *mut u8, 0xcc, size);
                libc::free(ptr.raw())
            };

            gc.bytes_allocated -= size;
            gc.memory.remove(i);
            continue;
        }

        i += 1;
    }
}
