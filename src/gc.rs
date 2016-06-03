use libc;
use std::ptr::write_bytes;

use class::Class;
use cpu::get_rootset;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::Obj;

const INITIAL_THRESHOLD: usize = 128;
const USED_RATIO: f64 = 0.75;

pub struct Gc {
    memory: Vec<Ptr>,
    bytes_allocated: usize,
    threshold: usize,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            memory: Vec::new(),
            bytes_allocated: 0,
            threshold: INITIAL_THRESHOLD,
        }
    }

    pub fn alloc(&mut self, size: usize) -> Ptr {
        let ctxt = get_ctxt();

        if ctxt.args.flag_gc_stress {
            // with --gc-stress collect garbage at every allocation
            // useful for testing
            self.collect();

        // do we pass threshold with this allocation?
        } else if self.bytes_allocated + size > self.threshold {
            // collect garbage
            self.collect();

            // if still more memory than USED_RATIO % of the threshold,
            // we need to increase the threshold
            if (self.bytes_allocated + size) as f64 > self.threshold as f64 * USED_RATIO {
                let saved_threshold = self.threshold;
                self.threshold = (self.threshold as f64 / USED_RATIO) as usize;

                if ctxt.args.flag_gc_dump {
                    println!("GC: increase threshold from {} to {}",
                             saved_threshold, self.threshold);
                }
            }
        }

        let ptr = unsafe { libc::malloc(size) };
        unsafe { write_bytes(ptr, 0, size); }
        let ptr = Ptr::new(ptr);

        self.memory.push(ptr);
        self.bytes_allocated += size;

        if ctxt.args.flag_gc_dump {
            println!("GC: allocate {} bytes: {:x} (total {} bytes, threshold {}, {} objects)",
                     size, ptr.raw() as usize,
                     self.bytes_allocated, self.threshold, self.memory.len());
        }

        ptr
    }

    pub fn collect(&mut self) {
        let ctxt = get_ctxt();
        let rootset = get_rootset(ctxt);

        if ctxt.args.flag_gc_dump {
            println!("GC: collect garbage");
        }

        for &ptr in &self.memory {
            let obj = unsafe { &mut *(ptr.raw() as *mut Obj) };
            obj.header_mut().unmark();
        }

        mark_literals();
        mark_rootset(&rootset);
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

fn mark_literals() {
    let ctxt = get_ctxt();
    let literals = ctxt.literals.lock().unwrap();

    for lit in literals.iter() {
        mark_recursive(lit.raw() as usize);
    }
}

fn mark_rootset(rootset: &Vec<usize>) {
    for &root in rootset {
        mark_recursive(root);
    }
}

fn mark_recursive(ptr: usize) {
    if ptr == 0 { return; }
    let obj = unsafe { &mut *(ptr as *mut Obj) };

    if !obj.header().is_marked() {
        obj.header_mut().mark();
        let class = obj.header().class();

        for field in &class.fields {
            if field.ty.reference_type() {
                let addr = ptr as isize + field.offset as isize;
                let obj = unsafe { *(addr as *const usize) };

                if obj == 0 { return; }
                mark_recursive(obj);
            }
        }
    }
}

fn sweep(gc: &mut Gc, dump: bool) {
    let mut i = 0;

    while i < gc.memory.len() {
        let ptr = gc.memory[i];
        let obj = unsafe { &mut *(ptr.raw() as *mut Obj) };

        if !obj.header().is_marked() {
            let size = obj.size();

            unsafe {
                // TODO: make overwriting memory optional
                write_bytes(ptr.raw() as *mut u8, 0xcc, size);

                libc::free(ptr.raw())
            };

            gc.bytes_allocated -= size;
            gc.memory.remove(i);

            if dump {
                println!("sweep {:x} with {} bytes", ptr.raw() as usize, size);
            }

            continue;
        }

        i += 1;
    }
}
