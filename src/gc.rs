use libc;

use cpu::get_rootset;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::Header;

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
        let ctxt = get_ctxt();

        if ctxt.args.flag_gc_extreme {
            let rootset = get_rootset(ctxt);
            // dump_rootset(&rootset);

            for &ptr in &self.memory {
                let obj = unsafe { &mut *(ptr.raw() as *mut Header) };
                obj.unmark();

                println!("unmark {:x}", ptr.raw() as usize);
            }

            mark(&rootset);
            sweep(&mut self.memory, ctxt.args.flag_gc_dump);

            let rootset = get_rootset(ctxt);
            // dump_rootset(&rootset);
        }

        let ptr = unsafe { libc::malloc(size) };
        let ptr = Ptr::new(ptr);

        self.memory.push(ptr);
        self.allocated += size;

        if ctxt.args.flag_gc_dump {
            println!("allocate {} bytes: {:x} (total: {} bytes, {} objects)", size,
                ptr.raw() as usize, self.allocated, self.memory.len());
        }

        println!("-------------\n");

        ptr
    }
}

fn dump_rootset(rootset: &Vec<usize>) {
    print!("rootset = [");

    for (ind, &ptr) in rootset.iter().enumerate() {
        if ind > 0 {
            print!(", ");
        }

        print!("{:x}", ptr);
    }

    println!("]");
}

fn mark(rootset: &Vec<usize>) {
    for &root in rootset {
        mark_recursive(root);
    }
}

fn mark_recursive(ptr: usize) {
    println!("mark {:x}", ptr);
    let mem = unsafe { &mut *(ptr as *mut Header) };

    mem.mark();
}

fn sweep(memory: &mut Vec<Ptr>, dump: bool) {
    let mut i = 0;

    while i < memory.len() {
        let ptr = memory[i];
        let obj = unsafe { &mut *(ptr.raw() as *mut Header) };

        if !obj.is_marked() {
            if dump {
                println!("sweep {:x}", ptr.raw() as usize);
            }

            unsafe { libc::free(ptr.raw()) };
            memory.remove(i);
            continue;
        }

        i += 1;
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
