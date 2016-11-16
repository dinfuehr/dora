use libc;
use std::ptr::{self, write_bytes};
use time;

use cpu::get_rootset;
use ctxt::get_ctxt;
use object::Obj;

const INITIAL_THRESHOLD: usize = 128;
const USED_RATIO: f64 = 0.75;

pub struct Gc {
    obj_start: *mut Obj,
    obj_end: *mut Obj,
    bytes_allocated: usize,
    threshold: usize,
    pub cur_marked: bool,

    pub duration: u64,
    pub malloc_duration: u64,
    pub collect_duration: u64,
    pub sweep_duration: u64,
    pub total_allocated: u64,
    pub collections: u64,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            obj_start: ptr::null_mut(),
            obj_end: ptr::null_mut(),
            cur_marked: true,
            bytes_allocated: 0,
            threshold: INITIAL_THRESHOLD,
            duration: 0,
            malloc_duration: 0,
            collect_duration: 0,
            sweep_duration: 0,
            total_allocated: 0,
            collections: 0,
        }
    }

    pub fn alloc(&mut self, size: usize) -> *mut Obj {
        let alloc_start = time::precise_time_ns();
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

        let malloc_start = time::precise_time_ns();
        let ptr = unsafe { libc::malloc(size) as *mut Obj };
        unsafe { write_bytes(ptr as *mut u8, 0, size); }
        self.malloc_duration += time::precise_time_ns() - malloc_start;

        {
            let ptr = ptr as *mut Obj;

            if self.obj_end.is_null() {
                assert!(self.obj_start.is_null());

                self.obj_start = ptr;
                self.obj_end = ptr;

            } else {
                assert!(!self.obj_start.is_null());

                let obj_end = unsafe { &mut *self.obj_end };
                obj_end.header_mut().set_succ(ptr);

                self.obj_end = ptr;
            }
        }

        self.bytes_allocated += size;
        self.total_allocated += size as u64;

        if ctxt.args.flag_gc_dump {
            println!("GC: allocate {} bytes: {:x} (total {} bytes, threshold {})",
                     size, ptr as usize,
                     self.bytes_allocated, self.threshold);
        }

        self.duration += time::precise_time_ns() - alloc_start;

        ptr
    }

    pub fn collect(&mut self) {
        let collect_start = time::precise_time_ns();
        let ctxt = get_ctxt();

        let rootset = get_rootset(ctxt);

        if ctxt.args.flag_gc_dump {
            println!("GC: collect garbage");
        }

        mark_literals(self.cur_marked);
        mark_rootset(&rootset, self.cur_marked);

        let sweep_start = time::precise_time_ns();
        let cur_marked = self.cur_marked;
        sweep(self, ctxt.args.flag_gc_dump, cur_marked);
        self.sweep_duration += time::precise_time_ns() - sweep_start;

        // switch cur_marked value, so that I don't have to unmark all
        // objects in the beginning of the next collection
        self.cur_marked = !self.cur_marked;

        self.collect_duration += time::precise_time_ns() - collect_start;
        self.collections += 1;
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        let mut obj = self.obj_start;

        while !obj.is_null() {
            let curr = obj;
            obj = unsafe { &mut *obj }.header().succ();

            unsafe {
                libc::free(curr as *mut libc::c_void);
            }
        }
    }
}

fn mark_literals(cur_marked: bool) {
    let ctxt = get_ctxt();
    let literals = ctxt.literals.lock().unwrap();

    for lit in literals.iter() {
        mark_recursive(lit.raw() as *mut Obj, cur_marked);
    }
}

fn mark_rootset(rootset: &Vec<usize>, cur_marked: bool) {
    for &root in rootset {
        mark_recursive(root as *mut Obj, cur_marked);
    }
}

fn mark_recursive(obj: *mut Obj, cur_marked: bool) {
    if obj.is_null() { return; }

    let mut elements: Vec<*mut Obj> = vec![obj];

    while !elements.is_empty() {
        let ptr = elements.pop().unwrap();
        let obj = unsafe { &mut *ptr };

        if obj.header().marked() != cur_marked {
            obj.header_mut().set_mark(cur_marked);
            let class = obj.header().vtbl().class();

            for field in class.all_fields(get_ctxt()) {
                if field.ty.reference_type() {
                    let addr = ptr as isize + field.offset as isize;
                    let obj = unsafe { *(addr as *const usize) } as *mut Obj;

                    if obj.is_null() { continue; }

                    elements.push(obj);
                }
            }
        }
    }
}

fn sweep(gc: &mut Gc, dump: bool, cur_marked: bool) {
    let mut obj = gc.obj_start;
    let mut last: *mut Obj = ptr::null_mut();

    while !obj.is_null() {
        let curr = unsafe { &mut *obj };
        let succ = curr.header().succ();

        if curr.header().marked() != cur_marked {
            let size = curr.size();

            unsafe {
                // overwrite memory for better detection of gc bugs
                write_bytes(obj as *mut u8, 0xcc, size);

                // free unused memory
                libc::free(obj as *mut libc::c_void);
            }

            gc.bytes_allocated -= size;

            if dump {
                println!("sweep {:x} with {} bytes", obj as usize, size);
            }

        } else {
            if last.is_null() {
                gc.obj_start = obj;
            } else {
                let last = unsafe { &mut *last };
                last.header_mut().set_succ(obj);
            }

            // last survived object
            last = obj;
        }

        // set next handled object
        obj = succ;
    }

    gc.obj_end = last;
}
