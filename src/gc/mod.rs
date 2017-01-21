use libc;
use std::ptr::{self, write_bytes};

use ctxt::{Context, get_ctxt};
use gc::code::CodeSpace;
use gc::copy::{minor_collect, SemiSpace};
use gc::root::{get_rootset, IndirectObj};
use object::Obj;
use timer::{in_ms, Timer};

mod code;
mod copy;
mod root;

const INITIAL_THRESHOLD: usize = 128;
const USED_RATIO: f64 = 0.75;

const INITIAL_SIZE: usize = 64 * 1024;
const LARGE_OBJECT_SIZE: usize = 64 * 1024;

pub struct Gc {
    obj_start: *mut Obj,
    obj_end: *mut Obj,
    bytes_allocated: usize,
    threshold: usize,
    pub cur_marked: bool,

    pub collect_duration: u64,
    pub total_allocated: u64,
    pub collections: u64,
    pub allocations: u64,

    from_space: SemiSpace,
    to_space: SemiSpace,

    code_space: CodeSpace,
}

impl Gc {
    pub fn new() -> Gc {
        Gc {
            obj_start: ptr::null_mut(),
            obj_end: ptr::null_mut(),
            cur_marked: true,
            bytes_allocated: 0,
            threshold: INITIAL_THRESHOLD,

            collect_duration: 0,
            total_allocated: 0,
            collections: 0,
            allocations: 0,

            from_space: SemiSpace::new(INITIAL_SIZE),
            to_space: SemiSpace::new(INITIAL_SIZE),
            code_space: CodeSpace::new(),
        }
    }

    pub fn alloc_copy(&mut self, ctxt: &Context, size: usize) -> *mut Obj {
        if ctxt.args.flag_gc_stress {
            minor_collect(ctxt, &mut self.from_space, &mut self.to_space);
        }

        if size > LARGE_OBJECT_SIZE {
            panic!("large object heap not implemented yet.");
        }

        let mut ptr = self.to_space.allocate(size);

        if ptr.is_null() {
            minor_collect(ctxt, &mut self.from_space, &mut self.to_space);

            ptr = self.to_space.allocate(size);

            if ptr.is_null() {
                panic!("out of memory");
            }
        }

        ptr as *mut Obj
    }

    pub fn alloc(&mut self, size: usize) -> *mut Obj {
        let ctxt = get_ctxt();

        if ctxt.args.flag_gc_stress {
            // with --gc-stress collect garbage at every allocation
            // useful for testing
            self.collect(ctxt);

            // do we pass threshold with this allocation?
        } else if self.bytes_allocated + size > self.threshold {
            // collect garbage
            self.collect(ctxt);

            // if still more memory than USED_RATIO % of the threshold,
            // we need to increase the threshold
            if (self.bytes_allocated + size) as f64 > self.threshold as f64 * USED_RATIO {
                let saved_threshold = self.threshold;
                self.threshold = (self.threshold as f64 / USED_RATIO) as usize;

                if ctxt.args.flag_gc_dump {
                    println!("GC: increase threshold from {} to {}",
                             saved_threshold,
                             self.threshold);
                }
            }
        }

        let ptr = unsafe { libc::malloc(size) as *mut Obj };
        unsafe {
            write_bytes(ptr as *mut u8, 0, size);
        }

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
        self.allocations += 1;

        ptr
    }

    pub fn collect(&mut self, ctxt: &Context) {
        let active_timer = ctxt.args.flag_gc_dump || ctxt.args.flag_gc_stats;
        let mut timer = Timer::new(active_timer);
        let rootset = get_rootset(ctxt);

        mark_literals(ctxt, self.cur_marked);
        mark_rootset(ctxt, rootset, self.cur_marked);

        let cur_marked = self.cur_marked;
        sweep(self, cur_marked);

        // switch cur_marked value, so that we don't have to unmark all
        // objects in the beginning of the next collection
        self.cur_marked = !self.cur_marked;
        self.collections += 1;

        timer.stop_with(|dur| {
            self.collect_duration += dur;

            if ctxt.args.flag_gc_dump {
                println!("GC: collect garbage ({} ms)", in_ms(dur));
            }
        });
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        let mut obj = self.obj_start;

        while !obj.is_null() {
            let curr = unsafe { &mut *obj };
            obj = unsafe { &mut *obj }.header().succ();

            unsafe {
                libc::free(curr as *mut _ as *mut libc::c_void);
            }
        }
    }
}

fn mark_literals(ctxt: &Context, cur_marked: bool) {
    let literals = ctxt.literals.lock().unwrap();

    for lit in literals.iter() {
        mark_recursive(ctxt, lit.raw() as *mut Obj, cur_marked);
    }
}

fn mark_rootset(ctxt: &Context, rootset: Vec<IndirectObj>, cur_marked: bool) {
    for root in rootset {
        mark_recursive(ctxt, root.obj_ptr(), cur_marked);
    }
}

fn mark_recursive(ctxt: &Context, obj: *mut Obj, cur_marked: bool) {
    if obj.is_null() {
        return;
    }

    let mut elements: Vec<*mut Obj> = vec![obj];

    while !elements.is_empty() {
        let ptr = elements.pop().unwrap();
        let obj = unsafe { &mut *ptr };

        if obj.header().marked() != cur_marked {
            obj.header_mut().set_mark(cur_marked);
            let mut classid = obj.header().vtbl().class().id;

            loop {
                let cls = ctxt.classes[classid].borrow();

                for field in &cls.fields {
                    if field.ty.reference_type() {
                        let addr = ptr as isize + field.offset as isize;
                        let obj = unsafe { *(addr as *const usize) } as *mut Obj;

                        if obj.is_null() {
                            continue;
                        }

                        elements.push(obj);
                    }
                }

                if let Some(parent_class) = cls.parent_class {
                    classid = parent_class;

                } else {
                    break;
                }
            }
        }
    }
}

fn sweep(gc: &mut Gc, cur_marked: bool) {
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

    // if no objects left, obj_start needs to be reset to null
    if last.is_null() {
        gc.obj_start = ptr::null_mut();
    }

    // last survived object is now end of list
    gc.obj_end = last;
}
