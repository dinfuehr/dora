use libc;

use std::ptr;

use ctxt::Context;
use gc::root::{get_rootset, IndirectObj};
use gc::GcStats;
use object::Obj;
use timer::{in_ms, Timer};

const INITIAL_THRESHOLD: usize = 128;
const USED_RATIO: f64 = 0.75;

pub struct MallocSpace {
    obj_start: *mut Obj,
    obj_end: *mut Obj,
    bytes_allocated: usize,
    threshold: usize,
    pub cur_marked: bool,
}

impl MallocSpace {
    pub fn new() -> MallocSpace {
        MallocSpace {
            obj_start: ptr::null_mut(),
            obj_end: ptr::null_mut(),
            cur_marked: true,
            bytes_allocated: 0,
            threshold: INITIAL_THRESHOLD,
        }
    }

    pub fn alloc(&mut self, ctxt: &Context, stats: &mut GcStats, size: usize) -> *mut u8 {
        if ctxt.args.flag_gc_stress {
            // with --gc-stress collect garbage at every allocation
            // useful for testing
            let rootset = get_rootset(ctxt);
            self.collect(ctxt, stats, rootset);

            // do we pass threshold with this allocation?
        } else if self.bytes_allocated + size > self.threshold {
            // collect garbage
            let rootset = get_rootset(ctxt);
            self.collect(ctxt, stats, rootset);

            // if still more memory than USED_RATIO % of the threshold,
            // we need to increase the threshold
            if (self.bytes_allocated + size) as f64 > self.threshold as f64 * USED_RATIO {
                let saved_threshold = self.threshold;
                self.threshold = (self.threshold as f64 / USED_RATIO) as usize;

                if ctxt.args.flag_gc_events {
                    println!("GC: increase threshold from {} to {}",
                             saved_threshold,
                             self.threshold);
                }
            }
        }

        let ptr = unsafe { libc::malloc(size) as *mut Obj };
        unsafe {
            ptr::write_bytes(ptr as *mut u8, 0, size);
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
        stats.total_allocated += size as u64;
        stats.allocations += 1;

        ptr as *mut u8
    }

    pub fn collect(&mut self, ctxt: &Context, stats: &mut GcStats, rootset: Vec<IndirectObj>) {
        let active_timer = ctxt.args.flag_gc_events || ctxt.args.flag_gc_stats;
        let mut timer = Timer::new(active_timer);

        mark_rootset(rootset, self.cur_marked);

        let cur_marked = self.cur_marked;
        sweep(self, stats, cur_marked);

        // switch cur_marked value, so that we don't have to unmark all
        // objects in the beginning of the next collection
        self.cur_marked = !self.cur_marked;
        stats.collections += 1;

        timer.stop_with(|dur| {
            stats.collect_duration += dur;

            if ctxt.args.flag_gc_events {
                println!("GC: collect garbage ({} ms)", in_ms(dur));
            }
        });
    }
}

impl Drop for MallocSpace {
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

fn mark_rootset(rootset: Vec<IndirectObj>, cur_marked: bool) {
    for root in rootset {
        mark_recursive(root.get(), cur_marked);
    }
}

fn mark_recursive(obj: *mut Obj, cur_marked: bool) {
    if obj.is_null() {
        return;
    }

    let mut elements: Vec<*mut Obj> = vec![obj];

    while !elements.is_empty() {
        let ptr = elements.pop().unwrap();
        let obj = unsafe { &mut *ptr };

        if obj.header().marked() != cur_marked {
            obj.header_mut().set_mark(cur_marked);

            obj.visit_reference_fields(|child| {
                let child = child.get();

                if !child.is_null() {
                    elements.push(child);
                }
            });
        }
    }
}

fn sweep(gc: &mut MallocSpace, _: &mut GcStats, cur_marked: bool) {
    let mut obj = gc.obj_start;
    let mut last: *mut Obj = ptr::null_mut();

    while !obj.is_null() {
        let curr = unsafe { &mut *obj };
        let succ = curr.header().succ();

        if curr.header().marked() != cur_marked {
            let size = curr.size();

            unsafe {
                // overwrite memory for better detection of gc bugs
                if cfg!(debug_assertions) {
                    ptr::write_bytes(obj as *mut u8, 0xcc, size);
                }

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
