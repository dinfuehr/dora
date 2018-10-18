use std::sync::atomic::{AtomicUsize, Ordering};

use ctxt::SemContext;
use driver::cmd::Args;
use gc::root::{get_rootset, IndirectObj};
use gc::{Address, Collector, formatted_size, Region};
use gc::tlab;
use mem;
use object::Obj;
use os::{self, ProtType};
use timer::Timer;

pub struct CopyCollector {
    total: Region,
    separator: Address,

    top: AtomicUsize,
    end: AtomicUsize,
}

impl CopyCollector {
    pub fn new(args: &Args) -> CopyCollector {
        let alignment = 2 * (os::page_size() as usize);
        let heap_size = mem::align_usize(args.max_heap_size(), alignment);
        let ptr = os::mmap(heap_size, os::Writable);

        if ptr.is_null() {
            panic!("could not allocate semi space of size {} bytes", heap_size);
        }

        let heap_start = Address::from_ptr(ptr);
        let heap = heap_start.region_start(heap_size);

        let semi_size = heap_size / 2;
        let separator = heap_start.offset(semi_size);

        if args.flag_gc_verbose {
            println!(
                "GC: {}; semi size: {}",
                heap,
                formatted_size(semi_size),
            );
        }

        CopyCollector {
            total: heap,
            separator: separator,
            top: AtomicUsize::new(heap_start.to_usize()),
            end: AtomicUsize::new(separator.to_usize()),
        }
    }
}

impl Collector for CopyCollector {
    fn alloc_tlab(&self, ctxt: &SemContext, size: usize, _array_ref: bool) -> Address {
        // try to allocate in current tlab
        if let Some(addr) = tlab::allocate(ctxt, size) {
            return addr;
        }

        // if there is not enough space, make heap iterable by filling tlab with unused objects
        tlab::make_iterable(ctxt);

        // allocate new tlab
        if let Some(tlab) = self.alloc_tlab_area(ctxt, tlab::calculate_size(size)) {
            let object_start = tlab.start;
            let tlab = Region::new(tlab.start.offset(size), tlab.end);

            // initialize TLAB to new boundaries
            tlab::initialize(ctxt, tlab);

            // object is allocated before TLAB
            object_start
        } else {
            self.bump_alloc(size)
        }
    }

    fn alloc_normal(&self, ctxt: &SemContext, size: usize, _array_ref: bool) -> Address {
        let ptr = self.bump_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        self.collect(ctxt);
        self.bump_alloc(size)
    }

    fn alloc_large(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> Address {
        self.alloc_normal(ctxt, size, array_ref)
    }

    fn collect(&self, ctxt: &SemContext) {
        // make heap iterable
        tlab::make_iterable(ctxt);

        let rootset = get_rootset(ctxt);
        self.collect_from(ctxt, &rootset);
    }

    fn minor_collect(&self, ctxt: &SemContext) {
        self.collect(ctxt);
    }
}

impl Drop for CopyCollector {
    fn drop(&mut self) {
        os::munmap(self.total.start.to_ptr(), self.total.size());
    }
}

impl CopyCollector {
    fn alloc_tlab_area(&self, ctxt: &SemContext, size: usize) -> Option<Region> {
        let ptr = self.bump_alloc(size);

        if ptr.is_non_null() {
            return Some(ptr.region_start(size));
        }

        self.collect(ctxt);

        let ptr = self.bump_alloc(size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn bump_alloc(&self, size: usize) -> Address {
        let mut old = self.top.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new > self.end.load(Ordering::Relaxed) {
                return Address::null();
            }

            let res = self
                .top
                .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old.into()
    }

    fn collect_from(&self, ctxt: &SemContext, rootset: &[IndirectObj]) {
        let mut timer = Timer::new(ctxt.args.flag_gc_events);

        // enable writing into to-space again (for debug builds)
        if cfg!(debug_assertions) {
            let to_space = self.to_space();
            os::mprotect(to_space.start.to_ptr(), to_space.size(), ProtType::Writable);
        }

        // empty to-space
        let to_space = self.to_space();
        let from_space = self.from_space();

        let mut top = to_space.start;
        let mut scan = top;

        for root in rootset {
            let root_ptr = root.get();

            if from_space.contains(Address::from_ptr(root_ptr)) {
                root.set(self.copy(root_ptr, &mut top));
            }
        }

        while scan < top {
            let object: &mut Obj = unsafe { &mut *scan.to_mut_ptr() };

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if from_space.contains(Address::from_ptr(field_ptr)) {
                    field.set(self.copy(field_ptr, &mut top));
                }
            });

            scan = scan.offset(object.size());
        }

        // disable access in current from-space
        // makes sure that no pointer into from-space is left (in debug-builds)
        if cfg!(debug_assertions) {
            os::mprotect(from_space.start.to_ptr(), from_space.size(), ProtType::None);
        }

        self.top.store(top.to_usize(), Ordering::Relaxed);
        self.end.store(to_space.end.to_usize(), Ordering::Relaxed);

        timer.stop_with(|time_pause| {
            if ctxt.args.flag_gc_events {
                println!("Copy GC: collect garbage ({:.1} ms)", time_pause);
            }
        });
    }

    fn copy(&self, obj: *mut Obj, top: &mut Address) -> *mut Obj {
        let obj = unsafe { &mut *obj };

        if let Some(fwd) = obj.header().vtbl_forwarded() {
            return fwd.to_mut_ptr();
        }

        let addr = *top;
        let obj_size = obj.size();

        obj.copy_to(addr, obj_size);
        *top = top.offset(obj_size);

        obj.header_mut().vtbl_forward_to(addr);

        addr.to_mut_ptr()
    }

    pub fn from_space(&self) -> Region {
        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            Region::new(self.total.start, self.separator)
        } else {
            Region::new(self.separator, self.total.end)
        }
    }

    pub fn to_space(&self) -> Region {
        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            Region::new(self.separator, self.total.end)
        } else {
            Region::new(self.total.start, self.separator)
        }
    }
}
