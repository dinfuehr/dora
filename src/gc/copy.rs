use ctxt::SemContext;
use driver::cmd::Args;
use gc::bump::BumpAllocator;
use gc::root::{get_rootset, Slot};
use gc::tlab;
use gc::{formatted_size, Address, Collector, Region};
use mem;
use object::Obj;
use os::{self, ProtType};
use timer::Timer;

pub struct CopyCollector {
    total: Region,
    separator: Address,

    alloc: BumpAllocator,
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
            println!("GC: {}; semi size: {}", heap, formatted_size(semi_size),);
        }

        CopyCollector {
            total: heap,
            separator: separator,
            alloc: BumpAllocator::new(heap_start, separator),
        }
    }
}

impl Collector for CopyCollector {
    fn alloc_tlab_area(&self, ctxt: &SemContext, size: usize) -> Option<Region> {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return Some(ptr.region_start(size));
        }

        self.collect(ctxt);

        let ptr = self.alloc.bump_alloc(size);

        return if ptr.is_null() {
            None
        } else {
            Some(ptr.region_start(size))
        };
    }

    fn alloc_normal(&self, ctxt: &SemContext, size: usize, _array_ref: bool) -> Address {
        let ptr = self.alloc.bump_alloc(size);

        if ptr.is_non_null() {
            return ptr;
        }

        self.collect(ctxt);
        self.alloc.bump_alloc(size)
    }

    fn alloc_large(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> Address {
        self.alloc_normal(ctxt, size, array_ref)
    }

    fn collect(&self, ctxt: &SemContext) {
        // make heap iterable
        tlab::make_iterable(ctxt);

        let rootset = get_rootset(ctxt);
        self.copy_collect(ctxt, &rootset);
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
    fn copy_collect(&self, ctxt: &SemContext, rootset: &[Slot]) {
        let mut timer = Timer::new(ctxt.args.flag_gc_verbose);

        // enable writing into to-space again (for debug builds)
        if cfg!(debug_assertions) {
            let to_space = self.to_space();
            os::mprotect(to_space.start.to_ptr(), to_space.size(), ProtType::Writable);
        }

        // empty to-space
        let to_space = self.to_space();
        let from_space = self.from_space();

        // determine size of heap before collection
        let old_size = self.alloc.top().offset_from(from_space.start);

        let mut top = to_space.start;
        let mut scan = top;

        for root in rootset {
            let root_ptr = root.get();

            if from_space.contains(root_ptr) {
                root.set(self.copy(root_ptr, &mut top));
            }
        }

        while scan < top {
            let object: &mut Obj = unsafe { &mut *scan.to_mut_ptr() };

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if from_space.contains(field_ptr) {
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

        self.alloc.reset(top, to_space.end);

        timer.stop_with(|time_pause| {
            let new_size = top.offset_from(to_space.start);
            let garbage = old_size - new_size;
            let garbage_ratio = if old_size == 0 {
                0f64
            } else {
                (garbage as f64 / old_size as f64) * 100f64
            };

            println!(
                "GC: Copy GC ({:.1} ms, {}->{} size, {}/{:.0}% garbage)",
                time_pause,
                formatted_size(old_size),
                formatted_size(new_size),
                formatted_size(garbage),
                garbage_ratio
            );
        });
    }

    fn copy(&self, obj_addr: Address, top: &mut Address) -> Address {
        let obj = obj_addr.to_mut_obj();

        if let Some(fwd) = obj.header().vtbl_forwarded() {
            return fwd;
        }

        let addr = *top;
        let obj_size = obj.size();

        obj.copy_to(addr, obj_size);
        *top = top.offset(obj_size);

        obj.header_mut().vtbl_forward_to(addr);

        addr
    }

    pub fn from_space(&self) -> Region {
        if self.alloc.limit() == self.separator {
            Region::new(self.total.start, self.separator)
        } else {
            Region::new(self.separator, self.total.end)
        }
    }

    pub fn to_space(&self) -> Region {
        if self.alloc.limit() == self.separator {
            Region::new(self.separator, self.total.end)
        } else {
            Region::new(self.total.start, self.separator)
        }
    }
}
