use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::{PROMOTION_AGE, Region};
use object::Obj;
use timer::{in_ms, Timer};

pub struct YoungGen {
    total: Region,
    from: SemiSpace,
    to: SemiSpace,
}

impl YoungGen {
    pub fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = (young_end.to_usize() - young_start.to_usize()) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            from: SemiSpace::new(young_start, half_address),
            to: SemiSpace::new(half_address, young_end),
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.from.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.from.end.to_usize() {
                return ptr::null();
            }

            let res =
                self.from.free
                    .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old as *const u8
    }

    pub fn collect(
        &mut self,
        ctxt: &SemContext,
        rootset: Vec<IndirectObj>,
    ) {
        let mut timer = Timer::new(ctxt.args.flag_gc_events);

        let mut scan = self.from.start;
        let end: Address = self.from.free.load(Ordering::SeqCst).into();
        let mut free = self.to.start;

        for &root in &rootset {
            let root_ptr = root.get();

            if self.from.includes(root_ptr as usize) {
                root.set(copy(root_ptr, &mut free));
            }
        }

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();

                if self.from.includes(child_ptr as usize) {
                    child.set(copy(child_ptr, &mut free));
                }
            });

            scan = scan.offset(object.size());
        }

        // memset from-space to garbage data for debug builds
        // makes sure that no pointer into from-space is left
        if cfg!(debug_assertions) {
            unsafe {
                ptr::write_bytes(self.from.start.to_usize() as *mut u8, 0xcc, self.from.size());
            }
        }

        self.swap_spaces();

        timer.stop_with(|dur| {
            if ctxt.args.flag_gc_events {
                println!("GC minor: collect garbage ({} ms)", in_ms(dur));
            }
        });
    }

    fn swap_spaces(&mut self) {
        unimplemented!();
    }
}

struct SemiSpace {
    start: Address,
    free: AtomicUsize,
    uncommitted: Address,
    end: Address,
}

impl SemiSpace {
    fn new(start: Address, end: Address) -> SemiSpace {
        SemiSpace {
            start: start,
            free: AtomicUsize::new(start.to_usize()),
            uncommitted: start,
            end: end,
        }
    }

    fn empty() -> SemiSpace {
        SemiSpace {
            start: Address::null(),
            free: AtomicUsize::new(0),
            uncommitted: Address::null(),
            end: Address::null(),
        }
    }

    fn size(&self) -> usize {
        self.end.to_usize() - self.start.to_usize()
    }

    fn committed_size(&self) -> usize {
        self.uncommitted.to_usize() - self.start.to_usize()
    }

    fn uncommitted_size(&self) -> usize {
        self.end.to_usize() - self.uncommitted.to_usize()
    }

    fn includes(&self, ptr: usize) -> bool {
        self.start.to_usize() <= ptr && ptr < self.end.to_usize()
    }
}

pub fn copy(obj: *mut Obj, free: &mut Address) -> *mut Obj {
    let obj = unsafe { &mut *obj };
    let addr = get_forwarding_address(obj);

    if is_forwarding_address(addr) {
        unmark_forwarding_address(addr).to_mut_ptr::<Obj>()
    } else {
        let obj_size = obj.size();
        let age = obj.header_mut().increase_age();

        let addr: Address;

        // if object is old enough we copy it into the old generation
        if age >= PROMOTION_AGE {
            unimplemented!();

        // otherwise the object remains in the young generation for nows
        } else {
            addr = *free;

            unsafe {
                ptr::copy_nonoverlapping(obj as *const Obj as *const u8, addr.to_mut_ptr::<u8>(), obj_size);
                *free = addr.offset(obj_size);
            }
        }

        set_forwarding_address(obj, addr);

        addr.to_mut_ptr::<Obj>()
    }
}

pub fn is_forwarding_address(obj: Address) -> bool {
    (obj.to_usize() & 1) == 1
}

pub fn mark_forwarding_address(obj: Address) -> Address {
    (obj.to_usize() | 1).into()
}

pub fn unmark_forwarding_address(obj: Address) -> Address {
    (obj.to_usize() & !1).into()
}

pub fn get_forwarding_address(obj: &Obj) -> Address {
    unsafe { *(obj as *const Obj as *const Address) }
}

pub fn set_forwarding_address(obj: &mut Obj, addr: Address) {
    unsafe {
        *(obj as *mut Obj as *mut Address) = mark_forwarding_address(addr);
    }
}