use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::{CARD_SIZE, PROMOTION_AGE, Region};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::old::OldGen;
use object::Obj;
use timer::{in_ms, Timer};

pub struct YoungGen {
    // bounds of from- & to-space
    total: Region,
    
    // address that separates from & to-space
    separator: Address,

    // address of next free memory
    free: AtomicUsize,

    // end of free memory (either separator or total.end)
    end: AtomicUsize,
}

impl YoungGen {
    pub fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = (young_end.to_usize() - young_start.to_usize()) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            separator: half_address,
            free: AtomicUsize::new(young_start.to_usize()),
            end: AtomicUsize::new(half_address.to_usize()),
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.end.load(Ordering::Relaxed) {
                return ptr::null();
            }

            let res =
                self.free
                    .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old as *const u8
    }

    pub fn collect(
        &self,
        ctxt: &SemContext,
        rootset: Vec<IndirectObj>,
        card_table: &CardTable,
        crossing_map: &CrossingMap,
        old: &OldGen,
    ) {
        let mut timer = Timer::new(ctxt.args.flag_gc_events);

        let mut scan;
        let scan_end: Address = self.free.load(Ordering::SeqCst).into();

        let mut free;
        let new_end;

        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            scan = self.total.start;
            free = self.separator;
            new_end = self.total.end;

        } else {
            scan = self.separator;
            free = self.total.start;
            new_end = self.separator;
        }

        for &root in &rootset {
            let root_ptr = root.get();

            if self.total.includes(Address::from_ptr(root_ptr)) {
                root.set(copy(root_ptr, &mut free, old));
            }
        }

        // copy objects for old -> young references
        card_table.visit_dirty(|card| {
            let crossing_entry = crossing_map.get(card);

            // card contains: any data but no references, then first object
            if crossing_entry.is_first_object() {
                let card_start = crossing_map.address_of_card(card);

                // first_object() returns number of words before end of card
                let offset_from_end = crossing_entry.first_object() as usize * 8;
                let _address = card_start.offset(CARD_SIZE - offset_from_end);

                unimplemented!();

            // card contains: references, then first object
            } else if crossing_entry.is_references_at_start() {
                let _number_references = crossing_entry.references_at_start() as usize;
                let _ptr = crossing_map.address_of_card(card);

                unimplemented!();

            // object spans multiple cards
            } else if crossing_entry.is_previous_card() {
                unimplemented!();

            } else {
                assert!(crossing_entry.is_no_references());
                panic!("dirty card without references: can this ever happen?");
            }
        });

        while scan < scan_end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();

                if self.total.includes(Address::from_ptr(child_ptr)) {
                    child.set(copy(child_ptr, &mut free, old));
                }
            });

            scan = scan.offset(object.size());
        }

        // swap spaces
        self.end.store(new_end.to_usize(), Ordering::Relaxed);

        // update start of free memory
        self.free.store(free.to_usize(), Ordering::SeqCst);

        // memset from-space to garbage data for debug builds
        // makes sure that no pointer into from-space is left
        if cfg!(debug_assertions) {
            let start = if new_end == self.separator {
                self.total.start
            } else {
                self.separator
            };

            let size = self.separator.to_usize() - self.total.start.to_usize();

            unsafe {
                ptr::write_bytes(start.to_mut_ptr::<u8>(), 0xcc, size);
            }
        }

        timer.stop_with(|dur| {
            if ctxt.args.flag_gc_events {
                println!("GC minor: collect garbage ({} ms)", in_ms(dur));
            }
        });
    }
}

pub fn copy(obj: *mut Obj, free: &mut Address, old: &OldGen) -> *mut Obj {
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
            addr = Address::from_ptr(old.alloc(obj_size));
            copy_object(obj, addr, obj_size);

        // otherwise the object remains in the young generation for now
        } else {
            addr = *free;
            copy_object(obj, addr, obj_size);
            *free = addr.offset(obj_size);
        }

        set_forwarding_address(obj, addr);

        addr.to_mut_ptr::<Obj>()
    }
}

fn copy_object(obj: &Obj, addr: Address, size: usize) {
    unsafe {
        ptr::copy_nonoverlapping(obj as *const Obj as *const u8, addr.to_mut_ptr::<u8>(), size);
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