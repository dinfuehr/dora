use std::cmp;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::{CARD_SIZE, Region};
use gc::swiper::card::CardTable;
use gc::swiper::crossing::{CrossingEntry, CrossingMap};
use gc::swiper::old::OldGen;
use mem;
use os::{self, ProtType};
use object::Obj;
use timer::{in_ms, Timer};

pub struct YoungGen {
    // bounds of from- & to-space
    total: Region,

    // size of both from- or to-space.
    // Not combined, use total.size() for that.
    size: usize,

    // address that separates from & to-space
    separator: Address,

    // address of next free memory
    free: AtomicUsize,

    // end of free memory (either separator or total.end)
    end: AtomicUsize,

    // separates survived from newly allocated objects
    // needed to decide whether to promote object into old space
    age_marker: AtomicUsize,
}

impl YoungGen {
    pub fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = young_end.offset_from(young_start) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            size: half_size,
            separator: half_address,
            age_marker: AtomicUsize::new(young_start.to_usize()),
            free: AtomicUsize::new(young_start.to_usize()),
            end: AtomicUsize::new(half_address.to_usize()),
        }
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn should_be_promoted(&self, addr: Address) -> bool {
        debug_assert!(self.contains(addr));

        addr.to_usize() < self.age_marker.load(Ordering::Relaxed)
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.end.load(Ordering::Relaxed) {
                return ptr::null();
            }

            let res = self.free.compare_exchange_weak(
                old,
                new,
                Ordering::SeqCst,
                Ordering::Relaxed,
            );

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

        let mut scan: Address;
        let new_end: Address;

        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            scan = self.separator;
            new_end = self.total.end;

        } else {
            // self.end == self.total.end
            scan = self.total.start;
            new_end = self.separator;
        }

        let mut free = scan;

        if cfg!(debug_assertions) {
            // make memory readable & writable again, so that we
            // can copy objects to the to-space.
            // Since this has some overhead, do it only in debug builds.
            os::mprotect(free.to_ptr::<u8>(), self.size, ProtType::Writable);
        }

        // detect all references from roots into young generation
        for &root in &rootset {
            let root_ptr = root.get();

            if self.contains(Address::from_ptr(root_ptr)) {
                root.set(copy(root_ptr, &mut free, self, old));
            }
        }

        // detect references from old generation (dirty cards) into young generation
        copy_dirty_cards(card_table, crossing_map, &mut free, self, old);

        // visit all fields in copied objects
        while scan < free {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();

                if self.contains(Address::from_ptr(child_ptr)) {
                    child.set(copy(child_ptr, &mut free, self, old));
                }
            });

            scan = scan.offset(object.size());
        }

        // swap spaces
        self.end.store(new_end.to_usize(), Ordering::Relaxed);

        // update start of free memory
        self.age_marker.store(free.to_usize(), Ordering::Relaxed);
        self.free.store(free.to_usize(), Ordering::SeqCst);

        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) {
            let start = if new_end == self.separator {
                self.total.start
            } else {
                self.separator
            };

            os::mprotect(start.to_ptr::<u8>(), self.size, ProtType::None);
        }

        timer.stop_with(|dur| if ctxt.args.flag_gc_events {
            println!("GC minor: collect garbage ({} ms)", in_ms(dur));
        });
    }
}

// copy all references from old- into young-generation.
fn copy_dirty_cards(
    card_table: &CardTable,
    crossing_map: &CrossingMap,
    mut free: &mut Address,
    young: &YoungGen,
    old: &OldGen,
) {
    card_table.visit_dirty(|card| {
        let crossing_entry = crossing_map.get(card);
        let card_start = crossing_map.address_of_card(card);
        let card_end = card_start.offset(CARD_SIZE);

        match crossing_entry {
            CrossingEntry::NoRefs => panic!("card dirty without any refs"),
            CrossingEntry::LeadingRefs(refs) => {
                let mut ptr = card_start;

                for _ in 0..refs {
                    let ind_ptr = IndirectObj::from_address(ptr);
                    let dir_ptr = ind_ptr.get();

                    if young.contains(Address::from_ptr(dir_ptr)) {
                        ind_ptr.set(copy(dir_ptr, &mut free, young, old));
                    }

                    ptr = ptr.offset(ptr_width());
                }

                // copy all objects from this card
                copy_card(ptr, card_end, free, young, old);
            }

            CrossingEntry::FirstObjectOffset(offset) => {
                let ptr = card_start.offset(offset as usize * ptr_width());

                // copy all objects from this card
                copy_card(ptr, card_end, free, young, old);
            }
        }
    });
}

fn copy_card(mut ptr: Address, end: Address, mut free: &mut Address, young: &YoungGen, old: &OldGen) {
    let old_end: Address = old.free.load(Ordering::Relaxed).into();
    let end = cmp::min(end, old_end);

    while ptr < end {
        let object = unsafe { &mut *ptr.to_mut_ptr::<Obj>() };

        object.visit_reference_fields(|child| {
            let child_ptr = child.get();

            if young.contains(Address::from_ptr(child_ptr)) {
                child.set(copy(child_ptr, &mut free, young, old));
            }
        });

        ptr = ptr.offset(object.size());
    }
}

pub fn copy(obj: *mut Obj, free: &mut Address, young: &YoungGen, old: &OldGen) -> *mut Obj {
    let obj_addr = Address::from_ptr(obj);
    let obj = unsafe { &mut *obj };
    let fwaddr = get_forwarding_address(obj);

    if is_forwarding_address(fwaddr) {
        unmark_forwarding_address(fwaddr).to_mut_ptr::<Obj>()

    } else {
        let obj_size = obj.size();

        let copy_addr: Address;

        // if object is old enough we copy it into the old generation
        if young.should_be_promoted(obj_addr) {
            copy_addr = Address::from_ptr(old.alloc(obj_size));

        // otherwise the object remains in the young generation for now
        } else {
            copy_addr = *free;
            *free = copy_addr.offset(obj_size);
        }

        copy_object(obj, copy_addr, obj_size);
        set_forwarding_address(obj, copy_addr);

        copy_addr.to_mut_ptr::<Obj>()
    }
}

fn copy_object(obj: &Obj, addr: Address, size: usize) {
    unsafe {
        ptr::copy_nonoverlapping(
            obj as *const Obj as *const u8,
            addr.to_mut_ptr::<u8>(),
            size,
        );
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

#[inline(always)]
fn ptr_width() -> usize {
    mem::ptr_width() as usize
}
