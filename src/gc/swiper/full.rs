use std::collections::{HashMap, HashSet};
use std::sync::atomic::Ordering;

use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use gc::space::Space;
use gc::swiper::card::{CardEntry, CardTable};
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::crossing::CrossingMap;
use gc::swiper::old::OldGen;
use gc::swiper::Region;
use gc::swiper::young::YoungGen;
use mem;
use object::Obj;
use os;
use timer::{in_ms, Timer};

pub struct FullCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    marking_bitmap: MarkingBitmap,
    fwd_table: HashMap<Address, Address>,
    young_refs_set: HashSet<Address>,
    old_top: Address,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
        perm_space: &'a Space,
        rootset: &'a [IndirectObj],
    ) -> FullCollector<'a, 'ast> {
        let marking_bitmap = MarkingBitmap::new(heap.clone());

        FullCollector {
            ctxt: ctxt,
            heap: heap,
            young: young,
            old: old,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            marking_bitmap: marking_bitmap,
            fwd_table: HashMap::new(),
            young_refs_set: HashSet::new(),
            old_top: Address::null(),
        }
    }

    pub fn collect(&mut self) {
        let mut timer = Timer::new(self.ctxt.args.flag_gc_verbose);
        let init_size = self.old.used_region().size();

        self.mark_live();
        self.compute_forward();
        self.update_references();
        self.relocate();

        self.old.free.store(self.old_top.to_usize(), Ordering::SeqCst);
        let new_size = self.old.used_region().size();

        timer.stop_with(|dur| {
            println!("GC: Full GC ({} ms, {}K -> {}K size)",
                in_ms(dur), init_size / 1024, new_size / 1024);
        });
    }

    fn mark_live(&mut self) {
        let mut marking_stack: Vec<Address> = Vec::new();

        for root in self.rootset {
            let root_ptr = Address::from_ptr(root.get());

            if self.heap.contains(root_ptr) {
                marking_stack.push(root_ptr);

            } else {
                debug_assert!(root_ptr.is_null() || self.perm_space.contains(root_ptr));
            }
        }

        while marking_stack.len() > 0 {
            let object_addr = marking_stack.pop().expect("stack already empty");
            let object = unsafe { &mut *object_addr.to_mut_ptr::<Obj>() };
            self.mark(object_addr);

            object.visit_reference_fields(|field| {
                let field_addr = Address::from_ptr(field.get());

                if self.heap.contains(field_addr) {
                    if !self.is_marked_addr(field_addr) {
                        marking_stack.push(field_addr);
                        self.mark(field_addr);
                    }

                } else {
                    debug_assert!(field_addr.is_null() || self.perm_space.contains(field_addr));
                }
            });
        }
    }

    fn compute_forward(&mut self) {
        let used_region = self.old.used_region();

        let mut scan = used_region.start;
        let end = used_region.end;

        let mut fwd = self.old.total.start;

        while scan < end {
            let obj = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let obj_size = obj.size();

            if self.is_marked(obj) {
                self.fwd_table.insert(scan, fwd);
                fwd = fwd.offset(obj_size);
            }

            scan = scan.offset(obj_size);
        }

        self.old_top = fwd;
    }

    fn update_references(&mut self) {
        let used_region = self.old.used_region();

        let mut scan = used_region.start;
        let end = used_region.end;

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let object_size = object.size();

            if self.is_marked(object) {
                let mut young_refs = false;

                object.visit_reference_fields(|field| {
                    let field_addr = Address::from_ptr(field.get());

                    if self.old.contains(field_addr) {
                        let fwd_addr = self.fwd_table.get(&field_addr).expect("forwarding address not found.");
                        field.set(fwd_addr.to_mut_ptr());

                    } else if self.young.contains(field_addr) {
                        young_refs = true;
                    }
                });

                if young_refs {
                    self.young_refs_set.insert(scan);
                }
            }

            scan = scan.offset(object_size);
        }

        for root in self.rootset {
            let root_ptr = Address::from_ptr(root.get());

            if self.old.contains(root_ptr) {
                let fwd_addr = self.fwd_table.get(&root_ptr).expect("forwarding address not found.");
                root.set(fwd_addr.to_mut_ptr());
            }
        }
    }

    fn relocate(&mut self) {
        let used_region = self.old.used_region();

        let mut scan = used_region.start;
        let end = used_region.end;

        self.crossing_map.set_first_object(0.into(), 0);
        let mut young_refs = false;
        let mut last_dest = Address::null();

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let object_size = object.size();

            if self.is_marked(object) {
                let &dest = self.fwd_table.get(&scan).expect("forwarding address not found.");
                object.copy_to(dest, object_size);

                if !young_refs {
                    young_refs = self.young_refs_set.contains(&scan);
                }

                let next_dest = dest.offset(object_size);

                if on_different_cards(dest, next_dest) {
                    self.update_crossing(next_dest);
                    self.update_card(dest, &mut young_refs);
                }

                last_dest = next_dest;
            }

            scan = scan.offset(object_size);
        }

        if !last_dest.is_null() && !start_of_card(last_dest) {
            self.update_card(last_dest, &mut young_refs);
        }
    }

    fn update_crossing(&mut self, addr: Address) {
        let offset = addr.to_usize() & (CARD_SIZE - 1);
        let offset_words = offset / (mem::ptr_width() as usize);

        let card = self.old.card_from_address(addr);

        self.crossing_map.set_first_object(card, offset_words);
    }

    fn update_card(&mut self, addr: Address, young_refs: &mut bool) {
        let card = self.old.card_from_address(addr);

        let card_entry = if *young_refs { CardEntry::Dirty } else { CardEntry:: Clean };
        self.card_table.set(card, card_entry);

        *young_refs = false;
    }

    fn is_marked(&self, obj: &Obj) -> bool {
        self.marking_bitmap.is_marked(Address::from_ptr(obj as *const _))
    }

    fn is_marked_addr(&self, addr: Address) -> bool {
        self.marking_bitmap.is_marked(addr)
    }

    fn mark(&mut self, addr: Address) {
        self.marking_bitmap.mark(addr);
    }
}

fn on_different_cards(curr: Address, next: Address) -> bool {
    (curr.to_usize() >> CARD_SIZE_BITS) != (next.to_usize() >> CARD_SIZE_BITS)
}

fn start_of_card(addr: Address) -> bool {
    (addr.to_usize() & (CARD_SIZE - 1)) == addr.to_usize()
}

pub struct MarkingBitmap {
    heap: Region,
    bitmap: Region,
}

impl MarkingBitmap {
    pub fn new(heap: Region) -> MarkingBitmap {
        let bitmap_size = mem::page_align(heap.size() / mem::ptr_width_usize() / 8);
        let bitmap_start = Address::from_ptr(os::mmap(bitmap_size, os::Writable));

        if bitmap_start.is_null() {
            panic!("could not allocate marking bitmap.");
        }

        MarkingBitmap {
            heap: heap,
            bitmap: Region::new(bitmap_start, bitmap_start.offset(bitmap_size)),
        }
    }

    pub fn mark(&mut self, addr: Address) {
        let (byte_offset, bit_offset) = self.mark_offset(addr);

        let byte = self.mark_byte(byte_offset);
        let modified_byte = byte | (1 << bit_offset);
        self.set_mark_byte(byte_offset, modified_byte);
    }

    pub fn is_marked(&self, addr: Address) -> bool {
        let (byte_offset, bit_offset) = self.mark_offset(addr);
        let byte = self.mark_byte(byte_offset);

        byte & (1 << bit_offset) != 0
    }

    fn mark_offset(&self, addr: Address) -> (usize, u8) {
        debug_assert!(self.heap.contains(addr));
        let offset = addr.offset_from(self.heap.start) / mem::ptr_width_usize();
        let byte = offset / 8;
        let bit = offset % 8;

        (byte, bit as u8)
    }

    fn mark_byte(&self, offset: usize) -> u8 {
        let addr = self.bitmap.start.offset(offset);

        unsafe {
            *addr.to_ptr()
        }
    }

    fn set_mark_byte(&self, offset: usize, val: u8) {
        let addr = self.bitmap.start.offset(offset);

        unsafe {
            *addr.to_mut_ptr() = val;
        }
    }
}

impl Drop for MarkingBitmap {
    fn drop(&mut self) {
        os::munmap(self.bitmap.start.to_ptr(), self.bitmap.size());
    }
}