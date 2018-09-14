use std::collections::HashMap;
use std::sync::atomic::Ordering;

use ctxt::SemContext;
use gc::root::IndirectObj;
use gc::space::Space;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::large::LargeSpace;
use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;
use gc::swiper::Region;
use gc::swiper::CARD_SIZE;
use gc::swiper::{formatted_size, on_different_cards};
use gc::Address;
use mem;
use object::{offset_of_array_data, Obj};
use os;
use timer::{in_ms, Timer};

pub struct FullCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    large_space: &'a LargeSpace,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,
    perm_space: &'a Space,

    marking_bitmap: MarkingBitmap,
    fwd_table: ForwardTable,

    fwd: Address,
    fwd_end: Address,
    old_top: Address,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        large_space: &'a LargeSpace,
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
            large_space: large_space,
            rootset: rootset,
            card_table: card_table,
            crossing_map: crossing_map,
            perm_space: perm_space,

            marking_bitmap: marking_bitmap,
            fwd_table: ForwardTable::new(),

            fwd: old.total.start,
            fwd_end: old.total.end,
            old_top: Address::null(),
        }
    }

    pub fn collect(&mut self) {
        let active = self.ctxt.args.flag_gc_verbose;
        let mut timer = Timer::new(active);
        let init_size = self.heap_size();

        let time_mark = Timer::ms(active, || {
            self.mark_live();
        });

        let time_forward = Timer::ms(active, || {
            self.compute_forward();
        });

        let time_updateref = Timer::ms(active, || {
            self.update_references();
        });

        let time_relocate = Timer::ms(active, || {
            self.relocate();
        });

        let time_large = Timer::ms(active, || {
            self.update_large_objects();
        });

        timer.stop_with(|dur| {
            let new_size = self.heap_size();
            let garbage = init_size - new_size;
            let garbage_ratio = if init_size == 0 {
                0f64
            } else {
                (garbage as f64 / init_size as f64) * 100f64
            };

            println!(
                "GC: Full GC ({:.2} ms, {}->{} size, {}/{:.0}% garbage); \
                 mark={:.1}ms forward={:.1}ms updateref={:.1}ms relocate={:.1}ms large={:.1}ms",
                in_ms(dur),
                formatted_size(init_size),
                formatted_size(new_size),
                formatted_size(garbage),
                garbage_ratio,
                time_mark,
                time_forward,
                time_updateref,
                time_relocate,
                time_large,
            );
        });
    }

    fn heap_size(&self) -> usize {
        self.young.used_region().size() + self.old.used_region().size()
    }

    fn mark_live(&mut self) {
        let mut marking_stack: Vec<Address> = Vec::new();
        let mut live_objects = 0;

        for root in self.rootset {
            let root_ptr = Address::from_ptr(root.get());

            if self.heap.contains(root_ptr) {
                if !self.is_marked_addr(root_ptr) {
                    marking_stack.push(root_ptr);
                    self.mark(root_ptr);
                    live_objects += 1;
                }
            } else {
                debug_assert!(root_ptr.is_null() || self.perm_space.contains(root_ptr));
            }
        }

        while marking_stack.len() > 0 {
            let object_addr = marking_stack.pop().expect("stack already empty");
            let object = unsafe { &mut *object_addr.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|field| {
                let field_addr = Address::from_ptr(field.get());

                if self.heap.contains(field_addr) {
                    if !self.is_marked_addr(field_addr) {
                        marking_stack.push(field_addr);
                        self.mark(field_addr);
                        live_objects += 1;
                    }
                } else {
                    debug_assert!(field_addr.is_null() || self.perm_space.contains(field_addr));
                }
            });
        }

        self.fwd_table.reserve_capacity(live_objects);
    }

    fn compute_forward(&mut self) {
        self.walk_old_and_young(|full, object, address, object_size| {
            if full.is_marked(object) {
                let fwd = full.allocate(object_size);
                full.fwd_table.forward_to(address, fwd);
            }
        });
    }

    fn update_references(&mut self) {
        self.walk_old_and_young(|full, object, _address, _| {
            if full.is_marked(object) {
                object.visit_reference_fields(|field| {
                    full.forward_reference(field);
                });
            }
        });

        for root in self.rootset {
            let root_ptr = Address::from_ptr(root.get());

            if !root_ptr.is_null()
                && !self.perm_space.contains(root_ptr)
                && !self.large_space.contains(root_ptr)
            {
                let fwd_addr = self.fwd_table.forward_address(root_ptr);
                root.set(fwd_addr.to_mut_ptr());
            }
        }
    }

    fn relocate(&mut self) {
        self.young.unprotect_to_space();

        self.crossing_map.set_first_object(0.into(), 0);
        let mut last_dest = Address::null();

        self.walk_old_and_young(|full, object, address, object_size| {
            if full.is_marked(object) {
                let dest = full.fwd_table.forward_address(address);
                object.copy_to(dest, object_size);

                let next_dest = dest.offset(object_size);

                if on_different_cards(dest, next_dest) && full.old.contains(dest) {
                    full.update_crossing(dest, next_dest, object.is_array_ref());
                }

                last_dest = next_dest;
            }
        });

        let young_top;
        let old_top;

        // check if we have left the old-generation into
        // the young generation for copying objects
        if self.old_top.is_null() {
            // if not, young gen is empty
            young_top = self.young.to_space().start;
            old_top = self.fwd;
        } else {
            young_top = self.fwd;
            old_top = self.old_top;
        }

        debug_assert!(self.young.valid_top(young_top));
        self.young.swap_spaces(young_top);
        self.young.protect_to_space();

        debug_assert!(self.old.valid_top(old_top));
        self.old.free.store(old_top.to_usize(), Ordering::SeqCst);
    }

    fn update_large_objects(&mut self) {
        self.large_space.remove_objects(|addr| {
            if !self.is_marked_addr(addr) {
                // free object
                return false;
            }

            let object = unsafe { &mut *addr.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|field| {
                self.forward_reference(field);
            });

            // keep object
            true
        });
    }

    fn forward_reference(&mut self, indirect_obj: IndirectObj) {
        let object_addr = Address::from_ptr(indirect_obj.get());

        if !object_addr.is_null()
            && !self.perm_space.contains(object_addr)
            && !self.large_space.contains(object_addr)
        {
            let fwd_addr = self.fwd_table.forward_address(object_addr);
            indirect_obj.set(fwd_addr.to_mut_ptr());
        }
    }

    fn walk_old_and_young<F>(&mut self, mut fct: F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
    {
        let used_region = self.old.used_region();
        self.walk_region(used_region.start, used_region.end, &mut fct);

        let used_region = self.young.used_region();
        self.walk_region(used_region.start, used_region.end, &mut fct);
    }

    fn walk_region<F>(&mut self, start: Address, end: Address, fct: &mut F)
    where
        F: FnMut(&mut FullCollector, &mut Obj, Address, usize),
    {
        let mut scan = start;

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let object_size = object.size();

            fct(self, object, scan, object_size);

            scan = scan.offset(object_size);
        }
    }

    fn allocate(&mut self, object_size: usize) -> Address {
        let addr = self.fwd;
        let next = self.fwd.offset(object_size);

        if next <= self.fwd_end {
            self.fwd = next;
            return addr;
        }

        panic!("FAIL: Not enough space for objects in old generation.");
    }

    fn update_crossing(&mut self, last: Address, addr: Address, array_ref: bool) {
        let last_card = self.card_table.card_idx(last);

        let offset = addr.to_usize() & (CARD_SIZE - 1);
        let offset_words = offset / mem::ptr_width_usize();

        let card = self.card_table.card_idx(addr);

        if array_ref {
            let last_card_end = self.card_table.to_address(last_card).offset(CARD_SIZE);
            let loop_start;

            if last.offset(offset_of_array_data() as usize) > last_card_end {
                let diff_words = last_card_end.offset_from(last) / mem::ptr_width_usize();
                self.crossing_map
                    .set_array_start((last_card.to_usize() + 1).into(), diff_words);

                loop_start = last_card.to_usize() + 2;
            } else {
                loop_start = last_card.to_usize() + 1;
            }

            let refs_per_card = CARD_SIZE / mem::ptr_width_usize();

            for i in loop_start..card.to_usize() {
                self.crossing_map
                    .set_references_at_start(i.into(), refs_per_card);
            }

            if card.to_usize() >= loop_start {
                self.crossing_map
                    .set_references_at_start(card, offset_words);
            }
        } else {
            for i in last_card.to_usize() + 1..card.to_usize() {
                self.crossing_map.set_no_references(i.into());
            }

            self.crossing_map.set_first_object(card, offset_words);
        }
    }

    fn is_marked(&self, obj: &Obj) -> bool {
        self.marking_bitmap
            .is_marked(Address::from_ptr(obj as *const _))
    }

    fn is_marked_addr(&self, addr: Address) -> bool {
        self.marking_bitmap.is_marked(addr)
    }

    fn mark(&mut self, addr: Address) {
        self.marking_bitmap.mark(addr);
    }
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

        unsafe { *addr.to_ptr() }
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

struct ForwardTable {
    data: HashMap<Address, Address>,
}

impl ForwardTable {
    fn new() -> ForwardTable {
        ForwardTable {
            data: HashMap::new(),
        }
    }

    fn reserve_capacity(&mut self, count: usize) {
        self.data.reserve(count);
    }

    fn forward_to(&mut self, addr: Address, fwd: Address) {
        self.data.insert(addr, fwd);
    }

    fn forward_address(&mut self, addr: Address) -> Address {
        *self.data.get(&addr).expect("no forward address found.")
    }
}
