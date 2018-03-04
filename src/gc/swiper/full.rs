use std::collections::HashMap;

use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use gc::swiper::card::CardTable;
use gc::swiper::crossing::CrossingMap;
use gc::swiper::old::OldGen;
use gc::swiper::Region;
use gc::swiper::young::YoungGen;
use mem;
use object::Obj;
use os;

pub struct FullCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    heap: Region,
    young: &'a YoungGen,
    old: &'a OldGen,
    rootset: &'a [IndirectObj],
    card_table: &'a CardTable,
    crossing_map: &'a CrossingMap,

    marking_bitmap: MarkingBitmap,
    fwd_table: HashMap<Address, Address>,
}

impl<'a, 'ast> FullCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        card_table: &'a CardTable,
        crossing_map: &'a CrossingMap,
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

            marking_bitmap: marking_bitmap,
            fwd_table: HashMap::new(),
        }
    }

    pub fn collect(&mut self) {
        self.mark_live();
        self.compute_forward();
        self.update_references();
        self.relocate();
    }

    fn mark_live(&mut self) {
        unimplemented!();
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
    }

    fn update_references(&mut self) {
        let used_region = self.old.used_region();

        let mut scan = used_region.start;
        let end = used_region.end;

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let object_size = object.size();

            if self.is_marked(object) {
                object.visit_reference_fields(|field| {
                    let field_addr = Address::from_ptr(field.get());

                    if self.heap.contains(field_addr) {
                        let fwd_addr = self.fwd_table.get(&field_addr).expect("forwarding address not found.");
                        field.set(fwd_addr.to_mut_ptr());
                    }
                });
            }

            scan = scan.offset(object_size);
        }
    }

    fn relocate(&mut self) {
        let used_region = self.old.used_region();

        let mut scan = used_region.start;
        let end = used_region.end;

        while scan < end {
            let object = unsafe { &mut *scan.to_mut_ptr::<Obj>() };
            let object_size = object.size();

            if self.is_marked(object) {
                let &dest = self.fwd_table.get(&scan).expect("forwarding address not found.");
                object.copy_to(dest, object_size);
            }

            scan = scan.offset(object_size);
        }
    }

    fn is_marked(&self, obj: &Obj) -> bool {
        self.marking_bitmap.is_marked(Address::from_ptr(obj as *const _))
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
        debug_assert!(self.heap.contains(addr));
        let offset = addr.offset_from(self.heap.start) / mem::ptr_width_usize();
        let byte_offset = offset / 8;
        let bit_offset = offset % 8;

        let byte = self.mark_byte(byte_offset);
        let modified_byte = byte | (1 << bit_offset);
        self.set_mark_byte(byte_offset, modified_byte);
    }

    pub fn is_marked(&self, _addr: Address) -> bool {
        unimplemented!()
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