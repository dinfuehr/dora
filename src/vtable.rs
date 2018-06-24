use alloc::alloc::{Global, Layout};
use alloc::allocator::Alloc;

use std::mem::{align_of, size_of};
use std::ops::{Deref, DerefMut};
use std::{self, fmt, ptr, slice};
use std::ptr::NonNull;

use class::ClassDef;

pub const DISPLAY_SIZE: usize = 6;

pub struct VTableBox(*mut VTable);

impl VTableBox {
    pub fn new(classptr: *mut ClassDef, entries: &[usize]) -> VTableBox {
        let size = VTable::size_of(entries.len());
        let vtable = VTable {
            classptr: classptr,
            subtype_depth: 0,
            subtype_display: [ptr::null(); DISPLAY_SIZE],
            subtype_overflow: ptr::null(),
            table_length: entries.len(),
            table: [0],
        };

        let mut heap: Global = Default::default();

        unsafe {
            let lay = Layout::from_size_align(size, align_of::<VTable>()).unwrap();
            let ptr = heap.alloc(lay).expect("could not allocate").as_ptr() as *mut VTable;
            ptr::write(ptr, vtable);

            ptr::copy(entries.as_ptr(), &mut (&mut *ptr).table[0], entries.len());

            VTableBox(ptr)
        }
    }
}

impl fmt::Debug for VTableBox {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vtable = self.deref();

        vtable.fmt(f)
    }
}

impl Deref for VTableBox {
    type Target = VTable;

    fn deref(&self) -> &VTable {
        unsafe { &*self.0 }
    }
}

impl DerefMut for VTableBox {
    fn deref_mut(&mut self) -> &mut VTable {
        unsafe { &mut *self.0 }
    }
}

impl Drop for VTableBox {
    fn drop(&mut self) {
        unsafe {
            let len = (&*self.0).table_length;
            ptr::drop_in_place(self.0);

            let mut heap: Global = Default::default();
            let lay = Layout::from_size_align(VTable::size_of(len), align_of::<VTable>()).unwrap();

            let ptr = NonNull::new_unchecked(self.0 as *mut u8);
            heap.dealloc(ptr, lay);
        }
    }
}

#[derive(Debug)]
pub struct VTable {
    pub classptr: *mut ClassDef,
    pub subtype_depth: i32,
    pub subtype_display: [*const VTable; DISPLAY_SIZE],
    pub subtype_overflow: *const *const VTable,
    pub table_length: usize,
    pub table: [usize; 1],
}

impl VTable {
    pub fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<VTable>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn classptr(&self) -> *mut ClassDef {
        self.classptr
    }

    pub fn class(&self) -> &mut ClassDef {
        unsafe { &mut *self.classptr }
    }

    pub fn table(&self) -> &[usize] {
        let ptr: *const usize = self.table.as_ptr();

        unsafe { slice::from_raw_parts(ptr, self.table_length) }
    }

    pub fn table_mut(&self) -> &mut [usize] {
        let ptr = self.table.as_ptr() as *mut usize;

        unsafe { slice::from_raw_parts_mut(ptr, self.table_length) }
    }

    pub fn offset_of_depth() -> i32 {
        offset_of!(VTable, subtype_depth) as i32
    }

    pub fn offset_of_display() -> i32 {
        offset_of!(VTable, subtype_display) as i32
    }

    pub fn offset_of_method_table() -> i32 {
        offset_of!(VTable, table) as i32
    }

    pub fn offset_of_overflow() -> i32 {
        offset_of!(VTable, subtype_overflow) as i32
    }

    pub fn get_subtype_overflow(&self, ind: usize) -> *const VTable {
        assert!(
            self.subtype_depth as usize >= DISPLAY_SIZE
                && ind < self.subtype_depth as usize - DISPLAY_SIZE + 1
        );

        unsafe {
            let ptr = self.subtype_overflow.offset(ind as isize) as *mut _;

            *ptr
        }
    }

    pub fn allocate_overflow(&mut self, num: usize) {
        assert!(self.subtype_overflow.is_null());

        let size = num * size_of::<*const VTable>();
        let align = align_of::<*const VTable>();

        let mut heap: Global = Default::default();
        let lay = Layout::from_size_align(size, align).unwrap();

        unsafe {
            self.subtype_overflow = heap.alloc(lay).expect("could not allocate").as_ptr() as *const _;
        }
    }

    pub fn deallocate_overflow(&mut self, num: usize) {
        assert!(!self.subtype_overflow.is_null());
        let mut heap: Global = Default::default();
        let lay = Layout::from_size_align(
            num * size_of::<*const VTable>(),
            align_of::<*const VTable>(),
        ).unwrap();

        unsafe {
            let ptr = NonNull::new_unchecked(self.subtype_overflow as *const _ as *mut u8);
            heap.dealloc(ptr, lay);
        }
    }
}

impl Drop for VTable {
    fn drop(&mut self) {
        if !self.subtype_overflow.is_null() {
            let elems = self.subtype_depth as usize - DISPLAY_SIZE + 1;
            self.deallocate_overflow(elems);
        }
    }
}
