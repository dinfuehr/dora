use alloc::heap;
use std::mem::{align_of, size_of};
use std::ops::{Deref, DerefMut};
use std::{self, fmt, ptr, slice};

use class::Class;
use mem;

pub const DISPLAY_SIZE: usize = 6;

pub struct VTableBox<'ast>(*mut VTable <'ast>);

impl<'ast> VTableBox<'ast> {
    pub fn new(classptr: *mut Class<'ast>, entries: &[usize]) -> VTableBox<'ast> {
        let size = VTable::size_of(entries.len());
        let vtable = VTable {
            classptr: classptr,
            subtype_depth: 0,
            subtype_offset: 0,
            subtype_display: [ptr::null(); DISPLAY_SIZE+1],
            subtype_overflow: ptr::null(),
            table_length: entries.len(),
            table: [0],
        };

        unsafe {
            let ptr = heap::allocate(size, align_of::<VTable<'ast>>()) as *mut VTable<'ast>;
            ptr::write(ptr, vtable);

            ptr::copy(entries.as_ptr(), &mut (&mut *ptr).table[0], entries.len());

            VTableBox(ptr)
        }
    }
}

impl<'ast> fmt::Debug for VTableBox<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vtable = self.deref();

        vtable.fmt(f)
    }
}

impl<'ast> Deref for VTableBox<'ast> {
    type Target = VTable<'ast>;

    fn deref(&self) -> &VTable<'ast> {
        unsafe { &*self.0 }
    }
}

impl<'ast> DerefMut for VTableBox<'ast> {
    fn deref_mut(&mut self) -> &mut VTable<'ast> {
        unsafe { &mut *self.0 }
    }
}

impl<'ast> Drop for VTableBox<'ast> {
    fn drop(&mut self) {
        unsafe {
            let len = (&*self.0).table_length;
            ptr::drop_in_place(self.0);

            heap::deallocate(self.0 as *mut u8, VTable::size_of(len),
                             align_of::<VTable<'ast>>());
        }
    }
}

#[derive(Debug)]
pub struct VTable<'ast> {
    pub classptr: *mut Class<'ast>,
    pub subtype_depth: i32,
    pub subtype_offset: i32,
    pub subtype_display: [*const VTable<'ast>; DISPLAY_SIZE+1],
    pub subtype_overflow: *const VTable<'ast>,
    pub table_length: usize,
    pub table: [usize; 1],
}

impl<'ast> VTable<'ast> {
    pub fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<VTable>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn classptr(&self) -> *mut Class<'ast> {
        self.classptr
    }

    pub fn class(&self) -> &mut Class<'ast> {
        unsafe { &mut *self.classptr }
    }

    pub fn table(&self) -> &[usize] {
        let ptr: *const usize = self.table.as_ptr();

        unsafe {
            slice::from_raw_parts(ptr, self.table_length)
        }
    }

    pub fn table_mut(&self) -> &mut [usize] {
        let ptr = self.table.as_ptr() as *mut usize;

        unsafe {
            slice::from_raw_parts_mut(ptr, self.table_length)
        }
    }

    pub fn offset_of_depth() -> i32 {
        mem::ptr_width()
    }

    pub fn offset_of_display() -> i32 {
        mem::ptr_width() * 2
    }

    pub fn offset_of_table() -> i32 {
        (4 + DISPLAY_SIZE as i32 + 1) * mem::ptr_width()
    }

    pub fn offset_of_overflow() -> i32 {
        (2 + DISPLAY_SIZE as i32 + 1) * mem::ptr_width()
    }

    pub fn get_subtype_overflow(&self, ind: usize) -> *const VTable<'ast> {
        assert!(self.subtype_depth as usize >= DISPLAY_SIZE &&
                ind < self.subtype_depth as usize - DISPLAY_SIZE + 1);

        unsafe {
            let ptr = self.subtype_overflow.offset(ind as isize) as *mut _;

            *ptr
        }
    }

    pub fn allocate_overflow(&mut self, num: usize) {
        assert!(self.subtype_overflow.is_null());

        unsafe {
            self.subtype_overflow = heap::allocate(num * size_of::<*const VTable<'ast>>(),
                                                   align_of::<*const VTable<'ast>>()) as *mut VTable<'ast>;
        }
    }

    pub fn deallocate_overflow(&mut self, num: usize) {
        unsafe {
            heap::deallocate(self.subtype_overflow as *const u8 as *mut u8,
                             num * size_of::<*const VTable<'ast>>(),
                             align_of::<*const VTable<'ast>>());
        }
    }
}

impl<'ast> Drop for VTable<'ast> {
    fn drop(&mut self) {
        if !self.subtype_overflow.is_null() {
            let elems = self.subtype_depth as usize - DISPLAY_SIZE + 1;
            self.deallocate_overflow(elems);
        }
    }
}
