use alloc::heap;
use std::{self, ptr, slice};

use class::Class;
use mem;

#[derive(Debug)]
pub struct VTable<'ast> {
    pub classptr: *mut Class<'ast>,
    pub table_length: usize,
    pub table: [usize; 1],
}

impl<'ast> VTable<'ast> {
    pub fn from_table(classptr: *mut Class<'ast>, entries: &[usize]) -> Box<VTable<'ast>> {
        let size = std::mem::size_of::<VTable>()
            + entries.len() * std::mem::size_of::<usize>();

        unsafe {
            let ptr = heap::allocate(size, mem::ptr_width() as usize) as *mut VTable<'ast>;

            let mut vtable = Box::from_raw(ptr);
            vtable.classptr = classptr;
            vtable.table_length = entries.len();

            ptr::copy(entries.as_ptr(), &mut vtable.table[0], entries.len());

            vtable
        }
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
}
