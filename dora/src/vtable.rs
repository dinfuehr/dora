use std::alloc::{alloc, dealloc, Layout};

use std::mem::align_of;
use std::ops::{Deref, DerefMut};
use std::{self, fmt, ptr, slice};

use crate::size::InstanceSize;
use crate::vm::ClassInstance;

pub struct VTableBox(*mut VTable);

impl VTableBox {
    pub fn new(
        class_instance_ptr: *const ClassInstance,
        instance_size: usize,
        element_size: usize,
        entries: &[usize],
    ) -> VTableBox {
        let size = VTable::size_of(entries.len());
        let vtable = VTable {
            class_instance_ptr,
            instance_size,
            element_size,
            table_length: entries.len(),
            table: [0],
        };

        let lay = Layout::from_size_align(size, align_of::<VTable>()).unwrap();
        unsafe {
            let ptr = alloc(lay) as *mut VTable;
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

            let lay = Layout::from_size_align(VTable::size_of(len), align_of::<VTable>()).unwrap();

            dealloc(self.0 as *mut _, lay);
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct VTable {
    pub class_instance_ptr: *const ClassInstance,
    pub instance_size: usize,
    pub element_size: usize,
    pub table_length: usize,
    pub table: [usize; 1],
}

impl VTable {
    pub fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<VTable>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn initialize_class_instance(&mut self, classptr: *const ClassInstance) {
        self.class_instance_ptr = classptr;
    }

    pub fn class_instance(&self) -> &ClassInstance {
        unsafe { &*self.class_instance_ptr }
    }

    pub fn instance_size(&self) -> usize {
        self.instance_size
    }

    pub fn element_size(&self) -> usize {
        self.element_size
    }

    pub fn table(&self) -> &[usize] {
        let ptr: *const usize = self.table.as_ptr();

        unsafe { slice::from_raw_parts(ptr, self.table_length) }
    }

    pub fn table_mut(&self) -> &mut [usize] {
        let ptr = self.table.as_ptr() as *mut usize;

        unsafe { slice::from_raw_parts_mut(ptr, self.table_length) }
    }

    pub fn offset_of_method_table() -> i32 {
        offset_of!(VTable, table) as i32
    }

    pub fn is_array_ref(&self) -> bool {
        let cls = self.class_instance();

        match cls.size {
            InstanceSize::ObjArray => true,
            _ => false,
        }
    }
}
