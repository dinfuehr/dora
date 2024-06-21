use std::mem::align_of;
use std::ops::{Deref, DerefMut};
use std::{self, fmt, ptr, slice};

use crate::gc::Address;
use crate::vm::{ClassInstance, VM};

pub struct VTableBox(*mut VTable);

impl VTableBox {
    pub fn new(
        vm: &VM,
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
        };

        let address = vm.gc.alloc_meta(size, align_of::<VTable>());
        assert!(address.is_non_null());

        let ptr = address.to_mut_ptr::<VTable>();

        unsafe {
            ptr::write(ptr, vtable);

            ptr::copy(
                entries.as_ptr(),
                (&*ptr).table_ptr() as *mut _,
                entries.len(),
            );

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

#[derive(Debug)]
#[repr(C)]
pub struct VTable {
    pub class_instance_ptr: *const ClassInstance,
    pub instance_size: usize,
    pub element_size: usize,
    pub table_length: usize,
}

impl VTable {
    pub const fn size_of(table_length: usize) -> usize {
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
        unsafe { slice::from_raw_parts(self.table_ptr(), self.table_length) }
    }

    pub fn table_mut(&self) -> &mut [usize] {
        unsafe { slice::from_raw_parts_mut(self.table_ptr() as *mut usize, self.table_length) }
    }

    pub fn table_ptr(&self) -> *const usize {
        let address = Address::from_ptr(self as *const _);
        address
            .offset(VTable::offset_of_method_table() as usize)
            .to_ptr()
    }

    pub fn offset_of_method_table() -> i32 {
        std::mem::size_of::<VTable>() as i32
    }
}
