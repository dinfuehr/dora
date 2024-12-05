use std::mem::align_of;
use std::{self, ptr, slice};

use crate::gc::Address;
use crate::vm::VM;
use crate::ShapeKind;

#[derive(Debug)]
#[repr(u8)]
pub enum ShapeVisitor {
    Regular,
    PointerArray,
    RecordArray,
    None,
    Invalid,
}

#[derive(Debug)]
#[repr(C)]
pub struct VTable {
    pub shape_kind: ShapeKind,
    pub visitor: ShapeVisitor,
    pub refs: Vec<i32>,
    pub instance_size: usize,
    pub element_size: usize,
    pub table_length: usize,
}

impl VTable {
    pub fn new(
        vm: &VM,
        shape_kind: ShapeKind,
        visitor: ShapeVisitor,
        refs: Vec<i32>,
        instance_size: usize,
        element_size: usize,
        entries: &[usize],
    ) -> *const VTable {
        let size = VTable::size_of(entries.len());

        let vtable = VTable {
            shape_kind,
            visitor,
            refs,
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
        }

        ptr
    }

    pub const fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<VTable>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn kind(&self) -> &ShapeKind {
        &self.shape_kind
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
