use std::mem::align_of;
use std::{self, ptr, slice};

use crate::ShapeKind;
use crate::gc::Address;
use crate::vm::{FieldInstance, VM};

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
pub struct Shape {
    pub shape_kind: ShapeKind,
    pub visitor: ShapeVisitor,
    pub refs: Vec<i32>,
    pub fields: Vec<FieldInstance>,
    pub instance_size: usize,
    pub element_size: usize,
    pub vtable_length: usize,
}

impl Shape {
    pub fn new(
        vm: &VM,
        shape_kind: ShapeKind,
        visitor: ShapeVisitor,
        refs: Vec<i32>,
        fields: Vec<FieldInstance>,
        instance_size: usize,
        element_size: usize,
        vtable_entries: &[usize],
    ) -> *const Shape {
        let size = Shape::size_of(vtable_entries.len());

        let vtable = Shape {
            shape_kind,
            visitor,
            refs,
            fields,
            instance_size,
            element_size,
            vtable_length: vtable_entries.len(),
        };

        let address = vm.gc.alloc_meta(size, align_of::<Shape>());
        assert!(address.is_non_null());

        let ptr = address.to_mut_ptr::<Shape>();

        unsafe {
            ptr::write(ptr, vtable);

            ptr::copy(
                vtable_entries.as_ptr(),
                (&*ptr).table_ptr() as *mut _,
                vtable_entries.len(),
            );
        }

        ptr
    }

    pub const fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<Shape>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn address(&self) -> Address {
        Address::from_ptr(self as *const _)
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
        unsafe { slice::from_raw_parts(self.table_ptr(), self.vtable_length) }
    }

    pub fn set_method_table_entry(&self, idx: usize, value: Address) {
        self.table_mut()[idx] = value.to_usize();
    }

    fn table_mut(&self) -> &mut [usize] {
        unsafe { slice::from_raw_parts_mut(self.table_ptr() as *mut usize, self.vtable_length) }
    }

    fn table_ptr(&self) -> *const usize {
        let address = Address::from_ptr(self as *const _);
        address.offset(Shape::offset_of_vtable() as usize).to_ptr()
    }

    pub fn offset_of_vtable() -> i32 {
        std::mem::size_of::<Shape>() as i32
    }
}
