use std::alloc::{alloc, dealloc, Layout};

use std::mem::{align_of, size_of};
use std::ops::{Deref, DerefMut};
use std::{self, fmt, ptr, slice};

use crate::size::InstanceSize;
use crate::vm::{ClassInstance, ClassInstanceId, VM};

pub const DISPLAY_SIZE: usize = 6;

pub struct VTableBox(*mut VTable);

impl VTableBox {
    pub fn new(
        classptr: *const ClassInstance,
        instance_size: usize,
        element_size: usize,
        entries: &[usize],
    ) -> VTableBox {
        let size = VTable::size_of(entries.len());
        let vtable = VTable {
            classptr,
            instance_size,
            element_size,
            subtype_depth: 0,
            subtype_display: [ptr::null(); DISPLAY_SIZE],
            subtype_overflow: ptr::null(),
            table_length: entries.len(),
            table: [0],
        };

        unsafe {
            let lay = Layout::from_size_align(size, align_of::<VTable>()).unwrap();
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
    pub classptr: *const ClassInstance,
    pub instance_size: usize,
    pub element_size: usize,
    pub subtype_depth: usize,
    pub subtype_display: [*const VTable; DISPLAY_SIZE],
    pub subtype_overflow: *const *const VTable,
    pub table_length: usize,
    pub table: [usize; 1],
}

impl VTable {
    pub fn size_of(table_length: usize) -> usize {
        std::mem::size_of::<VTable>() + table_length * std::mem::size_of::<usize>()
    }

    pub fn initialize_class_instance(&mut self, classptr: *const ClassInstance) {
        self.classptr = classptr;
    }

    pub fn class_instance(&self) -> &ClassInstance {
        unsafe { &*self.classptr }
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

        let lay = Layout::from_size_align(size, align).unwrap();

        unsafe {
            self.subtype_overflow = alloc(lay) as *const _;
        }
    }

    pub fn deallocate_overflow(&mut self, num: usize) {
        assert!(!self.subtype_overflow.is_null());
        let lay = Layout::from_size_align(
            num * size_of::<*const VTable>(),
            align_of::<*const VTable>(),
        )
        .unwrap();

        unsafe {
            dealloc(self.subtype_overflow as *mut _, lay);
        }
    }

    pub fn is_array_ref(&self) -> bool {
        let cls = self.class_instance();

        match cls.size {
            InstanceSize::ObjArray => true,
            _ => false,
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

pub fn ensure_display(
    vm: &VM,
    vtable: &mut VTableBox,
    parent_id: Option<ClassInstanceId>,
) -> usize {
    // if subtype_display[0] is set, vtable was already initialized
    assert!(vtable.subtype_display[0].is_null());

    if let Some(parent_id) = parent_id {
        let parent = vm.class_instances.idx(parent_id);

        let parent_vtable = parent.vtable.read();
        let parent_vtable = parent_vtable.as_ref().unwrap();
        assert!(!parent_vtable.subtype_display[0].is_null());

        let depth = 1 + parent_vtable.subtype_depth;

        let depth_fixed;

        if depth >= DISPLAY_SIZE {
            depth_fixed = DISPLAY_SIZE;

            vtable.allocate_overflow(depth as usize - DISPLAY_SIZE + 1);

            unsafe {
                if depth > DISPLAY_SIZE {
                    ptr::copy_nonoverlapping(
                        parent_vtable.subtype_overflow,
                        vtable.subtype_overflow as *mut _,
                        depth as usize - DISPLAY_SIZE,
                    );
                }

                let ptr = vtable
                    .subtype_overflow
                    .offset(depth as isize - DISPLAY_SIZE as isize)
                    as *mut _;

                *ptr = &**vtable as *const _;
            }
        } else {
            depth_fixed = depth;

            vtable.subtype_display[depth] = &**vtable as *const _;
        }

        vtable.subtype_depth = depth;
        vtable.subtype_display[0..depth_fixed]
            .clone_from_slice(&parent_vtable.subtype_display[0..depth_fixed]);

        depth
    } else {
        vtable.subtype_depth = 0;
        vtable.subtype_display[0] = &**vtable as *const _;

        0
    }
}
