use ctxt::SemContext;
use gc::{Address, Region};
use mem;
use object::Header;
use vtable::VTable;

pub const TLAB_SIZE: usize = 32 * 1024;
pub const TLAB_OBJECT_SIZE: usize = 8 * 1024;

pub fn initialize(ctxt: &SemContext, tlab: Region) {
    ctxt.tld.borrow_mut().tlab_initialize(tlab.start, tlab.end);
}

pub fn calculate_size(additional: usize) -> usize {
    assert!(additional < TLAB_OBJECT_SIZE);

    TLAB_SIZE + additional
}

pub fn allocate(ctxt: &SemContext, size: usize) -> Option<Address> {
    assert!(size < TLAB_OBJECT_SIZE);
    let tlab = ctxt.tld.borrow().tlab_region();

    if size <= tlab.size() {
        ctxt.tld
            .borrow_mut()
            .tlab_initialize(tlab.start.offset(size), tlab.end);
        Some(tlab.start)
    } else {
        None
    }
}

pub fn make_iterable(ctxt: &SemContext) {
    let tlab = ctxt.tld.borrow().tlab_region();
    make_iterable_region(ctxt, tlab.start, tlab.end);
}

pub fn make_iterable_region(ctxt: &SemContext, start: Address, end: Address) {
    if start == end {
        // nothing to do

    } else if end.offset_from(start) == mem::ptr_width_usize() {
        unsafe {
            *start.to_mut_ptr::<usize>() = 0;
        }
    } else if end.offset_from(start) == Header::size() as usize {
        // fill with object
        let cls_id = ctxt.vips.obj(ctxt);
        let cls = ctxt.class_defs[cls_id].borrow();
        let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable as usize;
        }
    } else {
        // fill with int array
        let cls_id = ctxt.vips.int_array(ctxt);
        let cls = ctxt.class_defs[cls_id].borrow();
        let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();

        // determine of header+length in bytes
        let header_size = Header::size() as usize + mem::ptr_width_usize();

        // calculate int array length
        let length: usize = end.offset_from(start.offset(header_size)) / 4;

        unsafe {
            *start.to_mut_ptr::<usize>() = vtable as usize;
            *start.offset(Header::size() as usize).to_mut_ptr::<usize>() = length;
        }
    }

    let n = Address::null();
    ctxt.tld.borrow_mut().tlab_initialize(n, n);
}
