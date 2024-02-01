use alloc::alloc::{alloc, dealloc, Layout};
use std::mem::size_of;
use std::ptr::{self, NonNull};

use crate::gc::Address;
use crate::mem::ptr_width_usize;

extern crate alloc;

const SEGMENT_SIZE: usize = 1024;
const SEGMENT_ENTRY_CAPACITY: usize =
    (SEGMENT_SIZE - size_of::<SegmentHeader>()) / ptr_width_usize();

pub struct Worklist {
    head: *mut SegmentHeader,
    tail: *mut SegmentHeader,
}

impl Worklist {
    pub fn new() -> Worklist {
        Worklist {
            head: ptr::null_mut(),
            tail: ptr::null_mut(),
        }
    }

    pub fn append(&mut self, other: &mut Worklist) {
        if self.head.is_null() {
            self.head = other.head;
            self.tail = other.tail;
        } else {
            self.tail_mut().next = other.head;
            self.tail = other.tail;
        }

        other.head = ptr::null_mut();
        other.tail = ptr::null_mut();
    }

    fn push_segment(&mut self, segment: WorklistSegment) {
        let ptr = segment.ptr.as_ptr();
        std::mem::forget(segment);

        if self.head.is_null() {
            self.head = ptr;
            self.tail = ptr;
        } else {
            self.tail_mut().next = ptr;
            self.tail = ptr;
        }

        debug_assert!(self.tail().next.is_null());
    }

    fn pop_segment(&mut self) -> Option<WorklistSegment> {
        if self.head.is_null() {
            None
        } else {
            let result = self.head;
            let new_head = self.head().next;

            self.head = new_head;
            if new_head.is_null() {
                self.tail = ptr::null_mut();
            }

            unsafe {
                (*result).next = ptr::null_mut();
            }

            Some(WorklistSegment {
                ptr: unsafe { NonNull::new_unchecked(result) },
            })
        }
    }

    pub fn clear(&mut self) {
        let mut seg = self.head;
        let layout = layout_segment();

        while !seg.is_null() {
            unsafe {
                let next_seg = (*seg).next;
                dealloc(next_seg as *mut _, layout);
                seg = next_seg;
            }
        }

        self.head = ptr::null_mut();
        self.tail = ptr::null_mut();
    }

    fn head(&self) -> &SegmentHeader {
        unsafe { &*self.head }
    }

    fn head_mut(&mut self) -> &mut SegmentHeader {
        unsafe { &mut *self.head }
    }

    fn tail(&self) -> &SegmentHeader {
        unsafe { &*self.tail }
    }

    fn tail_mut(&mut self) -> &mut SegmentHeader {
        unsafe { &mut *self.tail }
    }
}

impl Drop for Worklist {
    fn drop(&mut self) {
        self.clear();
    }
}

pub struct WorklistSegment {
    ptr: NonNull<SegmentHeader>,
}

impl WorklistSegment {
    pub fn new() -> WorklistSegment {
        WorklistSegment {
            ptr: alloc_segment(),
        }
    }

    pub fn push(&mut self, address: Address) -> bool {
        unsafe { self.ptr.as_mut().push(address) }
    }

    pub fn pop(&mut self) -> Option<Address> {
        unsafe { self.ptr.as_mut().pop() }
    }

    pub fn len(&self) -> usize {
        unsafe { self.ptr.as_ref().len() }
    }
}

#[repr(C)]
struct SegmentHeader {
    next: *mut SegmentHeader,
    len: usize,
}

impl SegmentHeader {
    fn len(&self) -> usize {
        self.len
    }

    fn push(&mut self, address: Address) -> bool {
        let old_len = self.len();
        if self.len() < SEGMENT_ENTRY_CAPACITY {
            unsafe {
                ptr::write(self.data_raw_mut().add(old_len), address);
                self.len = old_len + 1;
            }
            true
        } else {
            false
        }
    }

    fn pop(&mut self) -> Option<Address> {
        let len = self.len();
        if len > 0 {
            self.len = len - 1;
            let value = unsafe { ptr::read(self.data_raw_mut().add(len - 1)) };
            Some(value)
        } else {
            None
        }
    }

    fn data_raw_mut(&mut self) -> *mut Address {
        unsafe { (self as *mut _ as *mut u8).add(size_of::<SegmentHeader>()) as *mut _ }
    }

    fn limit_raw_mut(&mut self) -> *mut Address {
        unsafe { (self as *mut _ as *mut u8).add(SEGMENT_SIZE) as *mut _ }
    }
}

impl Drop for WorklistSegment {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.ptr.as_ptr() as *mut _, layout_segment());
        }
    }
}

fn alloc_segment() -> NonNull<SegmentHeader> {
    unsafe {
        let header = alloc(layout_segment()) as *mut SegmentHeader;

        if header.is_null() {
            panic!("native heap allocation failed");
        }

        (*header).next = ptr::null_mut();
        (*header).len = 0;

        NonNull::new_unchecked(header)
    }
}

fn layout_segment() -> Layout {
    Layout::from_size_align(SEGMENT_SIZE, ptr_width_usize()).expect("broken layout")
}

unsafe impl Sync for Worklist {}
unsafe impl Send for Worklist {}

#[cfg(test)]
mod tests {
    use crate::gc::Address;

    use super::{Worklist, WorklistSegment, SEGMENT_ENTRY_CAPACITY};

    #[test]
    fn segments_in_worklist() {
        let mut worklist = Worklist::new();
        const SEGMENTS: usize = 10;

        for size in 0..SEGMENTS {
            let mut seg = WorklistSegment::new();
            for _ in 0..size {
                seg.push(Address::null());
            }
            assert_eq!(seg.len(), size);
            worklist.push_segment(seg);
        }

        let mut popped = 0;

        while let Some(seg) = worklist.pop_segment() {
            assert_eq!(seg.len(), popped);
            popped += 1;
        }

        assert_eq!(popped, SEGMENTS);
    }

    #[test]
    fn push_into_segment() {
        let mut seg = WorklistSegment::new();
        for idx in 0..SEGMENT_ENTRY_CAPACITY {
            assert_eq!(seg.len(), idx);
            assert!(seg.push(Address::null()));
        }
        assert_eq!(seg.len(), SEGMENT_ENTRY_CAPACITY);
        assert!(!seg.push(Address::null()));
    }

    #[test]
    fn pop_from_segment() {
        let mut seg = WorklistSegment::new();
        assert!(seg.push(1.into()));
        assert!(seg.push(2.into()));
        assert_eq!(seg.pop().expect("missing").to_usize(), 2);
        assert_eq!(seg.pop().expect("missing").to_usize(), 1);
        assert!(seg.pop().is_none());
    }
}
