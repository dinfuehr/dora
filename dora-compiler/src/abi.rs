use std::mem::offset_of;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize};

use crate::layout::{array_header_size, object_header_size};

pub const CODE_ALIGNMENT: usize = 16;
pub const REMEMBERED_BIT_SHIFT: usize = 33;

pub const K: usize = 1024;
pub const LARGE_OBJECT_SIZE: usize = 32 * K;
pub const MAX_TLAB_OBJECT_SIZE: usize = 8 * K;

pub const GLOBAL_UNINITIALIZED: u8 = 0;
pub const GLOBAL_RUNNING: u8 = 1;
pub const GLOBAL_INITIALIZED: u8 = 2;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Trap {
    DIV0,
    ASSERT,
    INDEX_OUT_OF_BOUNDS,
    NIL,
    CAST,
    OOM,
    STACK_OVERFLOW,
    ILLEGAL,
    OVERFLOW,
    SHIFT,
}

impl TryFrom<u8> for Trap {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Trap::DIV0),
            1 => Ok(Trap::ASSERT),
            2 => Ok(Trap::INDEX_OUT_OF_BOUNDS),
            3 => Ok(Trap::NIL),
            4 => Ok(Trap::CAST),
            5 => Ok(Trap::OOM),
            6 => Ok(Trap::STACK_OVERFLOW),
            7 => Ok(Trap::ILLEGAL),
            8 => Ok(Trap::OVERFLOW),
            9 => Ok(Trap::SHIFT),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ThreadState {
    Running = 0,
    Parked = 1,
    SafepointRequested = 2,
    ParkedSafepointRequested = 3,
    Safepoint = 4,
}

impl From<u8> for ThreadState {
    fn from(value: u8) -> ThreadState {
        match value {
            0 => ThreadState::Running,
            1 => ThreadState::Parked,
            2 => ThreadState::SafepointRequested,
            3 => ThreadState::ParkedSafepointRequested,
            4 => ThreadState::Safepoint,
            _ => unreachable!(),
        }
    }
}

impl ThreadState {
    pub fn is_running(&self) -> bool {
        match *self {
            ThreadState::Running | ThreadState::SafepointRequested => true,
            _ => false,
        }
    }

    pub fn is_parked(&self) -> bool {
        match *self {
            ThreadState::Parked | ThreadState::ParkedSafepointRequested => true,
            _ => false,
        }
    }

    pub fn to_usize(&self) -> usize {
        *self as usize
    }
}

impl Default for ThreadState {
    fn default() -> ThreadState {
        ThreadState::Running
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    pub fn from(val: usize) -> Address {
        Address(val)
    }

    #[inline(always)]
    pub fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn null() -> Address {
        Address(0)
    }

    #[inline(always)]
    pub fn is_null(self) -> bool {
        self.0 == 0
    }
}

impl From<usize> for Address {
    fn from(val: usize) -> Address {
        Address(val)
    }
}

pub struct Header;

impl Header {
    #[inline(always)]
    pub const fn size() -> i32 {
        object_header_size()
    }

    #[inline(always)]
    pub const fn array_size() -> i32 {
        array_header_size()
    }

    #[inline(always)]
    pub const fn offset_shape_word() -> usize {
        0
    }

    #[inline(always)]
    pub const fn offset_metadata_word() -> usize {
        4
    }
}

// ABI mirror of dora_runtime::shape::Shape and the descriptor records emitted
// by assembly.rs::write_shape_metadata. Keep all three layouts in sync.
#[repr(C)]
struct ShapeLayout {
    visitor: usize,
    refs_data: *const i32,
    refs_len: usize,
    instance_size: usize,
    element_size: usize,
    vtable_length: usize,
    kind_data: *const u8,
    kind_len: usize,
    fields_data: *const u8,
    fields_len: usize,
}

pub struct Shape;

impl Shape {
    #[inline(always)]
    pub const fn offset_of_vtable() -> i32 {
        std::mem::size_of::<ShapeLayout>() as i32
    }

    #[inline(always)]
    pub const fn offset_of_instance_size() -> usize {
        offset_of!(ShapeLayout, instance_size)
    }

    #[inline(always)]
    pub const fn offset_of_element_size() -> usize {
        offset_of!(ShapeLayout, element_size)
    }

    #[inline(always)]
    pub const fn offset_of_refs_data() -> usize {
        offset_of!(ShapeLayout, refs_data)
    }

    #[inline(always)]
    pub const fn offset_of_refs_len() -> usize {
        offset_of!(ShapeLayout, refs_len)
    }

    #[inline(always)]
    pub const fn offset_of_visitor() -> usize {
        offset_of!(ShapeLayout, visitor)
    }

    #[inline(always)]
    pub const fn offset_of_vtable_length() -> usize {
        offset_of!(ShapeLayout, vtable_length)
    }

    #[inline(always)]
    pub const fn offset_of_kind_data() -> usize {
        offset_of!(ShapeLayout, kind_data)
    }

    #[inline(always)]
    pub const fn offset_of_kind_len() -> usize {
        offset_of!(ShapeLayout, kind_len)
    }

    #[inline(always)]
    pub const fn offset_of_fields_data() -> usize {
        offset_of!(ShapeLayout, fields_data)
    }

    #[inline(always)]
    pub const fn offset_of_fields_len() -> usize {
        offset_of!(ShapeLayout, fields_len)
    }
}

#[repr(C)]
pub struct DoraToNativeInfo {
    pub last: *const DoraToNativeInfo,
    pub fp: usize,
    pub pc: usize,
}

impl DoraToNativeInfo {
    pub fn last_offset() -> i32 {
        offset_of!(DoraToNativeInfo, last) as i32
    }

    pub fn fp_offset() -> i32 {
        offset_of!(DoraToNativeInfo, fp) as i32
    }

    pub fn pc_offset() -> i32 {
        offset_of!(DoraToNativeInfo, pc) as i32
    }
}

#[repr(C)]
struct ThreadLocalDataLayout {
    tlab_top: AtomicUsize,
    tlab_end: AtomicUsize,
    stack_limit: AtomicUsize,
    dtn: AtomicUsize,
    managed_thread_handle: AtomicUsize,
    concurrent_marking: AtomicBool,
    state: AtomicU8,
    shape_base: usize,
}

pub struct ThreadLocalData;

impl ThreadLocalData {
    pub fn tlab_top_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, tlab_top) as i32
    }

    pub fn tlab_end_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, tlab_end) as i32
    }

    pub fn concurrent_marking_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, concurrent_marking) as i32
    }

    pub fn state_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, state) as i32
    }

    pub fn stack_limit_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, stack_limit) as i32
    }

    pub fn dtn_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, dtn) as i32
    }

    pub fn managed_thread_handle_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, managed_thread_handle) as i32
    }

    pub fn shape_base_offset() -> i32 {
        offset_of!(ThreadLocalDataLayout, shape_base) as i32
    }
}

pub fn thread_local_dtn_offset() -> i32 {
    ThreadLocalData::dtn_offset()
}
