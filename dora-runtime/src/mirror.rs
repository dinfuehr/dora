use std;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::Address;
use crate::gc::root::Slot;
use crate::handle::{Handle, create_handle};
use crate::mem;
pub use crate::mirror::string::Str;
use crate::vm::VM;
use crate::{Shape, ShapeVisitor};

mod string;

#[repr(C)]
pub struct Header {
    // ptr to class
    word: HeaderWord,
}

const FWDPTR_BIT: usize = 1;
const METADATA_OFFSET: usize = 4;

const MARK_BIT_SHIFT: usize = METADATA_OFFSET * 8;
const MARK_BIT: usize = 1 << MARK_BIT_SHIFT;

pub const REMEMBERED_BIT_SHIFT: usize = MARK_BIT_SHIFT + 1;
const REMEMBERED_BIT: usize = 1 << REMEMBERED_BIT_SHIFT;

#[repr(C)]
struct HeaderWord(AtomicUsize);

impl HeaderWord {
    #[inline(always)]
    fn raw_vtblptr(&self, meta_space_start: Address) -> Address {
        let value = self.raw();
        let value = value & 0xFFFF_FFFF;
        let full = meta_space_start.offset(value);
        debug_assert_eq!(full.to_usize() & FWDPTR_BIT, 0);
        full.into()
    }

    fn install_fwdptr(&self, value: Address) {
        self.set_raw(value.to_usize() | FWDPTR_BIT);
    }

    fn vtblptr_or_fwdptr(&self, meta_space_start: Address) -> VtblptrWordKind {
        let value = self.raw();

        if (value & FWDPTR_BIT) != 0 {
            let address: Address = (value & !FWDPTR_BIT).into();
            VtblptrWordKind::Fwdptr(address)
        } else {
            let value = value & 0xFFFF_FFFF;
            let full = meta_space_start.offset(value);
            VtblptrWordKind::Vtblptr(full)
        }
    }

    fn try_install_fwdptr(
        &self,
        expected_vtblptr: Address,
        meta_space_start: Address,
        new_address: Address,
    ) -> ForwardResult {
        let current_value = self.raw();

        let compressed_shape = expected_vtblptr.offset_from(meta_space_start);
        let expected_value = current_value & (0xFFFF_FFFF << 32) | compressed_shape;

        let fwd = new_address.to_usize() | 1;
        let result =
            self.0
                .compare_exchange(expected_value, fwd, Ordering::Relaxed, Ordering::Relaxed);

        match result {
            Ok(value) => {
                assert_eq!(value, expected_value);
                ForwardResult::Forwarded
            }

            Err(forwarding_value) => {
                // If update fails, this needs to be a forwarding pointer
                debug_assert!((forwarding_value | 1) != 0);

                let forwarding_address: Address = (forwarding_value & !1).into();
                ForwardResult::AlreadyForwarded(forwarding_address)
            }
        }
    }

    fn setup(
        &self,
        vtblptr: Address,
        meta_space_start: Address,
        is_marked: bool,
        is_remembered: bool,
    ) {
        self.set_raw(HeaderWord::compute_word(
            vtblptr,
            meta_space_start,
            is_marked,
            is_remembered,
        ));
    }

    fn compute_word(
        vtblptr: Address,
        meta_space_start: Address,
        is_marked: bool,
        is_remembered: bool,
    ) -> usize {
        let compressed = vtblptr.offset_from(meta_space_start);
        compressed
            | (0xFFFF_FFFC << 32)
            | (is_marked as usize) << MARK_BIT_SHIFT
            | (is_remembered as usize) << REMEMBERED_BIT_SHIFT
    }

    fn try_mark(&self) -> bool {
        let mut current = self.raw();
        loop {
            if current & MARK_BIT == 0 {
                let next = (current & !REMEMBERED_BIT) | MARK_BIT;
                let result = self.0.compare_exchange_weak(
                    current,
                    next,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                );

                match result {
                    Ok(_) => return true,
                    Err(actual) => current = actual,
                }
            } else {
                return false;
            }
        }
    }

    fn clear_mark(&self) {
        self.0.fetch_and(!MARK_BIT, Ordering::Relaxed);
    }

    fn is_marked(&self) -> bool {
        (self.raw() & MARK_BIT) != 0
    }

    fn is_remembered(&self) -> bool {
        (self.raw() & REMEMBERED_BIT) != 0
    }

    fn set_remembered(&self) {
        let current = self.raw();
        self.set_raw(current | REMEMBERED_BIT);
    }

    fn clear_remembered(&self) {
        let current = self.raw();
        self.set_raw(current & !REMEMBERED_BIT);
    }

    fn raw(&self) -> usize {
        self.0.load(Ordering::Relaxed)
    }

    fn set_raw(&self, value: usize) {
        self.0.store(value, Ordering::Relaxed);
    }
}

pub enum ForwardResult {
    Forwarded,
    AlreadyForwarded(Address),
}

pub enum VtblptrWordKind {
    Vtblptr(Address),
    Fwdptr(Address),
}

impl Header {
    #[inline(always)]
    pub const fn size() -> i32 {
        std::mem::size_of::<Header>() as i32
    }

    #[inline(always)]
    pub fn array_size() -> i32 {
        Header::size() + mem::ptr_width()
    }

    #[inline(always)]
    pub fn shape(&self, meta_space_start: Address) -> &Shape {
        unsafe { &*self.raw_vtblptr(meta_space_start).to_ptr::<Shape>() }
    }

    pub fn compressed_vtblptr(&self) -> usize {
        self.word.raw() & 0xFFFF_FFFF
    }

    pub fn sentinel(&self) -> usize {
        (self.word.raw() >> 32) & 0xFFFF_FFFC
    }

    #[inline(always)]
    fn raw_vtblptr(&self, meta_space_start: Address) -> Address {
        self.word.raw_vtblptr(meta_space_start)
    }

    #[inline(always)]
    pub fn setup_header_word(
        &self,
        addr: Address,
        meta_space_start: Address,
        is_marked: bool,
        is_remembered: bool,
    ) {
        self.word
            .setup(addr, meta_space_start, is_marked, is_remembered);
    }

    #[inline(always)]
    pub fn install_fwdptr(&self, address: Address) {
        self.word.install_fwdptr(address);
    }

    #[inline(always)]
    pub fn vtblptr_or_fwdptr(&self, meta_space_start: Address) -> VtblptrWordKind {
        self.word.vtblptr_or_fwdptr(meta_space_start)
    }

    #[inline(always)]
    pub fn try_install_fwdptr(
        &self,
        meta_space_start: Address,
        expected_vtblptr: Address,
        new_address: Address,
    ) -> ForwardResult {
        self.word
            .try_install_fwdptr(expected_vtblptr, meta_space_start, new_address)
    }

    #[inline(always)]
    pub fn try_mark(&self) -> bool {
        self.word.try_mark()
    }

    #[inline(always)]
    pub fn clear_mark(&self) {
        self.word.clear_mark();
    }

    #[inline(always)]
    pub fn is_marked(&self) -> bool {
        self.word.is_marked()
    }

    #[inline(always)]
    pub fn is_remembered(&self) -> bool {
        self.word.is_remembered()
    }

    #[inline(always)]
    pub fn set_remembered(&self) {
        self.word.set_remembered()
    }

    pub fn compute_header_word(
        vtblptr: Address,
        meta_space_start: Address,
        is_marked: bool,
        is_remembered: bool,
    ) -> usize {
        HeaderWord::compute_word(vtblptr, meta_space_start, is_marked, is_remembered)
    }

    pub fn offset_shape_word() -> usize {
        offset_of!(Header, word)
    }

    pub fn offset_metadata_word() -> usize {
        METADATA_OFFSET
    }

    #[inline(always)]
    pub fn clear_remembered(&self) {
        self.word.clear_remembered();
    }
}

// is used to reference any object
#[repr(C)]
pub struct Object {
    header: Header,
    data: u8,
}

impl Object {
    #[inline(always)]
    pub fn address(&self) -> Address {
        Address::from_ptr(self as *const _)
    }

    #[inline(always)]
    pub fn header(&self) -> &Header {
        &self.header
    }

    #[inline(always)]
    pub fn data(&self) -> *const u8 {
        &self.data as *const u8
    }

    pub fn size_for_vtblptr(&self, vtblptr: Address) -> usize {
        let vtbl = unsafe { &*vtblptr.to_mut_ptr::<Shape>() };
        let instance_size = vtbl.instance_size;

        if instance_size != 0 {
            return instance_size;
        }

        determine_array_size(self, vtbl.element_size)
    }

    pub fn size(&self, meta_space_start: Address) -> usize {
        self.size_for_vtblptr(self.header().raw_vtblptr(meta_space_start))
    }

    pub fn visit_reference_fields<F>(&self, meta_space_start: Address, f: F)
    where
        F: FnMut(Slot),
    {
        let shape = self.header().shape(meta_space_start);
        visit_refs(shape, self.address(), f);
    }

    // TODO: Remove this inline-annotation. It is only required to silence a
    // ASAN failure.
    #[inline(never)]
    pub fn copy_to(&self, dest: Address, size: usize) {
        unsafe {
            ptr::copy(
                self as *const Object as *const u8,
                dest.to_mut_ptr::<u8>(),
                size,
            );
        }
    }

    pub fn is_filler(&self, vm: &VM) -> bool {
        let vtblptr = self.header().raw_vtblptr(vm.meta_space_start());

        vtblptr == vm.known.filler_word_shape().address()
            || vtblptr == vm.known.filler_array_shape().address()
            || vtblptr == vm.known.free_space_shape().address()
    }
}

fn visit_refs<F>(shape: &Shape, object: Address, f: F)
where
    F: FnMut(Slot),
{
    match shape.visitor {
        ShapeVisitor::PointerArray => {
            visit_object_array_refs(object, f);
        }

        ShapeVisitor::RecordArray => {
            visit_struct_array_refs(shape, object, shape.element_size as usize, f);
        }

        ShapeVisitor::Regular => {
            visit_regular_object(shape, object, f);
        }

        ShapeVisitor::None => {}
        ShapeVisitor::Invalid => unreachable!(),
    }
}

fn visit_regular_object<F>(shape: &Shape, object: Address, mut f: F)
where
    F: FnMut(Slot),
{
    for &offset in &shape.refs {
        f(Slot::at(object.offset(offset as usize)));
    }
}

fn visit_object_array_refs<F>(object: Address, mut f: F)
where
    F: FnMut(Slot),
{
    let array = unsafe { &*object.to_ptr::<StrArray>() };

    // walk through all objects in array
    let mut ptr = Address::from_ptr(array.data());
    let limit = ptr.add_ptr(array.len() as usize);

    while ptr < limit {
        f(Slot::at(ptr));
        ptr = ptr.add_ptr(1);
    }
}

fn visit_struct_array_refs<F>(shape: &Shape, object: Address, element_size: usize, mut f: F)
where
    F: FnMut(Slot),
{
    let array = unsafe { &*object.to_ptr::<StrArray>() };
    debug_assert!(!shape.refs.is_empty());

    // walk through all elements in array
    let array_start = array.data_address();
    let array_size_without_header = element_size * array.len() as usize;
    let array_end = array_start.offset(array_size_without_header);

    let mut ptr = array_start;

    while ptr < array_end {
        // each of those elements might have multiple references
        for &offset in &shape.refs {
            f(Slot::at(ptr.offset(offset as usize)));
        }
        ptr = ptr.offset(element_size as usize);
    }
}

fn determine_array_size(obj: &Object, element_size: usize) -> usize {
    let handle: Ref<UInt8Array> = Ref {
        ptr: obj as *const Object as *const UInt8Array,
    };

    let calc =
        Header::size() as usize + mem::ptr_width_usize() + element_size * handle.len() as usize;

    mem::align_usize_up(calc, mem::ptr_width_usize())
}

#[repr(C)]
pub struct Ref<T> {
    ptr: *const T,
}

unsafe impl<T> Send for Ref<T> {}
unsafe impl<T> Sync for Ref<T> {}

impl<T> Ref<T> {
    pub fn null() -> Ref<T> {
        Ref { ptr: ptr::null() }
    }

    pub fn cast<R>(&self) -> Ref<R> {
        Ref {
            ptr: self.ptr as *const R,
        }
    }

    pub fn raw(&self) -> *const T {
        self.ptr
    }

    pub fn address(&self) -> Address {
        Address::from_ptr(self.ptr)
    }
}

// known limitation of #[derive(Copy, Clone)]
// traits need to be implemented manually
impl<T> Copy for Ref<T> {}
impl<T> Clone for Ref<T> {
    fn clone(&self) -> Ref<T> {
        *self
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<T> DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *(self.ptr as *mut T) }
    }
}

impl<T> Into<Ref<T>> for usize {
    fn into(self) -> Ref<T> {
        Ref {
            ptr: self as *const T,
        }
    }
}

impl<T> Into<Ref<T>> for Address {
    fn into(self) -> Ref<T> {
        Ref { ptr: self.to_ptr() }
    }
}

pub fn byte_array_from_buffer(vm: &VM, buf: &[u8]) -> Ref<UInt8Array> {
    let mut handle = byte_array_alloc_heap(vm, buf.len());
    handle.length = buf.len();

    let data = handle.data() as *mut u8;
    unsafe {
        // copy buffer content into Str
        ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
    }

    handle
}

fn byte_array_alloc_heap(vm: &VM, len: usize) -> Ref<UInt8Array> {
    let size = Header::size() as usize      // Object header
                + mem::ptr_width() as usize // length field
                + len; // array content

    let size = mem::align_usize_up(size, mem::ptr_width() as usize);
    let ptr = vm.gc.alloc(vm, size);

    let mut handle: Ref<UInt8Array> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
    handle.header_mut().setup_header_word(
        vm.known.byte_array_shape().address(),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );
    handle.length = len;

    handle
}

pub trait ArrayElement {
    const REF: bool;
}

impl ArrayElement for bool {
    const REF: bool = false;
}

impl ArrayElement for u8 {
    const REF: bool = false;
}

impl ArrayElement for char {
    const REF: bool = false;
}

impl ArrayElement for i32 {
    const REF: bool = false;
}

impl ArrayElement for i64 {
    const REF: bool = false;
}

impl ArrayElement for f32 {
    const REF: bool = false;
}

impl ArrayElement for f64 {
    const REF: bool = false;
}

impl ArrayElement for Ref<Str> {
    const REF: bool = true;
}

#[repr(C)]
pub struct Array<T: Copy> {
    header: Header,
    length: usize,
    data: u8,
    phantom: PhantomData<T>,
}

impl<T> Array<T>
where
    T: Copy + ArrayElement,
{
    #[allow(dead_code)]
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn data(&self) -> *const T {
        &self.data as *const u8 as *const T
    }

    pub fn slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.data(), self.len()) }
    }

    pub fn data_address(&self) -> Address {
        Address::from_ptr(self.data())
    }

    pub fn data_mut(&mut self) -> *mut T {
        &self.data as *const u8 as *mut T
    }

    pub fn get_at(&self, idx: usize) -> T {
        unsafe { *self.data().offset(idx as isize) }
    }

    pub fn set_at(&mut self, idx: usize, val: T) {
        unsafe {
            *self.data_mut().offset(idx as isize) = val;
        }
    }

    pub fn alloc(vm: &VM, len: usize, elem: T, shape: &Shape) -> Ref<Array<T>> {
        let size = Header::size() as usize        // Object header
                   + mem::ptr_width() as usize    // length field
                   + len * std::mem::size_of::<T>(); // array content

        let ptr = vm.gc.alloc(vm, size).to_usize();
        let mut handle: Ref<Array<T>> = ptr.into();
        let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
        handle.header_mut().setup_header_word(
            shape.address(),
            vm.meta_space_start(),
            is_marked,
            is_remembered,
        );
        handle.length = len;

        for i in 0..handle.len() {
            unsafe {
                *handle.data_mut().offset(i as isize) = elem;
            }
        }

        handle
    }
}

pub fn offset_of_array_length() -> i32 {
    offset_of!(Array<i32>, length) as i32
}

pub fn offset_of_array_data() -> i32 {
    offset_of!(Array<i32>, data) as i32
}

pub type UInt8Array = Array<u8>;
pub type Int32Array = Array<i32>;
pub type StrArray = Array<Ref<Str>>;

pub fn alloc(vm: &VM, shape: &Shape) -> Ref<Object> {
    let size = mem::align_usize_up(shape.instance_size(), mem::ptr_width() as usize);
    assert!(size > 0);

    let ptr = vm.gc.alloc(vm, size).to_usize();
    let object: Ref<Object> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
    object.header().setup_header_word(
        shape.address(),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );

    object
}

#[repr(C)]
pub struct Stacktrace {
    pub header: Header,
    pub backtrace: Ref<Int32Array>,
    pub elements: Ref<Object>,
}

#[repr(C)]
pub struct StacktraceElement {
    pub header: Header,
    pub text: Ref<Str>,
}

#[repr(C)]

pub struct StacktraceIterator {
    pub header: Header,
    pub code_id: i32,
    pub offset: i32,
    pub text: Ref<Str>,
    pub inlined_function_id: i32,
}
