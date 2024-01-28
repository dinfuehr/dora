use std;
use std::cmp;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::slice;
use std::str;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use crate::gc::root::Slot;
use crate::gc::{Address, Region};
use crate::handle::{create_handle, Handle};
use crate::mem;
use crate::size::InstanceSize;
use crate::vm::{ClassInstance, ClassInstanceId, VM};
use crate::vtable::VTable;

#[repr(C)]
pub struct Header {
    // ptr to class
    word: HeaderWord,
}

#[repr(C)]
struct MetadataWord {
    _padding0: AtomicBool,
    is_remembered: AtomicBool,
    _padding1: u16,
    _padding2: u32,
}

impl MetadataWord {
    fn offset_remembered_byte() -> usize {
        offset_of!(MetadataWord, is_remembered)
    }

    fn compute_word(is_marked: bool, is_remembered: bool) -> u32 {
        is_marked as u32 | (is_remembered as u32) << 8
    }

    fn set_raw(&self, is_remembered: bool) {
        self.is_remembered.store(is_remembered, Ordering::Relaxed);
    }
}

const FWDPTR_BIT: usize = 1;

const MARK_BIT_SHIFT: usize = 32;
const MARK_BIT: usize = 1 << MARK_BIT_SHIFT;

pub const REMEMBERED_BIT_SHIFT: usize = 33;
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

        let compressed_vtable = expected_vtblptr.offset_from(meta_space_start);
        let expected_value = current_value & (0xFFFF_FFFF << 32) | compressed_vtable;

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
    pub fn vtbl(&self, meta_space_start: Address) -> &mut VTable {
        unsafe { &mut *self.raw_vtblptr(meta_space_start).to_mut_ptr::<VTable>() }
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

    pub fn compute_metadata_word(is_marked: bool, is_remembered: bool) -> u32 {
        MetadataWord::compute_word(is_marked, is_remembered)
    }

    pub fn compute_header_word(
        vtblptr: Address,
        meta_space_start: Address,
        is_marked: bool,
        is_remembered: bool,
    ) -> usize {
        HeaderWord::compute_word(vtblptr, meta_space_start, is_marked, is_remembered)
    }

    pub fn offset_vtable_word() -> usize {
        offset_of!(Header, word)
    }

    #[inline(always)]
    pub fn clear_remembered(&self) {
        self.word.clear_remembered();
    }
}

// is used to reference any object
#[repr(C)]
pub struct Obj {
    header: Header,
    data: u8,
}

impl Obj {
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
        let vtbl = unsafe { &*vtblptr.to_mut_ptr::<VTable>() };
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
        let vtable = self.header().vtbl(meta_space_start);
        let classptr = vtable.class_instance_ptr;
        let cls = unsafe { &*classptr };

        visit_refs(self.address(), cls, None, f);
    }

    // TODO: Remove this inline-annotation. It is only required to silence a
    // ASAN failure.
    #[inline(never)]
    pub fn copy_to(&self, dest: Address, size: usize) {
        unsafe {
            ptr::copy(
                self as *const Obj as *const u8,
                dest.to_mut_ptr::<u8>(),
                size,
            );
        }
    }

    pub fn is_filler(&self, vm: &VM) -> bool {
        let vtblptr = self.header().raw_vtblptr(vm.meta_space_start());

        vtblptr == vm.known.filler_word_class_address()
            || vtblptr == vm.known.filler_array_class_address()
            || vtblptr == vm.known.free_space_class_address()
    }
}

fn visit_refs<F>(object: Address, cls: &ClassInstance, range: Option<Region>, f: F)
where
    F: FnMut(Slot),
{
    match cls.size {
        InstanceSize::ObjArray => {
            visit_object_array_refs(object, range, f);
        }

        InstanceSize::StructArray(element_size) => {
            visit_struct_array_refs(object, cls, element_size as usize, None, f);
        }

        InstanceSize::UnitArray | InstanceSize::PrimitiveArray(_) | InstanceSize::Str => {}

        InstanceSize::FillerWord | InstanceSize::FillerArray | InstanceSize::FreeSpace => {
            unreachable!()
        }

        InstanceSize::Fixed(_) => {
            visit_fixed_object(object, cls, f);
        }

        InstanceSize::CodeObject => unreachable!(),
    }
}

fn visit_fixed_object<F>(object: Address, cls: &ClassInstance, mut f: F)
where
    F: FnMut(Slot),
{
    for &offset in &cls.ref_fields {
        f(Slot::at(object.offset(offset as usize)));
    }
}

fn visit_object_array_refs<F>(object: Address, range: Option<Region>, mut f: F)
where
    F: FnMut(Slot),
{
    let array = unsafe { &*object.to_ptr::<StrArray>() };

    // walk through all objects in array
    let mut ptr = Address::from_ptr(array.data());
    let mut limit = ptr.add_ptr(array.len() as usize);

    // visit elements until `limit` reached
    if let Some(range) = range {
        ptr = cmp::max(ptr, range.start);
        limit = cmp::min(limit, range.end);
    }

    while ptr < limit {
        f(Slot::at(ptr));
        ptr = ptr.add_ptr(1);
    }
}

fn visit_struct_array_refs<F>(
    object: Address,
    cls: &ClassInstance,
    element_size: usize,
    range: Option<Region>,
    mut f: F,
) where
    F: FnMut(Slot),
{
    let array = unsafe { &*object.to_ptr::<StrArray>() };

    if cls.ref_fields.is_empty() {
        return;
    }

    // walk through all elements in array
    let array_start = array.data_address();
    let array_size_without_header = element_size * array.len() as usize;
    let array_limit = array_start.offset(array_size_without_header);

    let mut ptr = array_start;
    let mut limit = array_limit;

    if let Some(range) = range {
        if ptr < range.start {
            let skip = (range.start.offset_from(ptr) + element_size - 1) / element_size;
            ptr = ptr.offset(skip * element_size);
        }

        limit = cmp::min(limit, range.end);
    }

    while ptr < limit {
        // each of those elements might have multiple references
        for &offset in &cls.ref_fields {
            f(Slot::at(ptr.offset(offset as usize)));
        }
        ptr = ptr.offset(element_size as usize);
    }
}

fn determine_array_size(obj: &Obj, element_size: usize) -> usize {
    let handle: Ref<UInt8Array> = Ref {
        ptr: obj as *const Obj as *const UInt8Array,
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

#[repr(C)]
pub struct Str {
    header: Header,
    length: usize,
    data: u8,
}

impl Str {
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

    pub fn data(&self) -> *const u8 {
        &self.data as *const u8
    }

    pub fn content(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.data(), self.len()) }
    }

    pub fn content_utf8(&self) -> &str {
        str::from_utf8(self.content()).expect("invalid encoding")
    }

    /// allocates string from buffer in permanent space
    pub fn from_buffer_in_perm(vm: &VM, buf: &[u8]) -> Ref<Str> {
        let mut handle = str_alloc_perm(vm, buf.len());
        handle.length = buf.len();

        let data = handle.data() as *mut u8;
        unsafe {
            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    /// allocates string from buffer in heap
    pub fn from_buffer(vm: &VM, buf: &[u8]) -> Ref<Str> {
        let mut handle = str_alloc_heap(vm, buf.len());
        handle.length = buf.len();

        let data = handle.data() as *mut u8;
        unsafe {
            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    pub fn from_str(vm: &VM, val: Handle<Str>, offset: usize, len: usize) -> Ref<Str> {
        let total_len = val.len();

        if offset > total_len {
            return Ref::null();
        }

        let len = std::cmp::min(total_len - offset, len);

        let slice = unsafe {
            let data = val.data().offset(offset as isize);
            slice::from_raw_parts(data, len)
        };

        if let Ok(_) = str::from_utf8(slice) {
            let mut handle = str_alloc_heap(vm, len);
            handle.length = len;

            let dest = handle.data() as *mut u8;
            unsafe {
                let src = val.data().offset(offset as isize);

                // copy buffer content into Str
                ptr::copy_nonoverlapping(src, dest, len);
            }

            handle
        } else {
            Ref::null()
        }
    }

    pub fn concat(vm: &VM, lhs: Handle<Str>, rhs: Handle<Str>) -> Handle<Str> {
        let len = lhs.len() + rhs.len();
        let mut handle = create_handle(str_alloc_heap(vm, len));

        handle.length = len;
        unsafe {
            ptr::copy_nonoverlapping(lhs.data(), handle.data() as *mut u8, lhs.len());
            ptr::copy_nonoverlapping(
                rhs.data(),
                handle.data().offset(lhs.len() as isize) as *mut u8,
                rhs.len(),
            );
        }

        handle
    }

    // duplicate string into a new object
    pub fn dup(&self, vm: &VM) -> Ref<Str> {
        let len = self.len();
        let mut handle = str_alloc_heap(vm, len);

        handle.length = len;
        unsafe {
            ptr::copy_nonoverlapping(self.data(), handle.data() as *mut u8, len);
        }

        handle
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

    let clsid = vm.byte_array();
    let cls = vm.class_instances.idx(clsid);
    let vtable = cls.vtable.read();
    let vtable: &VTable = vtable.as_ref().unwrap();
    let mut handle: Ref<UInt8Array> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
    handle.header_mut().setup_header_word(
        Address::from_ptr(vtable as *const VTable),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );
    handle.length = len;

    handle
}

fn str_alloc_heap(vm: &VM, len: usize) -> Ref<Str> {
    str_alloc(vm, len, |vm, size| vm.gc.alloc(vm, size), false)
}

fn str_alloc_perm(vm: &VM, len: usize) -> Ref<Str> {
    str_alloc(vm, len, |vm, size| vm.gc.alloc_readonly(vm, size), true)
}

fn str_alloc<F>(vm: &VM, len: usize, alloc: F, is_readonly: bool) -> Ref<Str>
where
    F: FnOnce(&VM, usize) -> Address,
{
    let size = Header::size() as usize      // Object header
                + mem::ptr_width() as usize // length field
                + len; // string content

    let size = mem::align_usize_up(size, mem::ptr_width() as usize);
    let ptr = alloc(vm, size);

    let clsid = vm.str();
    let cls = vm.class_instances.idx(clsid);
    let vtable = cls.vtable.read();
    let vtable: &VTable = vtable.as_ref().unwrap();
    let handle: Ref<Str> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, is_readonly);
    handle.header().setup_header_word(
        Address::from_ptr(vtable as *const VTable),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );

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

    pub fn alloc(vm: &VM, len: usize, elem: T, clsid: ClassInstanceId) -> Ref<Array<T>> {
        let size = Header::size() as usize        // Object header
                   + mem::ptr_width() as usize    // length field
                   + len * std::mem::size_of::<T>(); // array content

        let ptr = vm.gc.alloc(vm, size).to_usize();
        let cls = vm.class_instances.idx(clsid);
        let vtable = cls.vtable.read();
        let vtable: &VTable = vtable.as_ref().unwrap();
        let mut handle: Ref<Array<T>> = ptr.into();
        let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
        handle.header_mut().setup_header_word(
            Address::from_ptr(vtable as *const VTable),
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

pub fn alloc(vm: &VM, clsid: ClassInstanceId) -> Ref<Obj> {
    let cls_def = vm.class_instances.idx(clsid);

    let size = match cls_def.size {
        InstanceSize::Fixed(size) => size as usize,
        _ => panic!("alloc only supports fix-sized types"),
    };

    let size = mem::align_usize_up(size, mem::ptr_width() as usize);

    let ptr = vm.gc.alloc(vm, size).to_usize();
    let vtable = cls_def.vtable.read();
    let vtable: &VTable = vtable.as_ref().unwrap();
    let object: Ref<Obj> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, false);
    object.header().setup_header_word(
        Address::from_ptr(vtable),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );

    object
}

pub struct Stacktrace {
    pub header: Header,
    pub backtrace: Ref<Int32Array>,
    pub elements: Ref<Obj>,
}

pub struct StacktraceElement {
    pub header: Header,
    pub name: Ref<Str>,
    pub line: i32,
}
