use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};

use crate::gc::Address;
use crate::object::{Obj, Ref};
use crate::os::{self, Reservation};
use crate::threads::current_thread;

pub struct HandleMemory {
    inner: UnsafeCell<HandleMemoryInner>,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        HandleMemory {
            inner: UnsafeCell::new(HandleMemoryInner::new()),
        }
    }

    fn get_inner_mut(&self) -> &mut HandleMemoryInner {
        unsafe { &mut *self.inner.get() }
    }

    pub fn create_handle<T>(&self, obj: Ref<T>) -> Handle<T> {
        let address = self.get_inner_mut().create_handle(obj.address());
        Handle(address.to_mut_ptr())
    }

    #[cfg(test)]
    fn handle_address(&self, object_address: Address) {
        let inner = unsafe { &mut *self.inner.get() };
        inner.create_handle(object_address);
    }

    fn create_scope(&self) -> HandleScope {
        self.get_inner_mut().create_scope()
    }

    fn drop_scope(&self, scope: HandleScope) {
        self.get_inner_mut().drop_scope(scope)
    }

    pub fn iterate_for_gc(&self) -> HandleMemoryIter {
        let inner = self.get_inner_mut();

        HandleMemoryIter {
            mem: inner,
            next_block_idx: 0,
            next: Address::null(),
            limit: Address::null(),
        }
    }
}

pub struct HandleMemoryInner {
    /// All blocks, Box is important since HandleBlock
    /// is a big struct that needs to get moved/copied on resizes.
    blocks: Vec<Reservation>,

    // Adress of next handle
    top: Address,
}

impl HandleMemoryInner {
    fn new() -> HandleMemoryInner {
        let reservation = allocate_block();
        let top = get_block_first(reservation.start());

        HandleMemoryInner {
            blocks: vec![reservation],
            top,
        }
    }

    #[inline(always)]
    fn create_handle(&mut self, object_address: Address) -> Address {
        let result = self.top;
        unsafe {
            *result.to_mut_ptr() = object_address;
        }

        self.top = result.add_ptr(1);

        if is_handle_block_aligned(self.top) {
            self.create_handle_slow();
        }

        result
    }

    fn create_handle_slow(&mut self) {
        let reservation = allocate_block();
        self.top = get_block_first(reservation.start());
        self.blocks.push(reservation);
    }

    #[inline(always)]
    fn create_scope(&mut self) -> HandleScope {
        HandleScope { last_top: self.top }
    }

    #[inline(always)]
    fn drop_scope(&mut self, scope: HandleScope) {
        if align_down_to_block_size(scope.last_top) == align_down_to_block_size(self.top) {
            assert!(scope.last_top <= self.top);
            self.top = scope.last_top;
        } else {
            self.drop_scope_slow(scope);
        }
    }

    fn drop_scope_slow(&mut self, scope: HandleScope) {
        self.top = scope.last_top;

        while !self.blocks.is_empty() {
            let block_start = self.blocks.last().expect("no element").start();

            if align_down_to_block_size(self.top) == block_start {
                return;
            }

            // Drop memory reservation.
            self.blocks.pop().expect("no element");
        }

        assert!(self.blocks.is_empty());
        assert_eq!(self.top, Address::null());
    }
}

fn allocate_block() -> Reservation {
    let reservation = os::commit_align(HANDLE_BLOCK_SIZE, HANDLE_BLOCK_SIZE, false);
    assert!(is_handle_block_aligned(reservation.start()));
    reservation
}

fn align_down_to_block_size(address: Address) -> Address {
    let aligned = address.to_usize() & !(HANDLE_BLOCK_SIZE - 1);
    aligned.into()
}

fn more_space_in_block(address: Address) -> bool {
    !address
        .add_ptr(1)
        .is_power_of_2_aligned(HANDLE_BLOCK_SIZE_BITS)
}

fn is_handle_block_aligned(block_start: Address) -> bool {
    block_start.is_power_of_2_aligned(HANDLE_BLOCK_SIZE_BITS)
}

fn get_block_limit(block_start: Address) -> Address {
    block_start.offset(HANDLE_BLOCK_SIZE)
}

fn get_block_first(block_start: Address) -> Address {
    block_start.add_ptr(1)
}

struct HandleScope {
    last_top: Address,
}

pub fn create_handle<T>(obj: Ref<T>) -> Handle<T> {
    let thread = current_thread();
    debug_assert!(thread.is_running());
    thread.handles.create_handle(obj)
}

const HANDLE_BLOCK_SIZE_BITS: usize = 16;
const HANDLE_BLOCK_SIZE: usize = 1 << HANDLE_BLOCK_SIZE_BITS;

struct BorderData {
    blocks: usize,
    element: usize,
}
#[repr(C)]
pub struct Handle<T>(*mut Ref<T>);

impl<T> Handle<T> {
    pub fn direct(self) -> Ref<T> {
        debug_assert!(current_thread().is_running());
        unsafe { *self.0 }
    }

    pub fn direct_ptr(self) -> Address {
        debug_assert!(current_thread().is_running());
        self.raw_load()
    }

    // Internal method for dereferencing this handle, does not
    // check thread on purpose for testing purposes.
    fn raw_load(self) -> Address {
        unsafe { *self.0 }.address()
    }

    pub fn location(&self) -> Address {
        Address::from_ptr(self.0)
    }

    pub fn from_address(location: Address) -> Handle<T> {
        Handle(location.to_mut_ptr())
    }

    pub fn cast<R>(self) -> Handle<R> {
        Handle(self.0 as *mut Ref<R>)
    }
}

impl<T> Deref for Handle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        debug_assert!(current_thread().is_running());
        unsafe { &*self.0 }.deref()
    }
}

impl<T> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut T {
        debug_assert!(current_thread().is_running());
        unsafe { &mut *self.0 }.deref_mut()
    }
}

// known limitation of #[derive(Copy, Clone)]
// traits need to be implemented manually
impl<T> Copy for Handle<T> {}
impl<T> Clone for Handle<T> {
    fn clone(&self) -> Handle<T> {
        *self
    }
}

pub struct HandleMemoryIter<'a> {
    mem: &'a HandleMemoryInner,
    next_block_idx: usize,
    next: Address,
    limit: Address,
}

impl<'a> Iterator for HandleMemoryIter<'a> {
    type Item = Handle<Obj>;

    fn next(&mut self) -> Option<Handle<Obj>> {
        if self.next < self.limit {
            let current = Handle::from_address(self.next);
            self.next = self.next.add_ptr(1);
            return Some(current);
        }

        if self.next_block_idx < self.mem.blocks.len() {
            let block_start = self.mem.blocks[self.next_block_idx].start();
            let block_limit = get_block_limit(block_start);
            let first_handle = get_block_first(block_start);

            self.next = first_handle.add_ptr(1);
            self.limit = if self.next_block_idx + 1 == self.mem.blocks.len() {
                // There should always be at least one handle in a block.
                assert!(block_start <= self.mem.top);
                assert!(self.mem.top < block_limit);
                self.mem.top
            } else {
                block_limit
            };

            self.next_block_idx += 1;

            return if first_handle < self.limit {
                Some(Handle::from_address(first_handle))
            } else {
                None
            };
        }

        None
    }
}

#[test]
fn test_handle_iteration() {
    let first_address = get_block_first(Address::null()).to_usize();
    let limit_address = get_block_limit(Address::null()).to_usize();

    let handles_per_block: usize = (limit_address - first_address) / crate::mem::ptr_width_usize();

    let sizes = [
        0,
        4,
        handles_per_block / 4,
        3 * handles_per_block,
        3 * handles_per_block + handles_per_block / 2,
        3 * handles_per_block + handles_per_block / 4,
    ];

    for size in sizes {
        let hm = HandleMemory::new();

        for _ in 0..size {
            hm.handle_address(1.into());
        }

        let scope = hm.create_scope();

        for _ in 0..size {
            hm.handle_address(2.into());
        }

        hm.handle_address(2.into());

        hm.drop_scope(scope);

        assert_eq!(hm.iterate_for_gc().count(), size);
        assert!(hm.iterate_for_gc().all(|x| x.raw_load() == 1.into()));
    }
}

pub fn handle_scope<F: FnOnce() -> R, R>(f: F) -> R {
    let thread = current_thread();
    assert!(thread.is_running());
    let scope = thread.handles.create_scope();
    let result = f();
    thread.handles.drop_scope(scope);
    result
}
