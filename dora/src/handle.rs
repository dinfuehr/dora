use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};

use crate::gc::{Address, K};
use crate::object::{Obj, Ref};
use crate::os;
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
        let top = inner.top;

        HandleMemoryIter {
            mem: inner,
            next_block_idx: 0,
            next: Address::null(),
            limit: Address::null(),
            top_in_last_block: top,
        }
    }
}

pub struct HandleMemoryInner {
    /// All blocks, Box is important since HandleBlock
    /// is a big struct that needs to get moved/copied on resizes.
    blocks: Vec<Address>,

    // Adress of next handle
    top: Address,
    limit: Address,
}

impl HandleMemoryInner {
    fn new() -> HandleMemoryInner {
        HandleMemoryInner {
            blocks: Vec::new(),
            top: Address::null(),
            limit: Address::null(),
        }
    }

    #[inline(always)]
    fn create_handle(&mut self, object_address: Address) -> Address {
        if self.top < self.limit {
            let result = self.top;
            unsafe {
                *result.to_mut_ptr() = object_address;
            }
            self.top = self.top.add_ptr(1);
            return result;
        }

        self.create_handle_slow(object_address)
    }

    fn create_handle_slow(&mut self, object_address: Address) -> Address {
        assert_eq!(self.top, self.limit);
        let block_start = os::commit(HANDLE_BLOCK_SIZE, false);
        unsafe {
            *block_start.to_mut_ptr() = object_address;
        }
        self.top = block_start.add_ptr(1);
        self.limit = block_start.offset(HANDLE_BLOCK_SIZE);
        self.blocks.push(block_start);
        block_start
    }

    #[inline(always)]
    fn create_scope(&mut self) -> HandleScope {
        HandleScope {
            last_top: self.top,
            last_limit: self.limit,
        }
    }

    #[inline(always)]
    fn drop_scope(&mut self, scope: HandleScope) {
        if scope.last_limit == self.limit {
            assert!(scope.last_top <= self.top);
            self.top = scope.last_top;
        } else {
            self.drop_scope_slow(scope);
        }
    }

    fn drop_scope_slow(&mut self, scope: HandleScope) {
        self.top = scope.last_top;
        self.limit = scope.last_limit;

        assert_ne!(
            self.limit,
            self.blocks
                .last()
                .expect("no element")
                .offset(HANDLE_BLOCK_SIZE)
        );

        while let Some(&block_start) = self.blocks.last() {
            let block_limit = block_start.offset(HANDLE_BLOCK_SIZE);

            if self.limit == block_limit {
                return;
            }

            os::free(block_start, HANDLE_BLOCK_SIZE);
            assert_eq!(block_start, self.blocks.pop().expect("no element"));
        }

        assert!(self.blocks.is_empty());
        assert_eq!(self.top, Address::null());
        assert_eq!(self.limit, Address::null());
    }
}

impl Drop for HandleMemoryInner {
    fn drop(&mut self) {
        for &block in &self.blocks {
            os::free(block, HANDLE_BLOCK_SIZE);
        }
    }
}

struct HandleScope {
    last_top: Address,
    last_limit: Address,
}

pub fn create_handle<T>(obj: Ref<T>) -> Handle<T> {
    let thread = current_thread();
    debug_assert!(thread.is_running());
    thread.handles.create_handle(obj)
}

const HANDLE_BLOCK_SIZE: usize = 64 * K;

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
    top_in_last_block: Address,
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
            let block_start = self.mem.blocks[self.next_block_idx];
            let block_limit = block_start.offset(HANDLE_BLOCK_SIZE);

            self.next = block_start.add_ptr(1);
            self.limit = if self.next_block_idx + 1 == self.mem.blocks.len() {
                // There should always be at least one handle in a block.
                assert!(block_start < self.top_in_last_block);
                assert!(self.top_in_last_block <= block_limit);
                self.top_in_last_block
            } else {
                block_limit
            };

            self.next_block_idx += 1;
            return Some(Handle::from_address(block_start));
        }

        None
    }
}

#[test]
fn test_handle_iteration() {
    let handles_per_block: usize = HANDLE_BLOCK_SIZE / crate::mem::ptr_width_usize();

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
