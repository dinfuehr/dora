use parking_lot::{Mutex, MutexGuard};
use std::ops::{Deref, DerefMut};

use crate::gc::Address;
use crate::object::{Obj, Ref};
use crate::threads::current_thread;

pub const HANDLE_BLOCK_SIZE: usize = 256;

pub struct HandleMemory {
    inner: Mutex<HandleMemoryInner>,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        HandleMemory {
            inner: Mutex::new(HandleMemoryInner::new()),
        }
    }

    pub fn handle<T>(&self, obj: Ref<T>) -> Handle<T> {
        debug_assert!(current_thread().is_running());
        let address = self.inner.lock().handle(obj.address());
        Handle(address.to_mut_ptr())
    }

    #[cfg(test)]
    fn handle_address(&self, object_address: Address) {
        self.inner.lock().handle(object_address);
    }

    pub fn push_border(&self) {
        self.inner.lock().push_border();
    }

    pub fn pop_border(&self) {
        self.inner.lock().pop_border();
    }

    pub fn iter(&self) -> HandleMemoryIter {
        let inner = self.inner.lock();
        let len = inner.blocks.len();
        let free = inner.free;

        HandleMemoryIter {
            mem: inner,
            block_idx: 0,
            element_idx: 0,
            filled_blocks: if len == 0 { 0 } else { len - 1 },
            handles_in_last_block: free,
        }
    }
}

pub struct HandleMemoryInner {
    /// All blocks, Box is important since HandleBlock
    /// is a big struct that needs to get moved/copied on resizes.
    blocks: Vec<Box<HandleBlock>>,

    // Store addresses of inserted borders.
    borders: Vec<BorderData>,

    // Index of next free position in block.
    free: usize,
}

impl HandleMemoryInner {
    pub fn new() -> HandleMemoryInner {
        let initial_block = Box::new(HandleBlock::new());

        HandleMemoryInner {
            blocks: vec![initial_block],
            borders: Vec::new(),
            free: 0,
        }
    }

    pub fn handle(&mut self, object_address: Address) -> Address {
        if self.free >= HANDLE_BLOCK_SIZE {
            self.push_block();
            self.free = 0;
        }

        let block = self.blocks.last_mut().unwrap();

        let idx = self.free;
        let elem = &mut block.elements[idx];
        self.free = idx + 1;

        *elem = object_address;

        Address::from_ptr(elem)
    }

    fn push_block(&mut self) {
        self.blocks.push(Box::new(HandleBlock::new()));
    }

    pub fn push_border(&mut self) {
        let blocks = self.blocks.len();
        let element = self.free;

        self.borders.push(BorderData { blocks, element });
    }

    pub fn pop_border(&mut self) {
        let border = self.borders.pop().expect("no border left");

        self.blocks.truncate(border.blocks);
        self.free = border.element;
    }
}

pub fn handle<T>(obj: Ref<T>) -> Handle<T> {
    let thread = current_thread();
    debug_assert!(thread.state_relaxed().is_running());
    thread.handles.handle(obj)
}

struct HandleBlock {
    elements: [Address; HANDLE_BLOCK_SIZE],
}

impl HandleBlock {
    fn new() -> HandleBlock {
        HandleBlock {
            elements: [Address::null(); HANDLE_BLOCK_SIZE],
        }
    }
}

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
    mem: MutexGuard<'a, HandleMemoryInner>,
    block_idx: usize,
    element_idx: usize,
    filled_blocks: usize,
    handles_in_last_block: usize,
}

impl<'a> Iterator for HandleMemoryIter<'a> {
    type Item = Handle<Obj>;

    fn next(&mut self) -> Option<Handle<Obj>> {
        if self.element_idx == HANDLE_BLOCK_SIZE {
            self.block_idx += 1;
            self.element_idx = 0;
        }

        if (self.block_idx < self.filled_blocks)
            || (self.block_idx == self.filled_blocks
                && self.element_idx < self.handles_in_last_block)
        {
            let idx = self.element_idx;
            self.element_idx += 1;

            let block = &self.mem.blocks[self.block_idx];
            return Some(Handle::from_address(Address::from_ptr(
                &block.elements[idx],
            )));
        } else {
            None
        }
    }
}

#[test]
fn test_handle_iteration() {
    let sizes = [
        0,
        4,
        HANDLE_BLOCK_SIZE / 4,
        3 * HANDLE_BLOCK_SIZE,
        3 * HANDLE_BLOCK_SIZE + HANDLE_BLOCK_SIZE / 2,
        3 * HANDLE_BLOCK_SIZE + HANDLE_BLOCK_SIZE / 4,
    ];

    for size in sizes {
        let hm = HandleMemory::new();

        for _ in 0..size {
            hm.handle_address(1.into());
        }

        hm.push_border();

        for _ in 0..size {
            hm.handle_address(2.into());
        }

        hm.handle_address(2.into());

        hm.pop_border();

        assert_eq!(hm.iter().count(), size);
        assert!(hm.iter().all(|x| x.raw_load() == 1.into()));
    }
}

pub fn handle_scope<F: FnOnce() -> R, R>(f: F) -> R {
    let thread = current_thread();
    thread.handles.push_border();
    let result = f();
    thread.handles.pop_border();
    result
}
