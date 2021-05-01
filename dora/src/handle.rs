use parking_lot::{Mutex, MutexGuard};
use std::ops::{Deref, DerefMut};

use crate::gc::Address;
use crate::object::{Obj, Ref};
use crate::threads::current_thread;

pub const HANDLE_SIZE: usize = 256;

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
        self.inner.lock().handle(obj)
    }

    pub fn push_border(&self) {
        self.inner.lock().push_border();
    }

    pub fn pop_border(&self) {
        self.inner.lock().pop_border();
    }

    pub fn iter(&self) -> HandleMemoryIter {
        let inner = self.inner.lock();
        let len = inner.buffers.len();
        let free = inner.free;

        HandleMemoryIter {
            mem: inner,
            buffer_idx: 0,
            element_idx: 0,
            full_buffer_len: if len == 0 { 0 } else { len - 1 },
            last_buffer_len: free,
        }
    }
}

pub struct HandleMemoryInner {
    /// all buffers, Box is important since HandleBuffer
    /// is a big struct that needs to get moved/copied on resizes
    buffers: Vec<Box<HandleBuffer>>,

    // store addresses of inserted borders
    borders: Vec<BorderData>,

    // index of next free position in buffer
    free: usize,
}

impl HandleMemoryInner {
    pub fn new() -> HandleMemoryInner {
        let buffer = box HandleBuffer::new();

        HandleMemoryInner {
            buffers: vec![buffer],
            borders: Vec::new(),
            free: 0,
        }
    }

    pub fn handle<T>(&mut self, obj: Ref<T>) -> Handle<T> {
        debug_assert!(current_thread().state_relaxed().is_running());

        if self.free >= HANDLE_SIZE {
            self.push_buffer();
            self.free = 0;
        }

        let buffer = self.buffers.last_mut().unwrap();

        let idx = self.free;
        let elem = &mut buffer.elements[idx];
        self.free = idx + 1;

        *elem = obj.cast::<Obj>();

        Handle(elem as *mut Ref<Obj> as *mut Ref<T>)
    }

    pub fn push_buffer(&mut self) {
        self.buffers.push(box HandleBuffer::new());
    }

    pub fn push_border(&mut self) {
        let buffer = self.buffers.len();
        let element = self.free;

        self.borders.push(BorderData { buffer, element });
    }

    pub fn pop_border(&mut self) {
        let border = self.borders.pop().expect("no border left");

        self.buffers.truncate(border.buffer);
        self.free = border.element;
    }
}

pub fn handle<T>(obj: Ref<T>) -> Handle<T> {
    let thread = current_thread();
    debug_assert!(thread.state_relaxed().is_running());
    thread.handles.handle(obj)
}

struct HandleBuffer {
    elements: [Ref<Obj>; HANDLE_SIZE],
}

impl HandleBuffer {
    fn new() -> HandleBuffer {
        HandleBuffer {
            elements: [Ref::null(); HANDLE_SIZE],
        }
    }
}

struct BorderData {
    buffer: usize,
    element: usize,
}
#[repr(C)]
pub struct Handle<T>(*mut Ref<T>);

impl<T> Handle<T> {
    pub fn direct(self) -> Ref<T> {
        unsafe { *self.0 }
    }

    pub fn direct_ptr(self) -> Address {
        unsafe { *self.0 }.address()
    }

    pub fn raw(self) -> *mut Ref<T> {
        self.0
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
        unsafe { &*self.0 }.deref()
    }
}

impl<T> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut T {
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
    buffer_idx: usize,
    element_idx: usize,
    full_buffer_len: usize,
    last_buffer_len: usize,
}

impl<'a> Iterator for HandleMemoryIter<'a> {
    type Item = Handle<Obj>;

    fn next(&mut self) -> Option<Handle<Obj>> {
        if self.buffer_idx < self.full_buffer_len {
            if self.element_idx < HANDLE_SIZE {
                let idx = self.element_idx;
                self.element_idx += 1;

                let buffer = &mut self.mem.buffers[self.buffer_idx];
                return Some(Handle(&mut buffer.elements[idx] as *mut Ref<Obj>));
            } else {
                self.buffer_idx += 1;
                self.element_idx = 0;
            }
        }

        if self.buffer_idx == self.full_buffer_len {
            if self.element_idx < self.last_buffer_len {
                let idx = self.element_idx;
                self.element_idx += 1;

                let buffer = &mut self.mem.buffers[self.buffer_idx];
                return Some(Handle(&mut buffer.elements[idx] as *mut Ref<Obj>));
            } else {
                return None;
            }
        }

        None
    }
}

pub fn handle_scope<F: FnOnce() -> R, R>(f: F) -> R {
    let thread = current_thread();
    thread.handles.push_border();
    let result = f();
    thread.handles.pop_border();
    result
}
