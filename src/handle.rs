use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicUsize, Ordering};

use object::{Obj, Ref};
use threads::THREAD;

pub const HANDLE_SIZE: usize = 256;

pub struct HandleMemory {
    /// all buffers, Box is important since HandleBuffer
    /// is a big struct that needs to get moved/copied on resizes
    buffers: RefCell<Vec<Box<HandleBuffer>>>,

    // store addresses of inserted borders
    borders: RefCell<Vec<BorderData>>,

    // index of next free position in buffer
    free: AtomicUsize,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        let buffer = box HandleBuffer::new();

        HandleMemory {
            buffers: RefCell::new(vec![buffer]),
            borders: RefCell::new(Vec::new()),
            free: AtomicUsize::new(0),
        }
    }

    pub fn root<T>(&self, obj: Ref<T>) -> Handle<T> {
        if self.free.load(Ordering::Relaxed) >= HANDLE_SIZE {
            self.push_buffer();
            self.free.store(0, Ordering::Relaxed);
        }

        let mut buffers = self.buffers.borrow_mut();
        let buffer = buffers.last_mut().unwrap();

        let idx = self.free.load(Ordering::Relaxed);
        let elem = &mut buffer.elements[idx];
        self.free.store(idx + 1, Ordering::Relaxed);

        *elem = obj.cast::<Obj>();

        Handle(elem as *mut Ref<Obj> as *mut Ref<T>)
    }

    fn push_buffer(&self) {
        self.buffers.borrow_mut().push(box HandleBuffer::new());
    }

    pub fn push_border(&self) {
        let buffer = self.buffers.borrow().len();
        let element = self.free.load(Ordering::Relaxed);

        self.borders.borrow_mut().push(BorderData {
            buffer: buffer,
            element: element,
        });
    }

    pub fn pop_border(&self) {
        let border = self.borders.borrow_mut().pop().expect("no border left");

        self.buffers.borrow_mut().truncate(border.buffer);
        self.free.store(border.element, Ordering::Relaxed);
    }

    pub fn iter(&self) -> HandleMemoryIter {
        let len = self.buffers.borrow().len();

        HandleMemoryIter {
            mem: self,
            buffer_idx: 0,
            element_idx: 0,
            full_buffer_len: if len == 0 { 0 } else { len - 1 },
            last_buffer_len: self.free.load(Ordering::Relaxed),
        }
    }
}

pub fn root<T>(obj: Ref<T>) -> Handle<T> {
    THREAD.with(|thread| thread.borrow().handles.root(obj))
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

pub struct Handle<T>(*mut Ref<T>);

impl<T> Handle<T> {
    pub fn direct(self) -> Ref<T> {
        unsafe { *self.0 }
    }

    pub fn raw(self) -> *mut Ref<T> {
        self.0
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
    mem: &'a HandleMemory,
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

                let mut buffers = self.mem.buffers.borrow_mut();
                let buffer = &mut buffers[self.buffer_idx];
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

                let mut buffers = self.mem.buffers.borrow_mut();
                let buffer = &mut buffers[self.buffer_idx];
                return Some(Handle(&mut buffer.elements[idx] as *mut Ref<Obj>));
            } else {
                return None;
            }
        }

        None
    }
}
