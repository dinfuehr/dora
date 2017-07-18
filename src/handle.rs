use std::cell::{Cell, RefCell};

use object::{Handle, Obj};

pub const HANDLE_SIZE: usize = 256;

pub struct HandleMemory {
    /// all buffers, Box is important since HandleBuffer
    /// is a big struct that needs to get moved/copied on resizes
    buffers: RefCell<Vec<Box<HandleBuffer>>>,

    // store addresses of inserted borders
    borders: RefCell<Vec<BorderData>>,

    // index of next free position in buffer
    free: Cell<usize>,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        let buffer = box HandleBuffer::new();

        HandleMemory {
            buffers: RefCell::new(vec![buffer]),
            borders: RefCell::new(Vec::new()),
            free: Cell::new(0),
        }
    }

    pub fn root(&self, obj: Handle<Obj>) -> Rooted<Obj> {
        if self.free.get() >= HANDLE_SIZE {
            self.push_buffer();
            self.free.set(0);
        }

        let mut buffers = self.buffers.borrow_mut();
        let buffer = buffers.last_mut().unwrap();
        let elem = &mut buffer.elements[self.free.get()];

        *elem = obj;

        Rooted(elem)
    }

    fn push_buffer(&self) {
        self.buffers.borrow_mut().push(box HandleBuffer::new());
    }

    pub fn push_border(&self) {
        let buffer = self.buffers.borrow().len();
        let element = self.free.get();

        self.borders.borrow_mut().push(BorderData {
            buffer: buffer,
            element: element,
        });
    }

    pub fn pop_border(&self) {
        let border = self.borders.borrow_mut().pop().expect("no border left");

        self.buffers.borrow_mut().truncate(border.buffer);
        self.free.set(border.element);
    }
}

struct HandleBuffer {
    elements: [Handle<Obj>; HANDLE_SIZE],
}

impl HandleBuffer {
    fn new() -> HandleBuffer {
        HandleBuffer { elements: [Handle::null(); HANDLE_SIZE] }
    }
}

struct BorderData {
    buffer: usize,
    element: usize,
}

#[derive(Copy, Clone)]
pub struct Rooted<T>(*mut Handle<T>);

impl<T> Rooted<T> {
    pub fn direct(self) -> Handle<T> {
        unsafe { *self.0 }
    }
}
