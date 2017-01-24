use std::ptr;

use gc::root::IndirectObj;
use object::Obj;

pub const HANDLE_SIZE: usize = 256;

pub struct HandleMemory {
    /// all buffers, Box is important since HandleBuffer
    /// is a big struct that needs to get moved/copied on resizes
    buffers: Vec<Box<HandleBuffer>>,
    borders: Vec<BorderData>,

    // next free position in buffer
    free: usize,
}

impl HandleMemory {
    pub fn new() -> HandleMemory {
        let buffer = box HandleBuffer::new();

        HandleMemory {
            buffers: vec![buffer],
            borders: Vec::new(),
            free: 0,
        }
    }

    pub fn push(&mut self, obj: *mut Obj) -> IndirectObj {
        if self.free >= HANDLE_SIZE {
            self.push_buffer();
            self.free = 0;
        }

        let buffer = self.buffers.last_mut().unwrap();
        let elem = &mut buffer.elements[self.free];

        *elem = obj;

        (elem as *mut _ as usize).into()
    }

    fn push_buffer(&mut self) {
        self.buffers.push(box HandleBuffer::new());
    }
}

struct HandleBuffer {
    elements: [*mut Obj; HANDLE_SIZE],
}

impl HandleBuffer {
    fn new() -> HandleBuffer {
        HandleBuffer {
            elements: [ptr::null_mut(); HANDLE_SIZE],
        }
    }
}

struct BorderData {
    buffer: usize,
    element: usize,
}

struct HandleBorder(usize);
