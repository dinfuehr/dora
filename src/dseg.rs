use libc::c_void;

use mem::{self, Ptr};

pub struct DSeg {
    entries: Vec<Entry>,
    size: i32,
}

struct Entry {
    disp: i32,
    value: Ptr,
}

impl DSeg {
    pub fn new() -> DSeg {
        DSeg {
            entries: Vec::new(),
            size: 0
        }
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn finish(&self, ptr: *mut c_void) {
        for entry in &self.entries {
            let offset = self.size + entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);
                *(entry_ptr as *mut (*const c_void)) = entry.value.raw_ptr();
            }
        }
    }

    pub fn add_addr(&mut self, ptr: Ptr) -> i32 {
        self.size = mem::align_i32(self.size + mem::ptr_width(), mem::ptr_width());

        let entry = Entry { disp: self.size, value: ptr };
        self.entries.push(entry);

        self.size
    }
}
