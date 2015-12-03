use libc::c_void;
use std::mem::size_of;

use mem;

pub struct DSeg {
    entries: Vec<Entry>,
    size: u32,
}

struct Entry {
    disp: i32,
    value: *const c_void,
}

impl DSeg {
    pub fn new() -> DSeg {
        DSeg {
            entries: Vec::new(),
            size: 0
        }
    }

    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn finish(&self, ptr: *mut c_void) {
        for entry in &self.entries {
            let offset = self.size as i32 + entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);
                *(entry_ptr as *mut (*const c_void)) = entry.value;
            }
        }
    }

    pub fn add_addr(&mut self, ptr: *const c_void) -> i32 {
        let ptr_width = size_of::<*const c_void>() as u32;
        self.size = mem::align(self.size + ptr_width, ptr_width);

        let disp = -(self.size as i32);

        let entry = Entry { disp: disp, value: ptr };
        self.entries.push(entry);

        disp
    }
}
