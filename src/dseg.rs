use mem;

#[derive(Debug)]
pub struct DSeg {
    entries: Vec<Entry>,
    size: i32,
}

#[derive(Debug)]
struct Entry {
    disp: i32,
    value: *const u8,
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

    pub fn finish(&self, ptr: *mut u8) {
        for entry in &self.entries {
            let offset = self.size - entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);
                *(entry_ptr as *mut (*const u8)) = entry.value;
            }
        }
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        for entry in &self.entries {
            if entry.value == ptr {
                return entry.disp;
            }
        }

        self.size = mem::align_i32(self.size + mem::ptr_width(), mem::ptr_width());

        let entry = Entry { disp: self.size, value: ptr };
        self.entries.push(entry);

        self.size
    }
}
