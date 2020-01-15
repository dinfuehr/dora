use crate::mem;
use std;

#[derive(Debug)]
pub struct DSeg {
    entries: Vec<Entry>,
    size: i32,
}

#[derive(Debug)]
struct Entry {
    disp: i32,
    value: Value,
}

#[derive(Debug, PartialEq)]
enum Value {
    Ptr(*const u8),
    Float(f32),
    Double(f64),
    Int(i32),
}

impl Value {
    fn size(&self) -> i32 {
        match self {
            &Value::Ptr(_) => mem::ptr_width(),
            &Value::Float(_) => std::mem::size_of::<f32>() as i32,
            &Value::Double(_) => std::mem::size_of::<f64>() as i32,
            &Value::Int(_) => std::mem::size_of::<i32>() as i32,
        }
    }
}

impl DSeg {
    pub fn new() -> DSeg {
        DSeg {
            entries: Vec::new(),
            size: 0,
        }
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn finish(&self, ptr: *const u8) {
        for entry in &self.entries {
            let offset = self.size - entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);

                match entry.value {
                    Value::Ptr(v) => {
                        *(entry_ptr as *mut *const u8) = v;
                    }

                    Value::Float(v) => {
                        *(entry_ptr as *mut f32) = v;
                    }

                    Value::Double(v) => {
                        *(entry_ptr as *mut f64) = v;
                    }

                    Value::Int(v) => {
                        *(entry_ptr as *mut i32) = v;
                    }
                }
            }
        }
    }

    pub fn add_addr_reuse(&mut self, ptr: *const u8) -> i32 {
        for entry in &self.entries {
            if entry.value == Value::Ptr(ptr) {
                return entry.disp;
            }
        }

        self.add_addr(ptr)
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        self.add_value(Value::Ptr(ptr))
    }

    pub fn add_i32(&mut self, value: i32) -> i32 {
        self.add_value(Value::Int(value))
    }

    pub fn add_f32(&mut self, value: f32) -> i32 {
        self.add_value(Value::Float(value))
    }

    pub fn add_f64(&mut self, value: f64) -> i32 {
        self.add_value(Value::Double(value))
    }

    fn add_value(&mut self, value: Value) -> i32 {
        let size = value.size();
        self.size = mem::align_i32(self.size + size, size);

        let entry = Entry {
            disp: self.size,
            value,
        };

        self.entries.push(entry);

        self.size
    }

    pub fn align(&mut self, size: i32) -> i32 {
        assert!(size > 0);
        self.size = mem::align_i32(self.size, size);

        self.size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_addr() {
        let mut dseg = DSeg::new();
        assert_eq!(mem::ptr_width(), dseg.add_addr(1 as *const u8));
        assert_eq!(2 * mem::ptr_width(), dseg.add_addr(1 as *const u8));
    }

    #[test]
    fn test_add_addr_reuse() {
        let mut dseg = DSeg::new();
        assert_eq!(mem::ptr_width(), dseg.add_addr_reuse(1 as *const u8));
        assert_eq!(mem::ptr_width(), dseg.add_addr_reuse(1 as *const u8));
    }
}
