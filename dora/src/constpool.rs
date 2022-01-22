use std;

use crate::gc::Address;
use crate::mem;

#[derive(Debug)]
pub struct ConstPool {
    entries: Vec<ConstPoolEntry>,
    size: i32,
}

#[derive(Debug)]
struct ConstPoolEntry {
    disp: i32,
    value: ConstPoolValue,
}

#[derive(Debug, PartialEq)]
enum ConstPoolValue {
    Ptr(Address),
    Float32(f32),
    Float64(f64),
    Int32(i32),
}

impl ConstPoolValue {
    fn size(&self) -> i32 {
        match self {
            &ConstPoolValue::Ptr(_) => mem::ptr_width(),
            &ConstPoolValue::Float32(_) => std::mem::size_of::<f32>() as i32,
            &ConstPoolValue::Float64(_) => std::mem::size_of::<f64>() as i32,
            &ConstPoolValue::Int32(_) => std::mem::size_of::<i32>() as i32,
        }
    }
}

impl ConstPool {
    pub fn new() -> ConstPool {
        ConstPool {
            entries: Vec::new(),
            size: 0,
        }
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn install(&self, ptr: *const u8) {
        for entry in &self.entries {
            let offset = self.size - entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);

                match entry.value {
                    ConstPoolValue::Ptr(v) => {
                        *(entry_ptr as *mut Address) = v;
                    }

                    ConstPoolValue::Float32(v) => {
                        *(entry_ptr as *mut f32) = v;
                    }

                    ConstPoolValue::Float64(v) => {
                        *(entry_ptr as *mut f64) = v;
                    }

                    ConstPoolValue::Int32(v) => {
                        *(entry_ptr as *mut i32) = v;
                    }
                }
            }
        }
    }

    pub fn add_addr_reuse(&mut self, ptr: Address) -> i32 {
        for entry in &self.entries {
            if entry.value == ConstPoolValue::Ptr(ptr) {
                return entry.disp;
            }
        }

        self.add_addr(ptr)
    }

    pub fn add_addr(&mut self, ptr: Address) -> i32 {
        self.add_value(ConstPoolValue::Ptr(ptr))
    }

    pub fn add_f32(&mut self, value: f32) -> i32 {
        self.add_value(ConstPoolValue::Float32(value))
    }

    pub fn add_f64(&mut self, value: f64) -> i32 {
        self.add_value(ConstPoolValue::Float64(value))
    }

    pub fn add_i32(&mut self, value: i32) -> i32 {
        self.add_value(ConstPoolValue::Int32(value))
    }

    fn add_value(&mut self, value: ConstPoolValue) -> i32 {
        let size = value.size();
        self.size = mem::align_i32(self.size + size, size);

        let entry = ConstPoolEntry {
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
        let mut constpool = ConstPool::new();
        assert_eq!(mem::ptr_width(), constpool.add_addr(1.into()));
        assert_eq!(2 * mem::ptr_width(), constpool.add_addr(1.into()));
    }

    #[test]
    fn test_add_addr_reuse() {
        let mut constpool = ConstPool::new();
        assert_eq!(mem::ptr_width(), constpool.add_addr_reuse(1.into()));
        assert_eq!(mem::ptr_width(), constpool.add_addr_reuse(1.into()));
    }
}
