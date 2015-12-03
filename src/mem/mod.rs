pub mod code;

pub fn align(value: u32, align: u32) -> u32 {
    ((value + align - 1) / align) * align
}

pub fn align_usize(value: usize, align: usize) -> usize {
    ((value + align - 1) / align) * align
}
