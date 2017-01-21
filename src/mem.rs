use std::i32;
use std::mem::size_of;

pub fn ptr_width() -> i32 {
    size_of::<*const u8>() as i32
}

pub fn align(value: u32, align: u32) -> u32 {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

pub fn align_i32(value: i32, align: i32) -> i32 {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

pub fn align_usize(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

pub fn fits_u8(value: i64) -> bool {
    0 <= value && value <= 255
}

pub fn fits_i32(value: i64) -> bool {
    i32::MIN as i64 <= value && value <= i32::MAX as i64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fits_u8() {
        assert_eq!(true, fits_u8(0));
        assert_eq!(true, fits_u8(255));
        assert_eq!(false, fits_u8(256));
        assert_eq!(false, fits_u8(-1));
    }

    #[test]
    fn test_fits_i32() {
        assert_eq!(true, fits_i32(0));
        assert_eq!(true, fits_i32(i32::MAX as i64));
        assert_eq!(true, fits_i32(i32::MIN as i64));
        assert_eq!(false, fits_i32(i32::MAX as i64 + 1));
        assert_eq!(false, fits_i32(i32::MIN as i64 - 1));
    }
}