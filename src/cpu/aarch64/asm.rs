use jit::buffer::Buffer;

pub fn nop(buf: &mut Buffer) {
    buf.emit_u32(0);
}

// pub fn add(rd: Reg, rn: Reg, imm: i32) {
//     assert!(fits_i12(imm));
// }

// fn class_addsub(rd: i32, rn: i32, imm: i32) -> u32 {
//     assert!(fits_i5(rd));
//     assert!(fits_i5(rn));
//     assert!(fits_i12(imm));
// }

fn fits_u5(imm: i32) -> bool {
    0 <= imm && imm < 32
}

fn fits_i12(imm: i32) -> bool {
    -2048 <= imm && imm < 2048
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fits_u5() {
        assert!(fits_u5(0));
        assert!(fits_u5(31));
        assert!(!fits_u5(32));
        assert!(!fits_u5(-1));
    }

    #[test]
    fn test_fits_i12() {
        assert!(fits_i12(0));
        assert!(fits_i12(-2048));
        assert!(fits_i12(2047));
        assert!(!fits_i12(-2049));
        assert!(!fits_i12(2048));
    }
}