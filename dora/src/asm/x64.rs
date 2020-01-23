use crate::asm::Assembler;

impl Assembler {
    pub fn ret(&mut self) {
        self.emit_u8(0xC3);
    }

    pub fn nop(&mut self) {
        self.emit_u8(0x90);
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::Assembler;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = Assembler::new();
            buf.$name();
            let expected = vec![$($expr,)*];

            assert_eq!(expected, buf.buffer());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            )
        ) => {{
            let mut buf = Assembler::new();
            buf.$name($($param,)*);
            let expected = vec![$($expr,)*];
            let data = buf.buffer();

            if expected != data {
                print!("exp: ");

                for (ind, val) in expected.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                print!("\ngot: ");

                for (ind, val) in data.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                println!("");

                panic!("emitted code wrong.");
            }
        }};
    }

    #[test]
    fn test_ret() {
        assert_emit!(0xc3; ret);
    }

    #[test]
    fn test_nop() {
        assert_emit!(0x90; nop);
    }
}
