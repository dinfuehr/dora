use crate::cpu::REG_COUNT;
use std::fmt;

pub struct ExecState {
    pub pc: usize,
    pub sp: usize,
    pub ra: usize,

    pub regs: [usize; REG_COUNT],
}

impl fmt::Debug for ExecState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        r#try!(writeln!(f, "ExecState {{"));
        r#try!(writeln!(f, "    pc = {:?}", self.pc as *const u8));
        r#try!(writeln!(f, "    sp = {:?}", self.sp as *const u8));
        r#try!(writeln!(f, "    ra = {:?}", self.ra as *const u8));

        for (ind, &val) in self.regs.iter().enumerate() {
            r#try!(writeln!(
                f,
                "    regs[{:2}] = {:-20?} {:-20}",
                ind, val as *const u8, val
            ));
        }

        write!(f, "}}")
    }
}
