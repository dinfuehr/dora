use std::fmt;
use cpu::REG_COUNT;

pub struct ExecState {
    pub pc: usize,
    pub sp: usize,
    pub ra: usize,

    pub regs: [usize; REG_COUNT]
}

impl fmt::Debug for ExecState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "ExecState {{"));
        try!(writeln!(f, "    pc = {:x}", self.pc));
        try!(writeln!(f, "    sp = {:x}", self.sp));
        try!(writeln!(f, "    ra = {:x}", self.ra));

        for (ind, &val) in self.regs.iter().enumerate() {
            try!(writeln!(f, "    regs[{}] = {:x}", ind, val));
        }

        write!(f, "}}")
    }
}
