use cpu::REG_COUNT;

pub struct ExecState {
    pub pc: usize,
    pub sp: usize,
    pub ra: usize,

    pub regs: [usize; REG_COUNT]
}
