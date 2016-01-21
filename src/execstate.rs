use cpu::REG_COUNT;

pub struct ExecState {
    pc: usize,
    sp: usize,
    ra: usize,

    regs: [usize; REG_COUNT]
}
