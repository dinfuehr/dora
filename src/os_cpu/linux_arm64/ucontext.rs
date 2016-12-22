#[repr(C)]
pub struct ucontext_t {
    _ignore: [u64; 22],
    pub uc_mcontext: mcontext_t,
}

#[repr(C)]
pub struct mcontext_t {
    _ignore: u64,
    pub regs: [usize; 31],
    pub sp: usize,
    pub pc: usize,
}
