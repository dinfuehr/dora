#[repr(C)]
pub struct ucontext_t {
    _ignore: [u64; 5],
    pub uc_mcontext: mcontext_t,
}

#[repr(C)]
pub struct mcontext_t {
    pub regs: [usize; 23],
}
