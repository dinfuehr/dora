#[repr(C)]
pub struct ucontext_t {
    _ignore: [u64; 22],
    pub uc_mcontext: *mut mcontext_t,
}

#[repr(C)]
pub struct mcontext_t {
    _ignore: u64,
    pub regs: [u64; 31],
    pub sp: u64,
    pub pc: u64,
}
