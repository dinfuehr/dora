#[repr(C)]
pub struct ucontext_t {
    _ignore: [u8; 48],
    pub uc_mcontext: *mut mcontext_t,
}

#[repr(C)]
pub struct mcontext_t {
    _ignore: [u8; 16],
    pub ss: [u64; 17],
}
