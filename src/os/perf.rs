use ctxt::Context;
use interner::Name;
use jit::fct::JitFct;

#[cfg(target_os = "linux")]
pub fn register_with_perf(jit_fct: &JitFct, ctxt: &Context, name: Name) {
    use libc;
    use std::fs::OpenOptions;
    use std::io::prelude::*;

    let pid = unsafe { libc::getpid() };
    let fname = format!("/tmp/perf-{}.map", pid);

    let mut options = OpenOptions::new();
    let mut file = options.create(true).append(true).open(&fname).unwrap();

    let code_start = jit_fct.ptr_start().raw() as usize;
    let code_end = jit_fct.ptr_end().raw() as usize;
    let name = ctxt.interner.str(name);

    let line = format!("{:x} {:x} dora::{}\n",
        code_start, code_end - code_start, name);
    file.write_all(line.as_bytes()).unwrap();
}

#[cfg(not(target_os = "linux"))]
pub fn register_with_perf(_: &JitFct, _: &Context, _: Name) {
    // nothing to do
}
