extern crate libc;

use libc::*;
use ucontext::{ucontext_t, mcontext_t};

mod ucontext;

static REG_RIP: usize = 16;

type Fct = extern "C" fn() -> i32;

fn alloc_code(code: &[u8]) -> Fct {
    unsafe {
        let pagesize = sysconf(_SC_PAGESIZE) as usize;

        let ptr = mmap(0 as *mut c_void, pagesize, PROT_READ | PROT_WRITE | PROT_EXEC,
            MAP_ANON | MAP_PRIVATE, -1, 0);

        std::ptr::copy_nonoverlapping(code.as_ptr(), ptr as *mut u8, code.len());

        std::mem::transmute(ptr)
    }
}

fn handler(signo: c_int, _: *const siginfo_t, ucontext: *mut ucontext_t) {
    println!("signal {}!", signo);

    unsafe {
        let mcontext = &mut (*ucontext).uc_mcontext as *mut mcontext_t;

        let xpc = (*mcontext).gregs[REG_RIP];
        println!("xpc = {:x}", xpc);

        // mov eax, 4
        // ret
        let code = [ 0xb8, 4, 0, 0, 0, 0xc3 ];
        let fct = alloc_code(&code);

        (*mcontext).gregs[REG_RIP] = fct as i64;
    }
}

fn main() {
    unsafe {
        let mut sa: sigaction = std::mem::uninitialized();

        sa.sa_sigaction = handler as usize;
        sigemptyset(&mut sa.sa_mask as *mut sigset_t);
        sa.sa_flags = SA_SIGINFO;

        if sigaction(SIGSEGV, &sa as *const sigaction, 0 as *mut sigaction) == -1 {
            perror("sigaction failed".as_ptr() as *const i8);
        }

        // mov r10, [9]
        let code = [ 0x4C, 0x8B, 0x14, 0x25, 9, 0, 0, 0 ];
        let fct = alloc_code(&code);
        println!("address of failure code = {:}", fct as usize);

        let res = fct();
        println!("res = {}", res);
    }

    println!("after raising and handling SIGSEGV");
}

