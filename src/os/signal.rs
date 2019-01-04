use libc;
use std;

use baseline::map::CodeDescriptor;
use ctxt::{get_vm, VM};
use exception::stacktrace_from_es;
use os;
use os_cpu::*;
use safepoint;

#[cfg(target_family = "windows")]
use winapi::winnt::EXCEPTION_POINTERS;

#[cfg(target_family = "unix")]
pub fn register_signals() {
    unsafe {
        let mut sa: libc::sigaction = std::mem::uninitialized();

        sa.sa_sigaction = handler as usize;
        libc::sigemptyset(&mut sa.sa_mask as *mut libc::sigset_t);
        sa.sa_flags = libc::SA_SIGINFO;

        if libc::sigaction(
            libc::SIGSEGV,
            &sa as *const libc::sigaction,
            0 as *mut libc::sigaction,
        ) == -1
        {
            libc::perror("sigaction for SIGSEGV failed".as_ptr() as *const libc::c_char);
        }

        if libc::sigaction(
            libc::SIGILL,
            &sa as *const libc::sigaction,
            0 as *mut libc::sigaction,
        ) == -1
        {
            libc::perror("sigaction for SIGILL failed".as_ptr() as *const libc::c_char);
        }
    }
}

#[cfg(target_family = "windows")]
pub fn register_signals(vm: &VM) {
    use kernel32::AddVectoredExceptionHandler;

    unsafe {
        AddVectoredExceptionHandler(1, Some(handler));
    }
}

#[cfg(target_family = "windows")]
extern "system" fn handler(exception: *mut EXCEPTION_POINTERS) -> i32 {
    use winapi::excpt;

    if fault_handler(exception) {
        return excpt::ExceptionContinueExecution.0 as i32;
    }

    excpt::ExceptionContinueSearch.0 as i32
}

#[cfg(target_family = "windows")]
fn fault_handler(exception: *mut EXCEPTION_POINTERS) -> bool {
    unsafe {
        let record = (*exception).ExceptionRecord;
        let context = (*exception).ContextRecord;
    }

    false
}

#[cfg(target_family = "unix")]
fn handler(signo: libc::c_int, info: *const siginfo_t, ucontext: *const u8) {
    let es = read_execstate(ucontext);
    let vm = get_vm();

    let addr = unsafe { (*info).si_addr } as *const u8;

    if detect_nil_check(vm, es.pc, signo, addr) {
        println!("nil check failed");
        let stacktrace = stacktrace_from_es(vm, &es);
        stacktrace.dump(vm);
        unsafe {
            libc::_exit(104);
        }
    } else if detect_polling_page_check(vm, signo, addr) {
        // polling page read failed => enter safepoint
        safepoint::block(&es);

    // otherwise trap not dected => crash
    } else {
        println!(
            "error: trap not detected (signal {}, addr {:?}).",
            signo, addr
        );
        println!();
        println!("{:?}", &es);
        println!();
        println!("polling page = {}", vm.polling_page.addr());
        println!();

        let code_map = vm.code_map.lock();
        code_map.dump(vm);

        unsafe {
            libc::_exit(1);
        }
    }
}

fn detect_nil_check(vm: &VM, pc: usize, signo: libc::c_int, addr: *const u8) -> bool {
    if signo != libc::SIGSEGV || addr as usize >= os::page_size() as usize {
        return false;
    }

    let code_map = vm.code_map.lock();

    if let Some(CodeDescriptor::DoraFct(fid)) = code_map.get(pc.into()) {
        let jit_fct = vm.jit_fcts.idx(fid);
        let offset = pc - jit_fct.fct_ptr().to_usize();

        let jit_fct = jit_fct.to_base().expect("baseline expected");
        jit_fct.nil_check_for_offset(offset as i32)
    } else {
        false
    }
}

fn detect_polling_page_check(vm: &VM, signo: libc::c_int, addr: *const u8) -> bool {
    signo == libc::SIGSEGV && vm.polling_page.addr().to_ptr() == addr
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Trap {
    DIV0,
    ASSERT,
    INDEX_OUT_OF_BOUNDS,
    NIL,
    THROW,
    CAST,
    UNEXPECTED,
    OOM,
}

impl Trap {
    pub fn int(self) -> u32 {
        match self {
            Trap::DIV0 => 1,
            Trap::ASSERT => 2,
            Trap::INDEX_OUT_OF_BOUNDS => 3,
            Trap::NIL => 4,
            Trap::THROW => 5,
            Trap::CAST => 6,
            Trap::UNEXPECTED => 7,
            Trap::OOM => 8,
        }
    }

    pub fn from(value: u32) -> Option<Trap> {
        match value {
            1 => Some(Trap::DIV0),
            2 => Some(Trap::ASSERT),
            3 => Some(Trap::INDEX_OUT_OF_BOUNDS),
            4 => Some(Trap::NIL),
            5 => Some(Trap::THROW),
            6 => Some(Trap::CAST),
            7 => Some(Trap::UNEXPECTED),
            8 => Some(Trap::OOM),
            _ => None,
        }
    }
}

struct siginfo_t {
    pub si_signo: libc::c_int,
    pub si_errno: libc::c_int,
    pub si_code: libc::c_int,
    pub si_addr: *const libc::c_void,
}
