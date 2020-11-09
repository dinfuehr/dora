pub use self::param::*;
pub use self::reg::*;

pub mod asm;
pub mod param;
pub mod reg;

#[cfg(target_os = "macos")]
pub fn flush_icache(start: *const u8, len: usize) {
    extern "C" {
        fn sys_icache_invalidate(data: *const u8, len: usize);
    }

    unsafe {
        sys_icache_invalidate(start, len);
    }
}

#[cfg(not(target_os = "macos"))]
pub fn flush_icache(start: *const u8, len: usize) {
    let start = start as usize;
    let end = start + len;

    let (icacheline_size, dcacheline_size) = cacheline_sizes();

    let istart = start & !(icacheline_size - 1);
    let dstart = start & !(dcacheline_size - 1);

    let mut ptr = dstart;

    while ptr < end {
        unsafe {
            llvm_asm!("dc civac, $0":: "r"(ptr) : "memory" : "volatile");
        }

        ptr += dcacheline_size;
    }

    unsafe {
        llvm_asm!("dsb ish" ::: "memory" : "volatile");
    }

    ptr = istart;

    while ptr < end {
        unsafe {
            llvm_asm!("ic ivau, $0":: "r"(ptr) : "memory" : "volatile");
        }

        ptr += icacheline_size;
    }

    unsafe {
        llvm_asm!("dsb ish
              isb" ::: "memory" : "volatile");
    }
}

#[cfg(not(target_os = "macos"))]
pub fn cacheline_sizes() -> (usize, usize) {
    let value: usize;

    unsafe {
        llvm_asm!("mrs $0, ctr_el0": "=r"(value)::: "volatile");
    }

    let insn = 4 << (value & 0xF);
    let data = 4 << ((value >> 16) & 0xF);

    (insn, data)
}

pub fn has_round() -> bool {
    true
}

pub fn has_popcnt() -> bool {
    true
}

pub fn has_lzcnt() -> bool {
    true
}

pub fn has_tzcnt() -> bool {
    true
}
