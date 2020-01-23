use std::sync::atomic::{compiler_fence, Ordering};

pub use self::param::*;
pub use self::reg::*;

pub mod asm;
pub mod param;
pub mod reg;

pub fn flush_icache(_: *const u8, _: usize) {
    // no flushing needed on x86_64, but emit compiler barrier
    compiler_fence(Ordering::SeqCst);
}
