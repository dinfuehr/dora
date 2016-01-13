pub use self::param::*;
pub use self::reg::*;

pub mod emit;

// TODO: remove `pub` when possible
pub mod instr;

pub mod param;
pub mod reg;
pub mod trap;
