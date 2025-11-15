#[cfg(feature = "capstone")]
pub use self::capstone::{disassemble, supported};

#[cfg(not(feature = "capstone"))]
pub use self::none::{disassemble, supported};

#[cfg(feature = "capstone")]
mod capstone;

#[cfg(not(feature = "capstone"))]
mod none;
