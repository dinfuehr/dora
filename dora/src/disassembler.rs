#[cfg(feature = "default")]
pub use self::capstone::{disassemble, supported};

#[cfg(not(feature = "default"))]
pub use self::none::{disassemble, supported};

#[cfg(feature = "default")]
mod capstone;

#[cfg(not(feature = "default"))]
mod none;
