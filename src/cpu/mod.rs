#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "arm")]
pub use self::arm::*;

#[cfg(target_arch = "arm")]
pub mod arm;
