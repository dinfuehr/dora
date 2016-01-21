#[cfg(target_arch = "x86_64")]
#[cfg(target_os = "linux")]
pub use self::linux_x64::*;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os="linux")]
pub mod linux_x64;
