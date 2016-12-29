#[cfg(target_arch = "x86_64")]
#[cfg(target_os = "linux")]
pub use self::linux_x64::*;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os="linux")]
pub mod linux_x64;

#[cfg(target_arch = "aarch64")]
#[cfg(target_os = "linux")]
pub use self::linux_arm64::*;

#[cfg(target_arch = "aarch64")]
#[cfg(target_os="linux")]
pub mod linux_arm64;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os = "macos")]
pub use self::darwin_x64::*;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os="macos")]
pub mod darwin_x64;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os = "windows")]
pub use self::win_x64::*;

#[cfg(target_arch = "x86_64")]
#[cfg(target_os="windows")]
pub mod win_x64;
