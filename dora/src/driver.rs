pub use self::start::start;

use std::path::PathBuf;

mod build;
mod compile;
pub mod flags;
mod init;
pub mod start;

fn append_exe_suffix(mut path: PathBuf) -> PathBuf {
    let exe_suffix = std::env::consts::EXE_SUFFIX;
    if !exe_suffix.is_empty() && path.extension().is_none() {
        let extension = exe_suffix.strip_prefix('.').unwrap_or(exe_suffix);
        path.set_extension(extension);
    }
    path
}
